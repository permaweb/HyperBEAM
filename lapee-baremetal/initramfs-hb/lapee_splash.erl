%% -*- erlang -*-
%%
%% lapee_splash -- LapEE 3D animated boot splash.
%%
%% Runs as a SEPARATE BEAM VM forked from init, parallel to the
%% HyperBEAM node. Same erts binary (lives in HyperBEAM's release
%% under /usr/lib/hyperbeam/erts-*/bin/erl) but its own VM, its own
%% scheduler, its own crash domain. If HB falls over the splash
%% keeps drawing; if the splash falls over HB doesn't notice.
%%
%% Compiled to a .beam at build time by build-initramfs-hb.sh and
%% loaded at runtime via:
%%
%%   erl -pa /usr/local/lib/lapee-splash -noshell -run lapee_splash main
%%
%% What it does:
%%   1. 3D wireframe of a clamshell laptop, rotated around Y axis,
%%      lid easing open from closed -> ~106 deg, ~12 fps.
%%      Bresenham-rasterised onto a fixed 80x24 char grid centred
%%      on the screen.
%%   2. State machine polled internally each frame:
%%        boot     -- before /run/lapee/primary-net exists
%%        net-up   -- primary-net exists, IP known
%%        hb-wait  -- IP known, HB /info not yet responding
%%        qr       -- HB /info returns 200; lock face-on, overlay QR
%%      No `lapee-splash set' from init needed -- the splash decides.
%%   3. At qr phase: `qrencode -t ASCII -o - http://<ip>:8734/'
%%      generates the QR; we overlay it on the laptop's projected
%%      screen panel and stop the animation there.

-module(lapee_splash).
-export([main/0, main/1]).

main()      -> main([]).

%% ============================================================
%% Constants
%% ============================================================
-define(FPS, 12).
-define(SLEEP_MS, 83).               %% ~1000/FPS
-define(POLL_TIMEOUT_MS, 500).
-define(MIN_W, 80).
-define(MIN_H, 24).

%% Paths are overridable via env vars so the same escript can be
%% exercised from a Mac dev box (LAPEE_CONSOLE=/tmp/out, etc.) and
%% from the iron initramfs (defaults below).
console_path()    -> os:getenv("LAPEE_CONSOLE",    "/dev/console").
primary_net_path() -> os:getenv("LAPEE_PRIMARY_NET", "/run/lapee/primary-net").
%% Probe the dev_tpm2 device's `/info' rather than `/~meta@1.0/info'
%% so we're checking the SAME endpoint init's writeback loop checks
%% (init line 722). If init's wait succeeds, splash's wait will too --
%% no chance of one signalling green while the other still waits.
%%
%% We deliberately keep the host:port and the path SEPARATE: the probe
%% is a raw gen_tcp speak (see hb_ready/0) so we never depend on
%% inets/httpc URL parsing -- which threw `function_clause' under the
%% guest's OTP 27 build for paths containing both `~' and `@', burning
%% one full overnight boot cycle to diagnose. Captured in splash.log
%% on real hardware before the rewrite.
probe_host()       -> os:getenv("LAPEE_PROBE_HOST", "127.0.0.1").
probe_port()       -> list_to_integer(os:getenv("LAPEE_PROBE_PORT", "8734")).
probe_path()       -> os:getenv("LAPEE_PROBE_PATH", "/~tpm2@2.0a/info").
log_path()         -> os:getenv("LAPEE_SPLASH_LOG", "/run/lapee/splash.log").

%% Terminal dimensions detected at startup via `stty size'. On the
%% iron framebuffer console with -vga std + 8x16 font that's
%% typically 128x48, not 80x24. Hard-coding 80x24 leaves the splash
%% in the upper-left corner of a wider screen.
detect_dims() ->
    Cmd = io_lib:format("stty -F ~s size 2>/dev/null", [console_path()]),
    Out = string:trim(os:cmd(lists:flatten(Cmd))),
    case string:tokens(Out, " ") of
        [RowsStr, ColsStr] ->
            try
                Rows = list_to_integer(RowsStr),
                Cols = list_to_integer(ColsStr),
                {max(?MIN_W, Cols), max(?MIN_H, Rows)}
            catch _:_ -> {?MIN_W, ?MIN_H}
            end;
        _ -> {?MIN_W, ?MIN_H}
    end.

%% ============================================================
%% Entry point
%% ============================================================
main(_Args) ->
    %% Splash uses raw gen_tcp for the readiness probe, so no
    %% inets/httpc start-up is needed. Log a startup line so the
    %% writeback splash.log shows the daemon survived its first
    %% sleep loop.
    log_start(),

    %% Detect actual terminal dimensions. On QEMU+OVMF with -vga std
    %% the framebuffer console is typically 128x48; on real iron
    %% it depends on the EFI framebuffer mode. Hard-coding 80x24
    %% would pin the splash to the upper-left of a wider screen.
    {Cols, Rows} = detect_dims(),
    log_event(io_lib:format("dims: ~bx~b", [Cols, Rows])),

    %% Open /dev/console raw. On the iron framebuffer console this
    %% is a character device -- writes are atomic, ANSI escapes
    %% interpreted in-kernel by fbcon.
    {ok, Out} = file:open(console_path(), [write, raw]),

    %% Hide cursor, clear screen, home.
    file:write(Out, <<"\e[?25l\e[2J\e[H">>),

    %% Wall-clock start so hb-wait phase can show elapsed seconds.
    %% Operators staring at "starting HyperBEAM..." for 60-180s under
    %% TCG-emulated amd64 need to see the counter advance, otherwise
    %% they reasonably conclude the boot is hung. erlang:monotonic_time
    %% is unaffected by NTP clock jumps.
    T0 = erlang:monotonic_time(millisecond),

    State0 = #{
        out         => Out,
        cols        => Cols,
        rows        => Rows,
        frame       => 0,
        yaw         => 0.0,
        lid         => 0.0,
        lid_target  => 1.85,
        phase       => boot,
        ip          => undefined,
        qr_lines    => undefined,
        t0_ms       => T0,
        hb_wait_t0  => undefined
    },
    log_event("phase=boot"),
    %% Trap ctrl-c / sigterm so we can restore the cursor on exit.
    process_flag(trap_exit, true),
    try
        loop(State0)
    after
        file:write(Out, <<"\e[?25h\n">>),
        file:close(Out)
    end.

%% ============================================================
%% Main loop
%% ============================================================
loop(S0) ->
    S1 = poll_state(S0),
    Frame = render(S1),
    file:write(maps:get(out, S1), Frame),
    timer:sleep(?SLEEP_MS),
    S2 = step_anim(S1),
    loop(S2).

%% ============================================================
%% State polling -- phase machine, IP discovery, HB probe
%% ============================================================
poll_state(S = #{phase := Phase, ip := Ip}) ->
    case Phase of
        boot ->
            case read_ip() of
                undefined -> S;
                NewIp     ->
                    log_event(io_lib:format("phase=net-up ip=~s", [NewIp])),
                    S#{phase => 'net-up', ip => NewIp}
            end;
        'net-up' ->
            case hb_ready() of
                true ->
                    log_event("phase=qr (HB ready on first poll)"),
                    Qr = generate_qr_lines(Ip),
                    S#{phase => qr, qr_lines => Qr, lid_target => 1.60};
                {false, Reason} ->
                    log_event(io_lib:format(
                        "phase=hb-wait (~s)", [Reason])),
                    HbT0 = erlang:monotonic_time(millisecond),
                    S#{phase => 'hb-wait', hb_wait_t0 => HbT0}
            end;
        'hb-wait' ->
            case hb_ready() of
                true ->
                    log_event("phase=qr (HB ready)"),
                    Qr = generate_qr_lines(Ip),
                    S#{phase => qr, qr_lines => Qr, lid_target => 1.60};
                {false, _Reason} ->
                    %% Don't spam the log -- only every ~30 polls
                    %% (~2.5 s wall) to keep splash.log readable.
                    Frame = maps:get(frame, S),
                    case Frame rem 60 of
                        0 ->
                            HbT0 = maps:get(hb_wait_t0, S),
                            Now = erlang:monotonic_time(millisecond),
                            log_event(io_lib:format(
                                "hb-wait: ~bs elapsed",
                                [(Now - HbT0) div 1000]));
                        _ -> ok
                    end,
                    S
            end;
        qr ->
            S
    end.

read_ip() ->
    case file:read_file(primary_net_path()) of
        {ok, Bin} ->
            case re:run(Bin, "(?m)^ip=([0-9.]+)",
                        [{capture, all_but_first, list}]) of
                {match, [Ip]} -> Ip;
                _             -> undefined
            end;
        _ -> undefined
    end.

%% Returns `true' when /info answered with HTTP 200, or
%% `{false, Reason}' otherwise. The Reason is a short human-readable
%% string suitable for splash.log -- not for screen.
%%
%% Implementation note: this used to call httpc:request/4, but the
%% guest's OTP 27 inets threw `error:function_clause' on URLs whose
%% path contained both `~' and `@' (e.g. `/~tpm2@2.0a/info'), causing
%% every poll to be caught and converted to "{false, function_clause}"
%% so the splash sat in hb-wait forever even after HB was answering.
%% The fix is to skip httpc and speak HTTP/1.0 over a raw gen_tcp
%% connection -- no URL parsing, no header validation, no ssl path,
%% nothing that can throw on a tilde. The status-line check still
%% needs the body to start with "HTTP/1.x 200" so we read just enough
%% bytes to see that prefix and bail.
hb_ready() ->
    Host = probe_host(),
    Port = probe_port(),
    Path = probe_path(),
    Tmo  = ?POLL_TIMEOUT_MS,
    case gen_tcp:connect(Host, Port,
                         [binary, {active, false},
                          {packet, raw}, {nodelay, true}],
                         Tmo) of
        {ok, Sock} ->
            try
                Req = io_lib:format(
                        "GET ~s HTTP/1.0\r\nHost: ~s:~b\r\n"
                        "Connection: close\r\n\r\n",
                        [Path, Host, Port]),
                case gen_tcp:send(Sock, Req) of
                    ok ->
                        case gen_tcp:recv(Sock, 0, Tmo) of
                            {ok, <<"HTTP/1.", _, " 200", _/binary>>} ->
                                true;
                            {ok, <<"HTTP/1.", _, " ", C1, C2, C3,
                                   _/binary>>} ->
                                {false, io_lib:format(
                                          "HTTP ~c~c~c", [C1,C2,C3])};
                            {ok, Other} ->
                                {false, io_lib:format(
                                          "unparsed ~P",
                                          [Other, 8])};
                            {error, Reason} ->
                                {false, io_lib:format(
                                          "recv ~p", [Reason])}
                        end;
                    {error, Reason} ->
                        {false, io_lib:format("send ~p", [Reason])}
                end
            after
                gen_tcp:close(Sock)
            end;
        {error, Reason} ->
            {false, io_lib:format("conn ~p", [Reason])}
    end.

generate_qr_lines(undefined) -> undefined;
generate_qr_lines(Ip) ->
    Url = lists:flatten(io_lib:format("http://~s:8734/", [Ip])),
    %% qrencode is shipped in the initramfs at /usr/bin/qrencode.
    %% `-m 0' = zero margin (we're tight on cells); `-t ASCIIi'
    %% = inverted 1-cell-per-module ASCII (denser than ASCII).
    Cmd = lists:flatten(
              io_lib:format("qrencode -m 0 -t ASCIIi -o - '~s' 2>/dev/null",
                            [Url])),
    Out = os:cmd(Cmd),
    case Out of
        ""  -> undefined;
        _   -> string:split(Out, "\n", all)
    end.

%% ============================================================
%% Animation state advance
%% ============================================================
step_anim(S = #{frame := F, yaw := Y, lid := L, lid_target := T, phase := P}) ->
    F1 = F + 1,
    Y1 = case P of
        qr -> 0.0;          %% lock face-on at qr
        _  -> Y + 0.05      %% slow orbit during boot/wait
    end,
    %% Ease lid toward target with a 15% step per frame -- smooth open
    L1 = L + (T - L) * 0.15,
    S#{frame => F1, yaw => Y1, lid => L1}.

%% ============================================================
%% 3D model + projection
%% ============================================================
%% Laptop in laptop-width units. +x right, +y up, +z forward.
%% Origin at hinge midpoint (back-top edge of base).
%% Base: 4.0 wide, 3.0 deep, 0.22 tall. Hinge at z=-1.5, y=0.
%% Lid:  4.0 wide, 2.5 tall. Rotates around the hinge edge.
%% lid_angle: 0 = closed flat on base; pi/2 = upright.
laptop_edges(LidAngle) ->
    Base = base_edges(),
    Lid  = lid_edges(LidAngle),
    Base ++ Lid.

base_edges() ->
    %% Just the 4 top edges of the base + the 4 bottom edges +
    %% the 4 vertical corner edges -- a 12-edge wireframe gets too
    %% busy at our resolution. Drop to 6: top rectangle + the two
    %% front-facing corners only, which reads as "thin slab" cleanly.
    Pt = [{-2.0, 0.00, -1.5}, {2.0, 0.00, -1.5},   %% back-top
          {-2.0, 0.00,  1.5}, {2.0, 0.00,  1.5}],  %% front-top
    Pb = [{-2.0,-0.22,  1.5}, {2.0,-0.22,  1.5}],  %% front-bottom
    %% Top rectangle (4 edges).
    Top = [{nth(1, Pt), nth(2, Pt)},
           {nth(3, Pt), nth(4, Pt)},
           {nth(1, Pt), nth(3, Pt)},
           {nth(2, Pt), nth(4, Pt)}],
    %% Front-bottom rectangle hint: front-top to front-bottom on
    %% each side, plus the front-bottom edge.
    Front = [{nth(3, Pt), nth(1, Pb)},
             {nth(4, Pt), nth(2, Pb)},
             {nth(1, Pb), nth(2, Pb)}],
    Top ++ Front.

lid_edges(A) ->
    %% Lid corners in local lid coords. Hinge is at origin (back-top
    %% edge of base). Closed lid sits FLAT ON TOP of the base, so the
    %% top edge starts at z=+LH (forward), y=0. Opening rotates the
    %% top edge UP and back toward the hinge.
    %%   A=0      -> closed flat (top at +z)
    %%   A=pi/2   -> upright open (top at +y)
    %%   A=1.85   -> ~106 deg, classic working angle (slight back-tilt)
    LH = 2.5,
    Local = [{-2.0, 0.0, 0.0},   %% 1: bottom-left at hinge
             { 2.0, 0.0, 0.0},   %% 2: bottom-right at hinge
             {-2.0, 0.0,  LH},   %% 3: top-left, lid closed
             { 2.0, 0.0,  LH}],  %% 4: top-right, lid closed
    Rot = [rotate_lid(P, A) || P <- Local],
    %% Translate so the hinge sits at z=-1.5, y=0 in world.
    World = [{X, Y, Z + (-1.5)} || {X, Y, Z} <- Rot],
    Idx = [{1,2},{3,4},{1,3},{2,4}],
    [{nth(I, World), nth(J, World)} || {I, J} <- Idx].

%% Rotation that takes the closed lid (top at +z) up to open (top at
%% +y) as A goes from 0 -> pi/2.
rotate_lid({X, Y, Z}, A) ->
    Ca = math:cos(A), Sa = math:sin(A),
    {X, Y * Ca + Z * Sa, -Y * Sa + Z * Ca}.

rotate_y({X, Y, Z}, A) ->
    Ca = math:cos(A), Sa = math:sin(A),
    {X * Ca + Z * Sa, Y, -X * Sa + Z * Ca}.

%% Project a 3D point to a 2D grid cell.
%% Simple orthographic projection with a Y-axis tilt for "3/4 view".
project({X, Y, Z}, W, H) ->
    Tilt = 0.45,                                  %% radians, look-down
    Yt = Y * math:cos(Tilt) - Z * math:sin(Tilt),
    Scale = 9.0,                                  %% chars per laptop-width
    %% Char cells are roughly 2:1 tall:wide; scale Y by half.
    %% Lift the laptop slightly above the vertical centre so the
    %% status line below has space without overlapping the base.
    Cx = W / 2.0 + X * Scale,
    Cy = H / 2.0 - Yt * Scale * 0.5 - 2.0,
    {round(Cx), round(Cy)}.

%% ============================================================
%% Bresenham line draw onto the grid
%% ============================================================
%% Grid is map: {Row, Col} => char.
draw_line(Grid, W, H, P1, P2) ->
    {X1, Y1} = P1, {X2, Y2} = P2,
    Ch = pick_char(X1, Y1, X2, Y2),
    bres(Grid, W, H, X1, Y1, X2, Y2, Ch).

pick_char(X1, Y1, X2, Y2) ->
    Dx = abs(X2 - X1), Dy = abs(Y2 - Y1),
    if
        Dy * 2 < Dx -> $-;
        Dx * 2 < Dy -> $|;
        (X2 - X1) * (Y2 - Y1) > 0 -> $\\;
        true -> $/
    end.

bres(Grid, W, H, X0, Y0, X1, Y1, Ch) ->
    Dx = abs(X1 - X0), Sx = if X0 < X1 -> 1; true -> -1 end,
    Dy = -abs(Y1 - Y0), Sy = if Y0 < Y1 -> 1; true -> -1 end,
    Err = Dx + Dy,
    bres_step(Grid, W, H, X0, Y0, X1, Y1, Dx, Dy, Sx, Sy, Err, Ch).

bres_step(Grid, W, H, X, Y, X1, Y1, _, _, _, _, _, _) when X =:= X1, Y =:= Y1 ->
    plot(Grid, W, H, X, Y, $+);
bres_step(Grid, W, H, X, Y, X1, Y1, Dx, Dy, Sx, Sy, Err, Ch) ->
    G1 = plot(Grid, W, H, X, Y, Ch),
    E2 = 2 * Err,
    {X2, Err1a} =
        if E2 >= Dy -> {X + Sx, Err + Dy};
           true     -> {X, Err}
        end,
    {Y2, Err1} =
        if E2 =< Dx -> {Y + Sy, Err1a + Dx};
           true     -> {Y, Err1a}
        end,
    bres_step(G1, W, H, X2, Y2, X1, Y1, Dx, Dy, Sx, Sy, Err1, Ch).

plot(Grid, W, H, X, Y, Ch) ->
    case X >= 1 andalso X =< W andalso Y >= 1 andalso Y =< H of
        true  -> Grid#{{Y, X} => Ch};
        false -> Grid
    end.

%% ============================================================
%% Frame composition + ANSI emission
%% ============================================================
render(S = #{cols := W, rows := H, yaw := Yaw, lid := Lid,
             phase := Phase, ip := Ip, qr_lines := Qr,
             hb_wait_t0 := HbT0}) ->
    Edges = laptop_edges(Lid),
    %% Apply yaw rotation around Y axis to every point.
    Edges1 = [{rotate_y(P, Yaw), rotate_y(Q, Yaw)} || {P, Q} <- Edges],
    %% Project to 2D.
    Edges2 = [{project(P, W, H), project(Q, W, H)} || {P, Q} <- Edges1],
    %% Rasterise.
    Grid0 = #{},
    Grid1 = lists:foldl(
              fun({P1, P2}, G) -> draw_line(G, W, H, P1, P2) end,
              Grid0, Edges2),
    %% Overlay QR if present (qr phase).
    Grid2 = case {Phase, Qr} of
        {qr, [_|_] = Lines} -> overlay_qr(Grid1, W, Lines);
        _                    -> Grid1
    end,
    %% Single status line, just below the laptop. The laptop
    %% silhouette tops out at H/2 - 2 - max-Y * Scale * 0.5 and
    %% bottoms out at roughly H/2 + 2; placing the status at
    %% H/2 + 7 lands it under the base with breathing room.
    Footer = footer_text(Phase, Ip, HbT0),
    StatusRow = (H div 2) + 7,
    Grid3 = overlay_centered(Grid2, W, min(StatusRow, H), Footer),
    %% Emit: cursor home, then row by row separated by \r\n.
    Rows = [emit_row(Grid3, W, R) || R <- lists:seq(1, H)],
    _ = S,
    [<<"\e[H">> |
     lists:join(<<"\r\n">>, Rows)].

emit_row(Grid, W, Row) ->
    [maps:get({Row, Col}, Grid, $\s) || Col <- lists:seq(1, W)].

%% Overlay a list of QR lines onto the lid panel area. We position
%% it at the projected centre of the lid; for simplicity (and so it
%% doesn't drift across frames) we use a fixed rectangle near the
%% top-centre of the grid.
overlay_qr(Grid, W, Lines) ->
    WidthQr = lists:max([length(L) || L <- Lines]),
    StartCol = W div 2 - WidthQr div 2,
    StartRow = 3,
    {GridOut, _} =
        lists:foldl(
          fun(Line, {G, R}) ->
              G1 = lists:foldl(
                     fun({Idx, Ch}, GG) ->
                         plot(GG, W, 1000, StartCol + Idx, R, Ch)
                     end,
                     G,
                     lists:zip(lists:seq(0, length(Line) - 1), Line)),
              {G1, R + 1}
          end,
          {Grid, StartRow},
          Lines),
    GridOut.

overlay_centered(Grid, W, Row, Text) ->
    Pad = max(0, (W - length(Text)) div 2),
    lists:foldl(
      fun({I, Ch}, G) ->
          plot(G, W, 1000, Pad + I + 1, Row, Ch)
      end,
      Grid,
      lists:zip(lists:seq(0, length(Text) - 1), Text)).

%% Status line texts -- short, single-purpose, fits on one row.
%% These are the only words the operator sees on screen during boot.
%%
%% In `hb-wait' we surface the IP + elapsed seconds. The IP is known
%% the moment udhcpc binds (well before HB cowboy is up); printing it
%% early gives the operator the URL they'll actually scan -- the QR
%% just confirms it. The seconds counter exists because HB cold-start
%% on TCG-emulated amd64 can run 60-180 s, which to a human staring
%% at a static "starting HyperBEAM..." looks indistinguishable from
%% "hung". A monotonically ticking number proves the boot is alive.
footer_text(boot, _, _)              -> "starting LapEE...";
footer_text('net-up', undefined, _)  -> "network up; starting HyperBEAM...";
footer_text('net-up', Ip, _)         -> "network up (" ++ Ip ++ "); starting HyperBEAM...";
footer_text('hb-wait', undefined, _) -> "starting HyperBEAM...";
footer_text('hb-wait', Ip, undefined) ->
    "starting HyperBEAM... " ++ Ip;
footer_text('hb-wait', Ip, HbT0) ->
    Now = erlang:monotonic_time(millisecond),
    Secs = (Now - HbT0) div 1000,
    "starting HyperBEAM... " ++ Ip ++
        " (" ++ integer_to_list(Secs) ++ "s)";
footer_text(qr, undefined, _)        -> "ready";
footer_text(qr, Ip, _)               -> "ready at http://" ++ Ip ++ ":8734/";
footer_text(_, _, _)                 -> "".

%% ============================================================
%% Helpers
%% ============================================================
nth(N, L) -> lists:nth(N, L).

%% ============================================================
%% Diagnostic log -- writes to /run/lapee/splash.log
%% ============================================================
%% File-based, append-on-each-event. Writeback in init copies this
%% to /mnt/esp/lapee-splash.log alongside the attestation so the
%% operator can post-mortem any "stuck splash" report after pulling
%% the stick. NOT user-sensitive: the log records phase transitions,
%% IP (already on screen), and httpc reasons -- no PSK, no SSID, no
%% wallet material. We swallow all errors -- the log is best-effort
%% diagnostic; it must never crash the splash itself.
log_start() ->
    catch file:write_file(log_path(),
        io_lib:format("[lapee-splash] started pid=~p t=~p~n",
                      [self(), erlang:monotonic_time(millisecond)])).

log_event(Msg) ->
    Line = io_lib:format("[lapee-splash] ~s~n",
                         [lists:flatten(Msg)]),
    catch file:write_file(log_path(), Line, [append]).
