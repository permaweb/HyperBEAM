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
%% Lid open angle (radians). 1.85 ≈ 106°, classic working tilt.
-define(LID_TARGET, 1.85).
%% Lid easing per frame. Lower = slower open. 0.04 at 12 fps =
%% the lid reaches >95% of its target after about 7 s. Was 0.15
%% in the first cut; user asked for slower so the open feels
%% deliberate before the spin settles in.
-define(LID_EASE, 0.04).
%% Yaw advance per frame, radians. Stays constant across the
%% whole splash lifetime -- the spin never locks face-on, so the
%% laptop keeps gently rotating with the URL underneath after HB
%% comes up. (Earlier cut snapped the yaw to 0 at qr phase to
%% reveal a QR overlay; the QR didn't scan reliably and the frozen
%% pose looked sad. Steady spin reads as "alive".)
-define(YAW_PER_FRAME, 0.05).

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
        phase       => boot,
        ip          => undefined,
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
%% The render+write+step_anim block is wrapped in try/catch so a
%% degenerate-but-renderable input (e.g. a malformed qrencode
%% output yielding lists:max([]) on the overlay path) doesn't kill
%% the whole splash and leave the operator staring at a frozen
%% frame for the rest of cold-start -- the exact UX bug the splash
%% was rewritten to fix. Code-review issue #3.
loop(S0) ->
    S1 = poll_state(S0),
    S2 = try
             Frame = render(S1),
             file:write(maps:get(out, S1), Frame),
             step_anim(S1)
         catch
             C:R:Stk ->
                 catch log_event(io_lib:format(
                     "render-crash ~p:~p ~P",
                     [C, R, Stk, 12])),
                 S1
         end,
    timer:sleep(?SLEEP_MS),
    loop(S2).

%% ============================================================
%% State polling -- phase machine, IP discovery, HB probe
%% ============================================================
poll_state(S = #{phase := Phase, ip := _Ip}) ->
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
                    log_event("phase=ready (HB ready on first poll)"),
                    S#{phase => ready};
                {false, Reason} ->
                    log_event(io_lib:format(
                        "phase=hb-wait (~s)", [Reason])),
                    HbT0 = erlang:monotonic_time(millisecond),
                    S#{phase => 'hb-wait', hb_wait_t0 => HbT0}
            end;
        'hb-wait' ->
            case hb_ready() of
                true ->
                    log_event("phase=ready (HB ready)"),
                    S#{phase => ready};
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
        ready ->
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
    %% `{packet, line}' makes recv block until a CRLF-terminated line
    %% lands -- exactly the HTTP status line. Without it (raw mode),
    %% TCG-emulated cowboy under cold-start can split "HTTP/1." and
    %% "1 200 OK\r\n..." across two TCP segments, falling out the
    %% bottom of the case clause as `unparsed' even though /info DID
    %% answer 200. Code-review issue #1, fixed before sign-off.
    case gen_tcp:connect(Host, Port,
                         [binary, {active, false},
                          {packet, line}, {nodelay, true}],
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

%% ============================================================
%% Animation state advance
%% ============================================================
%% The yaw advances every frame regardless of phase -- the spin
%% never locks. The lid eases toward the open target with the
%% per-frame step defined by ?LID_EASE; a smaller value is a slower,
%% more deliberate open (asymptotic, so it never quite stops moving
%% but is visually fully-open after ~7 s at 12 fps with 0.04).
step_anim(S = #{frame := F, yaw := Y, lid := L}) ->
    F1 = F + 1,
    Y1 = Y + ?YAW_PER_FRAME,
    L1 = L + (?LID_TARGET - L) * ?LID_EASE,
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

%% Scale (chars per laptop-width unit) derived from terminal size:
%% target ~50% of screen width, capped to fit vertically with room
%% for the status line below. Cells are 2:1 tall:wide so the y-axis
%% scale factor is halved; the open laptop is ~2.6 units tall.
%%
%% Resulting on-screen sizes (without env override):
%%   80x24   -> Scale 8 (32 cells / 80 wide = 40%)
%%   160x50  -> Scale 17 (68/160 = 42%)
%%   282x94  -> Scale 31 (124/282 = 44%)
%%
%% Operators on a HiDPI framebuffer where this still feels small can
%% override via LAPEE_SPLASH_SCALE=<float>.
splash_scale(W, H) ->
    case os:getenv("LAPEE_SPLASH_SCALE") of
        false -> auto_scale(W, H);
        ""    -> auto_scale(W, H);
        Str ->
            try list_to_float(Str)
            catch _:_ ->
                try float(list_to_integer(Str))
                catch _:_ -> auto_scale(W, H)
                end
            end
    end.

auto_scale(W, H) ->
    %% 4 laptop-width units want ~50% of screen width: 4*Scale = W/2,
    %% so Scale = W/8.
    %%
    %% Vertically: the look-down tilt mixes Z into projected Y, so the
    %% silhouette's row span depends on yaw. Worst-case Yt range across
    %% all yaws is roughly [-1.29, 3.25] -> 4.54 units, halved by the
    %% 2:1 char aspect = 2.27 * Scale rows. Reserve 5 rows for status
    %% line + margin and bound Scale so the spinning silhouette never
    %% clips into the footer at any yaw.
    ScaleW = W / 8.0,
    ScaleH = max(2.0, (H - 5) / 2.3),
    max(4.0, min(ScaleW, ScaleH)).

%% Y-coordinate shift so the laptop's vertical midpoint sits at
%% roughly 0.45 * H (slightly above centre, so the footer beneath the
%% base isn't crowded against the bottom edge). The yaw-aware mid
%% point Yt is ~0.98 in tilt-space.
%%   Cy_mid = H/2 - 0.98 * Scale * 0.5 - Lift  -> want H * 0.45
%%   Lift   = 0.05 * H - 0.49 * Scale
splash_lift(H, Scale) ->
    0.05 * H - 0.49 * Scale.

%% Row at which the status footer lands. ~0.92 * H -- below the
%% laptop's bottom-most projected cell at any yaw (which sits near
%% ~0.85 * H with the worst-case scale + lift above), with at least
%% one row of breathing room. Clamp to H-1 so a tiny terminal still
%% has somewhere to draw.
splash_status_row(H) ->
    max(1, min(H - 1, round(H * 0.92))).

%% Project a 3D point to a 2D grid cell.
%% Simple orthographic projection with a Y-axis tilt for "3/4 view".
%% Scale + lift are computed once per frame in render/1 and threaded.
project({X, Y, Z}, W, H, Scale, Lift) ->
    Tilt = 0.45,                                  %% radians, look-down
    Yt = Y * math:cos(Tilt) - Z * math:sin(Tilt),
    %% Char cells are roughly 2:1 tall:wide; scale Y by half.
    Cx = W / 2.0 + X * Scale,
    Cy = H / 2.0 - Yt * Scale * 0.5 - Lift,
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
render(#{cols := W, rows := H, yaw := Yaw, lid := Lid,
         phase := Phase, ip := Ip, hb_wait_t0 := HbT0}) ->
    Scale = splash_scale(W, H),
    Lift  = splash_lift(H, Scale),
    Edges = laptop_edges(Lid),
    %% Apply yaw rotation around Y axis to every point.
    Edges1 = [{rotate_y(P, Yaw), rotate_y(Q, Yaw)} || {P, Q} <- Edges],
    %% Project to 2D using the dynamic scale + lift.
    Edges2 = [{project(P, W, H, Scale, Lift),
               project(Q, W, H, Scale, Lift)}
              || {P, Q} <- Edges1],
    %% Rasterise.
    Grid0 = #{},
    Grid1 = lists:foldl(
              fun({P1, P2}, G) -> draw_line(G, W, H, P1, P2) end,
              Grid0, Edges2),
    %% Single status line, well below the laptop. Its row is computed
    %% from H so the spacing between laptop bottom and footer scales
    %% with the screen instead of being a fixed +7. The spin keeps
    %% going beneath the URL once HB is ready -- no face-on lock, no
    %% QR overlay.
    Footer = footer_text(Phase, Ip, HbT0),
    StatusRow = splash_status_row(H),
    Grid2 = overlay_centered(Grid1, W, StatusRow, Footer),
    %% Emit: cursor home, then row by row separated by \r\n.
    Rows = [emit_row(Grid2, W, R) || R <- lists:seq(1, H)],
    [<<"\e[H">> |
     lists:join(<<"\r\n">>, Rows)].

emit_row(Grid, W, Row) ->
    [maps:get({Row, Col}, Grid, $\s) || Col <- lists:seq(1, W)].

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
%% early gives the operator the URL they'll actually use. The seconds
%% counter exists because HB cold-start can run 60-180 s on TCG and
%% several seconds even on iron; a static "starting HyperBEAM..." for
%% that long looks indistinguishable from a hang.
%%
%% At `ready' we drop the elapsed counter and just show the URL; the
%% laptop keeps spinning underneath. No QR overlay, no face-on lock.
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
footer_text(ready, undefined, _)     -> "Running.";
footer_text(ready, Ip, _)            -> "Running at http://" ++ Ip ++ ":8734/";
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
%% Both helpers append. log_start used to truncate; symmetry matters
%% for the case where init's writeback copy races a second splash
%% process (shouldn't happen, but the asymmetry was a tripwire flagged
%% in code-review issue #5). [append] on busybox tmpfs lowers to
%% open(O_APPEND|O_WRONLY) + write(); both atomic for sub-page lines.
log_start() ->
    catch file:write_file(log_path(),
        io_lib:format("[lapee-splash] started pid=~p t=~p~n",
                      [self(), erlang:monotonic_time(millisecond)]),
        [append]).

log_event(Msg) ->
    Line = io_lib:format("[lapee-splash] ~s~n",
                         [lists:flatten(Msg)]),
    catch file:write_file(log_path(), Line, [append]).
