%%% @doc Scriptable fake-gun process for pool unit tests.
%%%
%%% Mimics gun's message protocol (gun_up, gun_down, gun_response, gun_data,
%%% gun_error, gun_notify) without real sockets. Each fake connection is a
%%% gen_server whose behaviour is controlled by a script supplied at start.
%%%
%%% Script is a list of actions executed in order when requests arrive:
%%%   {reply, Status, Headers, Body}  — respond fin immediately
%%%   {reply_chunked, Status, Headers, [Chunk,...]} — nofin + fin chunks
%%%   {reply_nofin, Status, Headers} — respond nofin and never finish
%%%   {error, Reason}                 — stream-level error
%%%   conn_down                       — simulate gun_down
%%%   {delay_up, Ms}                  — delay gun_up by Ms milliseconds
%%%   connect_timeout                 — never send gun_up (simulates timeout)
%%%
%%% Usage:
%%%   {ok, FakePid} = hb_gun_test_fake:open(Owner, Script)
%%%   %% FakePid sends {gun_up, FakePid, http} to Owner automatically
%%%   %% (unless script starts with delay_up/connect_timeout)
%%%
-module(hb_gun_test_fake).
-behaviour(gen_server).

-include("include/hb.hrl").

-export([open/2, open/3, close/1, request/5, cancel/2, cancelled_refs/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    owner     :: pid(),
    protocol  = http :: http | http2,
    script    = []   :: list(),
    streams   = #{} :: #{reference() => pid()},
    cancelled = []   :: [reference()]
}).

%% @doc Open a fake gun connection. Sends {gun_up, Pid, Proto} to Owner
%% unless the first script entry is connect_timeout or {delay_up, Ms}.
open(Owner, Script) ->
    open(Owner, Script, http).

open(Owner, Script, Proto) ->
    {ok, Pid} = gen_server:start_link(?MODULE, {Owner, Script, Proto}, []),
    {ok, Pid}.

close(Pid) ->
    gen_server:stop(Pid, normal, 1000).

%% @doc Mimic gun:request/5. Returns a StreamRef.
request(Pid, _Method, _Path, _Headers, _Body) ->
    Ref = make_ref(),
    gen_server:cast(Pid, {request, Ref, self()}),
    Ref.

%% @doc Mimic gun:cancel/2. Records the ref; owner can query via cancelled_refs/1.
cancel(Pid, Ref) ->
    gen_server:cast(Pid, {cancel, Ref}).

%% @doc Return list of all StreamRefs that were cancelled.
cancelled_refs(Pid) ->
    gen_server:call(Pid, cancelled_refs).

%%====================================================================
init({Owner, Script, Proto}) ->
    State = #state{owner = Owner, script = Script, protocol = Proto},
    case Script of
        [connect_timeout | _] ->
            {ok, State#state{script = tl(Script)}};
        [{delay_up, Ms} | Rest] ->
            erlang:send_after(Ms, self(), send_gun_up),
            {ok, State#state{script = Rest}};
        _ ->
            Owner ! {gun_up, self(), Proto},
            {ok, State}
    end.

handle_call(cancelled_refs, _From, State = #state{cancelled = C}) ->
    {reply, C, State};
handle_call(Req, _From, State) ->
    ?event(warning, {unhandled_call, {module, ?MODULE}, {request, Req}}),
    {reply, ok, State}.

handle_cast({request, Ref, CallerPid},
            State = #state{owner = Owner, script = Script, protocol = Proto}) ->
    case Script of
        [] ->
            CallerPid ! {gun_error, self(), Ref, no_script},
            {noreply, State};
        [conn_down | Rest] ->
            Owner ! {gun_down, self(), Proto, conn_down, []},
            {noreply, State#state{script = Rest}};
        [{reply, Status, Headers, Body} | Rest] ->
            CallerPid ! {gun_response, self(), Ref, fin, Status, Headers},
            _ = Body,
            {noreply, State#state{script = Rest}};
        [{reply_chunked, Status, Headers, Chunks} | Rest] ->
            CallerPid ! {gun_response, self(), Ref, nofin, Status, Headers},
            send_chunks(CallerPid, self(), Ref, Chunks),
            {noreply, State#state{script = Rest}};
        [{reply_nofin, Status, Headers} | Rest] ->
            CallerPid ! {gun_response, self(), Ref, nofin, Status, Headers},
            {noreply, State#state{script = Rest}};
        [{error, Reason} | Rest] ->
            CallerPid ! {gun_error, self(), Ref, Reason},
            {noreply, State#state{script = Rest}}
    end;

handle_cast({cancel, Ref}, State = #state{cancelled = C}) ->
    {noreply, State#state{cancelled = [Ref | C]}};
handle_cast(Cast, State) ->
    ?event(warning, {unhandled_cast, {module, ?MODULE}, {cast, Cast}}),
    {noreply, State}.

handle_info(send_gun_up, State = #state{owner = Owner, protocol = Proto}) ->
    Owner ! {gun_up, self(), Proto},
    {noreply, State};

handle_info(Msg, State) ->
    ?event(warning, {unhandled_info, {module, ?MODULE}, {message, Msg}}),
    {noreply, State}.

terminate(_Reason, _State) -> ok.

send_chunks(_CallerPid, _GunPid, _Ref, []) -> ok;
send_chunks(CallerPid, GunPid, Ref, [Last]) ->
    CallerPid ! {gun_data, GunPid, Ref, fin, Last};
send_chunks(CallerPid, GunPid, Ref, [H | T]) ->
    CallerPid ! {gun_data, GunPid, Ref, nofin, H},
    send_chunks(CallerPid, GunPid, Ref, T).
