%%%-------------------------------------------------------------------
%%% @doc lapee_tpm_nif — raw NIF bindings to libtss2-esys.
%%%
%%% This module is the lowest layer: every exported function is a NIF.
%%% The shared library is built from c_src/ and placed in priv/.
%%%-------------------------------------------------------------------
-module(lapee_tpm_nif).

-export([
    startup/0,
    pcr_read/1,
    pcr_extend/2,
    create_primary_ek/0,
    create_signing_key/1,
    quote/3,
    sign/2,
    tpm_properties/0,
    nv_read_public/1,
    nv_read/1,
    flush_context/1,
    set_tcti/1
]).

-on_load(init/0).

-define(APPNAME, lapee_tpm).
-define(LIBNAME, "lapee_tpm_nif").

init() ->
    SoName =
        case code:priv_dir(?APPNAME) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join("..", "priv")) of
                    true ->
                        filename:join("../priv", ?LIBNAME);
                    false ->
                        filename:join("priv", ?LIBNAME)
                end;
            Dir ->
                filename:join(Dir, ?LIBNAME)
        end,
    %% Default TCTI: swtpm on TCP port 2321 (matches scripts/swtpm.sh).
    %% On macOS we pass the full library path so dlopen doesn't need the loader
    %% search path to include the tss2 prefix.
    DefaultTcti =
        case os:type() of
            {unix, darwin} ->
                Lib = "/Users/sam/src/hyperbeam/.claude/worktrees/sharp-lichterman/"
                      "lapee-baremetal/work/tss2-prefix/lib/libtss2-tcti-swtpm.0.dylib",
                Lib ++ ":host=127.0.0.1,port=2321";
            _ ->
                "swtpm:host=127.0.0.1,port=2321"
        end,
    Tcti =
        case os:getenv("LAPEE_TPM_TCTI") of
            false -> DefaultTcti;
            V -> V
        end,
    erlang:load_nif(SoName, Tcti).

%% --- NIF stubs; real implementations live in c_src/ ---

startup() -> erlang:nif_error(nif_not_loaded).

pcr_read(_Idx) -> erlang:nif_error(nif_not_loaded).

pcr_extend(_Idx, _Data) -> erlang:nif_error(nif_not_loaded).

create_primary_ek() -> erlang:nif_error(nif_not_loaded).

create_signing_key(_ParentHandle) -> erlang:nif_error(nif_not_loaded).

quote(_SignHandle, _PcrList, _Nonce) -> erlang:nif_error(nif_not_loaded).

sign(_SignHandle, _Message) -> erlang:nif_error(nif_not_loaded).

tpm_properties() -> erlang:nif_error(nif_not_loaded).

nv_read_public(_TpmHandle) -> erlang:nif_error(nif_not_loaded).

nv_read(_TpmHandle) -> erlang:nif_error(nif_not_loaded).

flush_context(_Handle) -> erlang:nif_error(nif_not_loaded).

set_tcti(_TctiString) -> erlang:nif_error(nif_not_loaded).
