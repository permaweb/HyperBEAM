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
    flush_context/1,
    set_tcti/1
]).

-on_load(init/0).

%% NIF lives alongside the rest of HB's priv files when the `lapee'
%% rebar3 profile is used; at release time `priv/lapee_tpm_nif.so' ends
%% up at `lib/hb-<vsn>/priv/', which `code:priv_dir(hb)' finds.
-define(APPNAME, hb).
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
    %% On macOS we pass the full library path so dlopen doesn't need the
    %% loader search path to include the tss2 prefix.
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
    %% Allow verifier-only HB instances (no TPM present) to load this
    %% module successfully. With LAPEE_TPM_ALLOW_NO_NIF=1, a load
    %% failure is logged but treated as OK — the NIF stubs still
    %% raise `nif_not_loaded' if called, so attest operations fail
    %% explicitly while verify/parse paths (which don't touch the
    %% TPM) continue to work.
    case erlang:load_nif(SoName, Tcti) of
        ok ->
            ok;
        {error, _} = Err ->
            case os:getenv("LAPEE_TPM_ALLOW_NO_NIF") of
                V1 when V1 =:= false; V1 =:= ""; V1 =:= "0" ->
                    Err;
                _ ->
                    %% on_load runs very early — logger may not be up
                    %% yet. Use stderr directly.
                    io:format(standard_error,
                              "[lapee_tpm_nif] running without NIF "
                              "(LAPEE_TPM_ALLOW_NO_NIF set; load_nif "
                              "returned ~p)~n",
                              [Err]),
                    ok
            end
    end.

%% --- NIF stubs; real implementations live in c_src/ ---

startup() -> erlang:nif_error(nif_not_loaded).

pcr_read(_Idx) -> erlang:nif_error(nif_not_loaded).

pcr_extend(_Idx, _Data) -> erlang:nif_error(nif_not_loaded).

create_primary_ek() -> erlang:nif_error(nif_not_loaded).

create_signing_key(_ParentHandle) -> erlang:nif_error(nif_not_loaded).

quote(_SignHandle, _PcrList, _Nonce) -> erlang:nif_error(nif_not_loaded).

sign(_SignHandle, _Message) -> erlang:nif_error(nif_not_loaded).

flush_context(_Handle) -> erlang:nif_error(nif_not_loaded).

set_tcti(_TctiString) -> erlang:nif_error(nif_not_loaded).
