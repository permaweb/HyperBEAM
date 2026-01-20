%%% @doc A minimal implementation of the AO-Core 1.5 resolution system.
-module(hb_ao_micro).
-export([resolve/2, resolve/3]).

resolve(Path, Opts) ->
    [Base|Reqs] = hb_path:term_to_path_parts(Path, Opts),
    lists:foldl(
        fun(Req, XBase) -> resolve(XBase, Req, Opts) end,
        Base,
        Reqs
    ).

%% @doc Resolve a hashpath via its `Base` and `Req` (`Prefix/Suffix`) components.
%% Stages:
%% 1. Normalize.
%% 2. Try to read the key directly. Return if found.
%% 3. Try to read the `device' of the `Base'. Return `Not Found' on absence.
%%    Additionally, extract the `path' from the `Req'.
%% 4. Read `Device/Req' from device. Return `Not Found' on absence.
%% 5. Vary the `Base' and `Req' by the `device' and `path'.
%% 6. Try to read `VariedBase/VariedReq' from the cache. Return if found.
%% 7. Execute the device function.
%% 8. Write the result to the cache. Link `VariedBase/VariedReq' to `Result'.
%% 9. Replace `... : base` in the result with `... : OriginalBaseID' if present.
%% 10. Return the result.
resolve(Base, Req, Opts) -> stage(1, Base, Req, Opts).
stage(1, Base, Req, Opts) when not is_binary(Base) ->
    {ok, BaseID} = hb_cache:write(Base, Opts),
    stage(1, BaseID, Req, Opts);
stage(1, BaseID, Req, Opts) when not is_binary(Req) ->
    {ok, ReqID} = hb_cache:write(Req, Opts),
    stage(1, BaseID, ReqID, Opts);
stage(1, BaseID, ReqID, Opts) ->
    stage(2, BaseID, ReqID, Opts);
stage(2, BaseID, ReqID, Opts) ->
    case hb_cache:read(<<BaseID/binary, "/", ReqID/binary>>, Opts) of
        {ok, Msg} -> {ok, Msg};
        not_found -> stage(3, BaseID, ReqID, Opts)
    end;
stage(3, BaseID, ReqID, Opts) ->
    DeviceID =
        case hb_cache:read(<<BaseID/binary, "/device">>, Opts) of
            {ok, DeviceID} -> DeviceID;
            not_found -> <<"message@1.0">>
        end,
    Path =
        case hb_cache:read(<<ReqID/binary, "/path">>, Opts) of
            {ok, Path} -> Path;
            not_found -> <<"set">>
        end,
    stage(4, BaseID, ReqID, DeviceID, Path, Opts);
stage(4, BaseID, ReqID, Device, Path, Opts) ->
    case hb_cache:read(<<Device/binary, "/", ReqID/binary>>, Opts) of
        {ok, Func} -> stage(5, BaseID, ReqID, Device, Path, Func, Opts);
        not_found -> {error, not_found}
    end;
stage(5, BaseID, ReqID, Device, Path, Func, Opts) ->
    
    {ok, VariedBase, VariedReq} = hb_types:vary(Device, Path, BaseID, ReqID, Opts),
    stage(6, BaseID, ReqID, Device, VariedBase, VariedReq, Opts).
stage(6, BaseID, ReqID, Device, VariedBase, VariedReq, Opts) ->
    case hb_cache:read(<<VariedBase/binary, "/", VariedReq/binary>>, Opts) of
        {ok, Msg} -> {ok, Msg};
        not_found -> stage(7, BaseID, ReqID, Device, VariedBase, VariedReq, Opts)
    end;
stage(7, BaseID, ReqID, Device, VariedBase, VariedReq, Opts) ->
    Msg = hb_device:apply(Device, VariedBase, VariedReq, Opts),
    hb_cache:write(<<VariedBase/binary, "/", VariedReq/binary>>, Msg, Opts),
    hb_cache:link(<<BaseID/binary, "/", ReqID/binary>>, <<VariedBase/binary, "/", VariedReq/binary>>, Opts),
    {ok, Msg}.