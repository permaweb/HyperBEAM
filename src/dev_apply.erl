%%% @doc A device that executes AO resolutions. It can be passed a key that
%%% refers to a path stored in the base message to execute upon the base or
%%% message referenced by the `source' key.
%%%
%%% Alternatively, a `base' and `request' pair can be passed to execute
%%% together via invoking the `pair' key.
%%%
%%% When given a message with a `base' and `request' key, the default handler
%%% will invoke `pair' upon it, setting the `path' in the resulting request to
%%% the key that `apply' was invoked with.
%%% 
%%% Paths found in keys interpreted by this device can contain a `base:' or
%%% `request:' prefix to indicate the message from which the path should be
%%% retrieved. If no such prefix is present, the `Request' message is checked
%%% first, and the `Base' message is checked second.
-module(dev_apply).
-export([info/1, pair/3, default/4]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc The device info. Forwards all keys aside `pair', `keys' and `set' are
%% resolved with the `apply/4' function.
info(_) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set_path">>, <<"remove">>],
        default => fun default/4
    }.

%% @doc The default handler. If the `base' and `request' keys are present in
%% the given request, then the `pair' function is called. Otherwise, the `eval'
%% key is used to resolve the request.
default(Key, Base, Request, Opts) ->
    ?event(debug_apply, {req, {key, Key}, {base, Base}, {request, Request}}),
    FoundBase = hb_maps:get(<<"base">>, Request, not_found, Opts),
    FoundRequest = hb_maps:get(<<"request">>, Request, not_found, Opts),
    case {FoundBase, FoundRequest} of
        {B, R} when B =/= not_found andalso R =/= not_found ->
            pair(Key, Base, Request, Opts);
        _ ->
            eval(Base, Request#{ <<"apply-path">> => Key }, Opts)
    end.

%% @doc Apply a request. We source the `base' message for the request either
%% from the `source' key if it is present, or we assume that the entire base
%% should be used. After sourcing the base, we resolve the `apply-path' on top
%% of it as a singleton message, if it is present in the request.
eval(Base, Request, Opts) ->
    maybe
        ?event({eval, {base, Base}, {request, Request}}),
        {ok, ApplyBase} ?=
            case find_path(<<"source">>, Base, Request, Opts) of
                {ok, SourcePath} ->
                    find_key(SourcePath, Base, Request, Opts);
                {error, path_not_found, _} ->
                    % If the base is not found, we return the base for this 
                    % request, minus the device (which will, inherently, be
                    % `apply@1.0' and cause recursion).
                    {ok, hb_maps:without([<<"device">>], Base, Opts)}
            end,
        ?event({eval, {apply_base, ApplyBase}}),
        case find_path(<<"apply-path">>, Base, Request, Opts) of
            {error, path_not_found, _} ->
                ?event({eval, no_path_to_execute}),
                {ok, ApplyBase};
            {ok, ApplyPathKey} ->
                ?event({eval, {key_containing_path_to_execute, ApplyPathKey}}),
                case find_key(ApplyPathKey, ApplyBase, Request, Opts) of
                    {error, _, _} ->
                        ?event({eval, path_to_execute_not_found}),
                        {error,
                            <<
                                "Path `",
                                (normalize_path(ApplyPathKey))/binary,
                                "` to execute not found."
                            >>
                        };
                    {ok, ApplyPath} ->
                        ApplyMsg = ApplyBase#{ <<"path">> => ApplyPath },
                        ?event({executing, ApplyMsg}),
                        hb_ao:resolve(ApplyMsg, Opts)
                end
        end
    else
        Error -> error_to_message(Error)
    end.

%% @doc Apply the message found at `request' to the message found at `base'.
pair(Base, Request, Opts) ->
    pair(<<"undefined">>, Base, Request, Opts).
pair(PathToSet, Base, Request, Opts) ->
    maybe
        {ok, RequestPath} ?= find_path(<<"request">>, Base, Request, Opts),
        {ok, BasePath} ?= find_path(<<"base">>, Base, Request, Opts),
        ?event({eval_pair, {base_source, BasePath}, {request_source, RequestPath}}),
        {ok, RequestSource} ?= find_key(RequestPath, Base, Request, Opts),
        {ok, BaseSource} ?= find_key(BasePath, Base, Request, Opts),
        PreparedRequest =
            case PathToSet of
                <<"undefined">> -> RequestSource;
                _ -> RequestSource#{ <<"path">> => PathToSet }
            end,
        ?event({eval_pair, {base, BaseSource}, {request, PreparedRequest}}),
        hb_ao:resolve(BaseSource, PreparedRequest, Opts)
    else
        Error -> error_to_message(Error)
    end.

%% @doc Resolve the given path on the message as `message@1.0'.
find_path(Path, Base, Request, Opts) ->
    Res =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, Request}, Path},
                {{as, <<"message@1.0">>, Base}, Path}
            ],
            path_not_found,
            Opts
        ),
    case Res of
        path_not_found -> {error, path_not_found, Path};
        Value -> {ok, Value}
    end.

%% @doc Find the value of the source key, supporting `base:' and `request:'
%% prefixes.
find_key(Path, Base, Request, Opts) ->
    BaseAs = {as, <<"message@1.0">>, Base},
    RequestAs = {as, <<"message@1.0">>, Request},
    MaybeResolve =
        case hb_path:term_to_path_parts(Path) of
            [BinKey|RestKeys] ->
                case binary:split(BinKey, <<":">>) of
                    [<<"base">>, <<"">>] ->
                        {message, Base};
                    [<<"request">>, <<"">>] ->
                        {message, Request};
                    [<<"base">>, Key] ->
                        {resolve, [{BaseAs, normalize_path([Key|RestKeys])}]};
                    [Req, Key] when Req == <<"request">> orelse Req == <<"req">> ->
                        {resolve, [{RequestAs, normalize_path([Key|RestKeys])}]};
                    [_] ->
                        {resolve, [
                            {RequestAs, normalize_path(Path)},
                            {BaseAs, normalize_path(Path)}
                        ]}
                end;
            _ -> {error, invalid_path, Path}
        end,
    case MaybeResolve of
        Err = {error, _, _} -> Err;
        {message, Message} -> {ok, Message};
        {resolve, Sources} ->
            ?event(
                {resolving_from_sources,
                    {path, Path},
                    {sources, Sources}
                }
            ),
            case hb_ao:get_first(Sources, source_not_found, Opts) of
                source_not_found -> {error, source_not_found, Path};
                Source -> {ok, Source}
            end
    end.

%% @doc Normalize the path.
normalize_path(Path) ->
    case hb_path:to_binary(Path) of
        <<"">> -> <<"/">>;
        P -> P
    end.

%% @doc Convert an error to a message.
error_to_message({error, invalid_path, ErrPath}) ->
    {error, #{
        <<"body">> =>
            <<"Path `", (normalize_path(ErrPath))/binary, "` is invalid.">>
    }};
error_to_message({error, source_not_found, ErrPath}) ->
    {error, #{
        <<"body">> =>
            <<
                "Source path `",
                (normalize_path(ErrPath))/binary,
                "` to apply not found."
            >>
    }};
error_to_message({error, path_not_found, ErrPath}) ->
    {error, #{
        <<"body">> =>
            <<
                "Path `",
                (normalize_path(ErrPath))/binary,
                "` to apply not found."
            >>
    }};
error_to_message(Error) ->
    Error.

%%% Tests

resolve_key_test() ->
    hb:init(),
    Base = #{
        <<"device">> => <<"apply@1.0">>,
        <<"body">> => <<"/~meta@1.0/build/node">>,
        <<"irrelevant">> => <<"irrelevant">>
    },
    Request = #{
        <<"irrelevant2">> => <<"irrelevant2">>,
        <<"path">> => <<"body">>
    },
    ?assertEqual({ok, <<"HyperBEAM">>}, hb_ao:resolve(Base, Request, #{})).

resolve_pair_test() ->
    Base = #{
        <<"device">> => <<"apply@1.0">>,
        <<"data-container">> => #{ <<"relevant">> => <<"DATA">> },
        <<"base">> => <<"data-container">>,
        <<"irrelevant">> => <<"irrelevant">>
    },
    Request = #{
        <<"irrelevant2">> => <<"irrelevant2">>,
        <<"data-path">> => <<"relevant">>,
        <<"request">> => <<"data-path">>,
        <<"path">> => <<"pair">>
    },
    ?assertEqual({ok, <<"DATA">>}, hb_ao:resolve(Base, Request, #{})).

reverse_resolve_pair_test() ->
    ?assertEqual(
        {ok, <<"TEST">>},
        hb_ao:resolve(
            <<
                "/~meta@1.0/build",
                "/node~apply@1.0&node=TEST&base=request:&request=base:"
            >>,
            #{}
        )
    ).

resolve_with_prefix_test() ->
    ShortTraceLen = hb_opts:get(short_trace_len),
    Node = hb_http_server:start_node(),
    ?assertEqual(
        {ok, ShortTraceLen},
        hb_http:request(
            <<"GET">>,
            Node,
            <<"/~meta@1.0/info/request:debug-info~apply@1.0">>,
            #{
                <<"debug-info">> => <<"short_trace_len">>
            },
            #{}
        )
    ).

apply_over_http_test() ->
    Node = hb_http_server:start_node(),
    Signed =
        hb_message:commit(
            #{
                <<"device">> => <<"apply@1.0">>,
                <<"user-path">> => <<"/user-request/test-key">>,
                <<"user-request">> =>
                    #{
                        <<"test-key">> => <<"DATA">>
                    }
            },
            #{ priv_wallet => hb:wallet() }
        ),
    ?assertEqual(
        {ok, <<"DATA">>},
        hb_ao:resolve(
            Signed#{ <<"path">> => <<"/user-path">> },
            #{ priv_wallet => hb:wallet() }
        )
    ),
    ?assertEqual(
        {ok, <<"DATA">>},
        hb_http:request(
            <<"GET">>,
            Node,
            <<"/user-path">>,
            Signed,
            #{ priv_wallet => hb:wallet() }
        )
    ).
