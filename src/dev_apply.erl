%%% @doc A device that executes AO resolutions. It can be passed either a key
%%% that points to a singleton message or list of messages to resolve, or a
%%% `base' and `request' pair to execute together via invoking the `pair' key.
%%%
%%% When given a message with a `base' and `request' key, the default handler
%%% will invoke `pair' upon it, setting the `path' in the resulting request to
%%% the key that `apply' was invoked with.
%%%
%%% If no `base' or `request' key is present, the default handler will invoke
%%% `eval' upon the given message, using the given key as the `source' of the
%%% message/list of messages to resolve.
%%%
%%% Paths found in keys interpreted by this device can contain a `base:' or
%%% `request:' prefix to indicate the message from which the path should be
%%% retrieved. If no such prefix is present, the `Request' message is checked
%%% first, and the `Base' message is checked second.
-module(dev_apply).
-export([info/1, pair/3]).
-include_lib("eunit/include/eunit.hrl").

-include("include/hb.hrl").


%% @doc The device info. Forwards all keys aside `pair', `keys' and `set' are
%% resolved with the `apply/4' function.
info(_) ->
    #{
      excludes => [<<"keys">>, <<"set">>],
      default => fun default/4
     }.


%% @doc The default handler. If the `base' and `request' keys are present in
%% the given request, then the `pair' function is called. Otherwise, the `eval'
%% key is used to resolve the request.
default(Key, Base, Request, Opts) ->
    FoundBase = hb_ao:get(<<"base">>, {as, <<"message@1.0">>, Request}, Opts),
    FoundRequest = hb_ao:get(<<"request">>, {as, <<"message@1.0">>, Request}, Opts),
    case {FoundBase, FoundRequest} of
        {B, R} when B =/= not_found andalso R =/= not_found ->
            pair(Key, Base, Request, Opts);
        _ ->
            eval(Base, Request#{<<"source">> => Key}, Opts)
    end.


%% @doc Apply the request's `source' key. If this key is invoked as a result
%% of the default handler, the `source' key is set to the key of the request.
eval(Base, Request, Opts) ->
    maybe
        {ok, Path} ?= find_path(<<"source">>, Base, Request, Opts),
        {ok, SingletonOrMsgList} ?= find_message(Path, Base, Request, Opts),
        if
            is_list(SingletonOrMsgList) ->
                hb_ao:resolve_many(SingletonOrMsgList, Opts);
            true ->
                hb_ao:resolve(SingletonOrMsgList, Opts)
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
        {ok, RequestSource} ?= find_message(RequestPath, Base, Request, Opts),
        {ok, BaseSource} ?= find_message(BasePath, Base, Request, Opts),
        PreparedRequest =
            case PathToSet of
                <<"undefined">> -> RequestSource;
                _ -> RequestSource#{<<"path">> => PathToSet}
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
          [{{as, <<"message@1.0">>, Request}, Path},
           {{as, <<"message@1.0">>, Base}, Path}],
          path_not_found,
          Opts),
    case Res of
        path_not_found -> {error, path_not_found, Path};
        Value -> {ok, Value}
    end.


%% @doc Find the value of the source key, supporting `base:' and `request:'
%% prefixes.
find_message(Path, Base, Request, Opts) ->
    BaseAs = {as, <<"message@1.0">>, Base},
    RequestAs = {as, <<"message@1.0">>, Request},
    MaybeResolve =
        case hb_path:term_to_path_parts(Path) of
            [BinKey | RestKeys] ->
                case binary:split(BinKey, <<":">>) of
                    [<<"base">>, <<"">>] ->
                        {message, Base};
                    [<<"request">>, <<"">>] ->
                        {message, Request};
                    [<<"base">>, Key] ->
                        {resolve, [{BaseAs, normalize_path([Key | RestKeys])}]};
                    [Req, Key] when Req == <<"request">> orelse Req == <<"req">> ->
                        {resolve, [{RequestAs, normalize_path([Key | RestKeys])}]};
                    [_] ->
                        {resolve, [{RequestAs, normalize_path(Path)},
                                   {BaseAs, normalize_path(Path)}]}
                end;
            _ -> {error, invalid_path, Path}
        end,
    case MaybeResolve of
        Err = {error, _, _} -> Err;
        {message, Message} -> {ok, Message};
        {resolve, Sources} ->
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
                  <<"Source path `",
                    (normalize_path(ErrPath))/binary,
                    "` to apply not found.">>
             }};
error_to_message({error, path_not_found, ErrPath}) ->
    {error, #{
              <<"body">> =>
                  <<"Path `",
                    (normalize_path(ErrPath))/binary,
                    "` to apply not found.">>
             }};
error_to_message(Error) ->
    Error.


%%% Tests


resolve_key_test() ->
    Base = #{
             <<"device">> => <<"apply@1.0">>,
             <<"irrelevant">> => <<"irrelevant">>
            },
    Request = #{
                <<"irrelevant2">> => <<"irrelevant2">>,
                <<"body">> => <<"/~meta@1.0/build/node">>,
                <<"path">> => <<"body">>
               },
    ?assertEqual({ok, <<"HyperBEAM">>}, hb_ao:resolve(Base, Request, #{})).


resolve_pair_test() ->
    Base = #{
             <<"device">> => <<"apply@1.0">>,
             <<"data-container">> => #{<<"relevant">> => <<"DATA">>},
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


resolve_with_prefix_test() ->
    ?assertEqual(
      {ok, <<"DATA">>},
      hb_ao:resolve(
        <<"/~meta@1.0/info/base:example-resolver~apply@1.0">>,
        #{
          <<"example-resolver">> => #{
                                      <<"path">> => <<"test-key">>,
                                      <<"test-key">> => <<"DATA">>
                                     }
         })).


reverse_resolve_pair_test() ->
    ?assertEqual(
      {ok, <<"TEST">>},
      hb_ao:resolve(
        <<"/~meta@1.0/build",
          "/node~apply@1.0&node=TEST&base=request:&request=base:">>,
        #{})).


apply_over_http_test() ->
    Node = hb_http_server:start_node(),
    Signed =
        hb_message:commit(
          #{
            <<"device">> => <<"apply@1.0">>,
            <<"user-request">> =>
                #{
                  <<"path">> => <<"/test-key">>,
                  <<"test-key">> => <<"DATA">>
                 }
           },
          #{priv_wallet => hb:wallet()}),
    ?assertEqual(
      {ok, <<"DATA">>},
      hb_ao:resolve(
        Signed#{<<"path">> => <<"/user-request">>},
        #{priv_wallet => hb:wallet()})),
    ?assertEqual(
      {ok, <<"DATA">>},
      hb_http:request(
        <<"GET">>,
        Node,
        <<"/user-request">>,
        Signed,
        #{priv_wallet => hb:wallet()})).
