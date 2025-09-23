%%% @doc A discovery engine for searching for and returning messages found in
%%% a node's cache, through supported stores.
%%% 
%%% This device supports various modes of matching, including:
%%%
%%% - `all' (default): Match all keys in the request message.
%%% - `base': Match all keys in the base message.
%%% - `only': Match only the key(s) specified in the `only' key.
%%% 
%%% The `only' key can be a binary, a map, or a list of keys. If it is a binary,
%%% it is split on commas to get a list of keys to search for. If it is a message,
%%% it is used directly as the match spec. If it is a list, it is assumed to be
%%% a list of keys that we should select from the request or base message and
%%% use as the match spec.
%%%
%%% The `return' key can be used to specify the type of data to return.
%%%
%%% - `count': Return the number of matches.
%%% - `paths': Return the paths of the matches in a list.
%%% - `messages': Return the messages associated with each match in a list.
%%% - `first-path': Return the first path of the matches.
%%% - `first-message': Return the first message of the matches.
%%% - `boolean': Return a boolean indicating whether any matches were found.
-module(dev_query).
%%% Message matching API:
-export([info/1, only/3, all/3, base/3]).
%%% GraphQL API:
-export([graphql/3, has_results/3]).
%%% Test setup:
-export([test_setup/0]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%%% Keys that should typically be excluded from searches.
-define(
    DEFAULT_EXCLUDES,
    [<<"path">>, <<"commitments">>, <<"return">>, <<"exclude">>, <<"only">>]
).

info(_Opts) ->
    #{
        excludes => [<<"keys">>, <<"set">>],
        default => fun default/4
    }.

%% @doc Execute the query via GraphQL.
graphql(Req, Base, Opts) ->
    dev_query_graphql:handle(Req, Base, Opts).

%% @doc Return whether a GraphQL esponse in a message has transaction results.
%% This key is used in HB's gateway client multirequest configuration to
%% determine if the response from the node should be considered admissible.
has_results(Base, Req, Opts) ->
    JSON =
        hb_ao:get_first(
            [
                {{as, <<"message@1.0">>, Base}, <<"body">>},
                {{as, <<"message@1.0">>, Req}, <<"body">>}
            ],
            <<"{}">>,
            Opts
        ),
    Decoded = hb_json:decode(JSON),
    ?event(debug_multi, {has_results, {decoded_json, Decoded}}),
    case Decoded of
        #{ <<"data">> := #{ <<"transactions">> := #{ <<"edges">> := Nodes } } }
                when length(Nodes) > 0 ->
            {ok, true};
        _ -> {ok, false}
    end.

%% @doc Search for the keys specified in the request message.
default(_, Base, Req, Opts) ->
    all(Base, Req, Opts).

%% @doc Search the node's store for all of the keys and values in the request,
%% aside from the `commitments' and `path' keys.
all(Base, Req, Opts) ->
    match(Req, Base, Req, Opts).

%% @doc Search the node's store for all of the keys and values in the base
%% message, aside from the `commitments' and `path' keys.
base(Base, Req, Opts) ->
    match(Base, Base, Req, Opts).

%% @doc Search only for the (list of) key(s) specified in `only' in the request.
%% The `only' key can be a binary, a map, or a list of keys. See the moduledoc
%% for semantics.
only(Base, Req, Opts) ->
    case hb_maps:get(<<"only">>, Req, not_found, Opts) of
        KeyBin when is_binary(KeyBin) ->
            % The descriptor is a binary, so we split it on commas to get a
            % list of keys to search for. If there is only one key, we
            % return a list with that key.
            match(binary:split(KeyBin, <<",">>, [global]), Base, Req, Opts);
        Spec when is_map(Spec) ->
            % The descriptor is a map, so we use it as the match spec.
            match(Spec, Base, Req, Opts);
        Keys when is_list(Keys) ->
            % The descriptor is a list, so we assume that it is a list of
            % keys that we should select from the request and use as the
            % match spec.
            match(Keys, Base, Req, Opts);
        not_found ->
            % We cannot find the key to match upon. Return an error.
            {error, not_found}
    end.

%% @doc Match the request against the base message, using the keys to select
%% the values from the request and (if not found) the values from the base
%% message.
match(Keys, Base, Req, Opts) when is_list(Keys) ->
    UserSpec =
        maps:from_list(
            lists:filtermap(
                fun(Key) ->
                    % Search for the value in the request. If not found,
                    % look in the base message.
                    Value =
                        hb_maps:get(
                            Key,
                            Req,
                            hb_maps:get(Key, Base, not_found, Opts),
                            Opts
                        ),
                    if Value == not_found -> false;
                    true -> {true, {Key, Value}}
                    end
                end,
                Keys
            )
        ),
    match(UserSpec, Base, Req, Opts);
match(UserSpec, _Base, Req, Opts) ->
    ?event({matching, {spec, UserSpec}}),
    FilteredSpec =
        hb_maps:without(
            hb_maps:get(<<"exclude">>, Req, ?DEFAULT_EXCLUDES, Opts),
            UserSpec
        ),
    ReturnType = hb_maps:get(<<"return">>, Req, <<"paths">>, Opts),
    ?event({matching, {spec, FilteredSpec}, {return, ReturnType}}),
    case hb_cache:match(FilteredSpec, Opts) of
        {ok, Matches} when ReturnType == <<"count">> ->
            ?event({matched, {paths, Matches}}),
            {ok, length(Matches)};
        {ok, Matches} when ReturnType == <<"paths">> ->
            ?event({matched, {paths, Matches}}),
            {ok, Matches};
        {ok, Matches} when ReturnType == <<"messages">> ->
            ?event({matched, {paths, Matches}}),
            Messages =
                lists:map(
                    fun(Path) ->
                        hb_util:ok(hb_cache:read(Path, Opts))
                    end,
                    Matches
                ),
            ?event({matched, {messages, Messages}}),
            {ok, Messages};
        {ok, Matches} when ReturnType == <<"first-path">> ->
            ?event({matched, {paths, Matches}}),
            {ok, hd(Matches)};
        {ok, Matches} when ReturnType == <<"first">>
                orelse ReturnType == <<"first-message">> ->
            ?event({matched, {paths, Matches}}),
            {ok, hb_util:ok(hb_cache:read(hd(Matches), Opts))};
        {ok, Matches} when ReturnType == <<"boolean">> ->
            ?event({matched, {paths, Matches}}),
            {ok, length(Matches) > 0};
        not_found when ReturnType == <<"boolean">> ->
            {ok, false};
        not_found ->
            {error, not_found}
    end.

%%% Tests

%% @doc Return test options with a test store.
test_setup() ->
    Store = hb_test_utils:test_store(hb_store_lmdb),
    Opts = #{ store => Store, priv_wallet => hb:wallet() },
    % Write a simple message.
    hb_cache:write(
        #{
            <<"basic">> => <<"binary-value">>,
            <<"basic-2">> => <<"binary-value-2">>
        },
        Opts
    ),
    % Write a nested and committed message.
    hb_cache:write(
        hb_message:commit(
            #{
                <<"test-key">> => <<"test-value">>,
                <<"test-key-2">> => <<"test-value-2">>,
                <<"nested">> => Nested = #{
                    <<"test-key-3">> => <<"test-value-3">>,
                    <<"test-key-4">> => <<"test-value-4">>
                }
            },
            Opts
        ),
        Opts
    ),
    % Write a list message with complex keys.
    hb_cache:write([<<"a">>, 2, ok], Opts),
    {ok, Opts, #{ <<"nested">> => hb_message:id(Nested, all, Opts) }}.

%% @doc Search for and find a basic test key.
basic_test() ->
    {ok, Opts, _} = test_setup(),
    {ok, [ID]} = hb_ao:resolve(<<"~query@1.0/all?basic=binary-value">>, Opts),
    {ok, Read} = hb_cache:read(ID, Opts),
    ?assertEqual(<<"binary-value">>, hb_maps:get(<<"basic">>, Read)),
    ?assertEqual(<<"binary-value-2">>, hb_maps:get(<<"basic-2">>, Read)),
    {ok, [Msg]} =
        hb_ao:resolve(
            <<"~query@1.0/all?basic-2=binary-value-2&return=messages">>,
            Opts
        ),
    ?assertEqual(<<"binary-value-2">>, hb_maps:get(<<"basic-2">>, Msg)),
    ok.

%% @doc Ensure that we can search for and match only a single key.
only_test() ->
    {ok, Opts, _} = test_setup(),
    {ok, [Msg]} =
        hb_ao:resolve(
            <<"~query@1.0/only=basic&basic=binary-value&wrong=1&return=messages">>,
            Opts
        ),
    ?assertEqual(<<"binary-value">>, hb_maps:get(<<"basic">>, Msg)),
    ok.

%% @doc Ensure that we can specify multiple keys to match.
multiple_test() ->
    {ok, Opts, _} = test_setup(),
    {ok, [Msg]} =
        hb_ao:resolve(
            <<
                "~query@1.0/only=basic,basic-2",
                "&basic=binary-value&basic-2=binary-value-2",
                "&return=messages"
            >>,
            Opts
        ),
    ?assertEqual(<<"binary-value">>, hb_maps:get(<<"basic">>, Msg)),
    ?assertEqual(<<"binary-value-2">>, hb_maps:get(<<"basic-2">>, Msg)),
    ok.

%% @doc Search for and find a nested test key.
nested_test() ->
    {ok, Opts, _} = test_setup(),
    {ok, [MsgWithNested]} =
        hb_ao:resolve(
            <<"~query@1.0/all?test-key=test-value&return=messages">>,
            Opts
        ),
    ?assert(hb_maps:is_key(<<"nested">>, MsgWithNested, Opts)),
    Nested = hb_maps:get(<<"nested">>, MsgWithNested, undefined, Opts),
    ?assertEqual(<<"test-value-3">>, hb_maps:get(<<"test-key-3">>, Nested, Opts)),
    ?assertEqual(<<"test-value-4">>, hb_maps:get(<<"test-key-4">>, Nested, Opts)),
    ok.

%% @doc Search for and find a list message with typed elements.
list_test() ->
    {ok, Opts, _} = test_setup(),
    {ok, [Msg]} =
        hb_ao:resolve(
            <<"~query@1.0/all?2+integer=2&3+atom=ok&return=messages">>,
            Opts
        ),
    ?assertEqual([<<"a">>, 2, ok], Msg),
    ok.

%% @doc Ensure user's can opt not to specify a key to resolve, instead specifying
%% only the matchable keys in the message.
return_key_test() ->
    {ok, Opts, _} = test_setup(),
    {ok, [ID]} =
        hb_ao:resolve(
            <<"~query@1.0/basic=binary-value">>,
            Opts
        ),
    {ok, Msg} = hb_cache:read(ID, Opts),
    ?assertEqual(<<"binary-value">>, hb_maps:get(<<"basic">>, Msg, Opts)),
    ok.

%% @doc Validate the functioning of various return types.
return_types_test() ->
    {ok, Opts, _} = test_setup(),
    {ok, [Msg]} =
        hb_ao:resolve(
            <<"~query@1.0/basic=binary-value&return=messages">>,
            Opts
        ),
    ?assertEqual(<<"binary-value">>, hb_maps:get(<<"basic">>, Msg, Opts)),
    ?assertEqual(
        {ok, 1},
        hb_ao:resolve(
            <<"~query@1.0/basic=binary-value&return=count">>,
            Opts
        )
    ),
    ?assertEqual(
        {ok, true},
        hb_ao:resolve(
            <<"~query@1.0/basic=binary-value&return=boolean">>,
            Opts
        )
    ),
    ?assertEqual(
        {ok, <<"binary-value">>},
        hb_ao:resolve(
            <<"~query@1.0/basic=binary-value&return=first-message/basic">>,
            Opts
        )
    ),
    ok.

http_test() ->
    {ok, Opts, _} = test_setup(),
    Node = hb_http_server:start_node(Opts),
    {ok, Msg} =
        hb_http:get(
            Node,
            <<"~query@1.0/only=basic&basic=binary-value?return=first">>,
            Opts
        ),
    ?assertEqual(<<"binary-value">>, hb_maps:get(<<"basic">>, Msg, Opts)),
    ok.