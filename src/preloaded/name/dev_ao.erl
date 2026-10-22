%%% @doc The device of the `ao:' URI scheme. An `ao:' URI is a hashpath (see
%%% `hb_hashpath'): `ao:' followed by a path, whose first part is the base.
%%% `ao:<ID>' is the hashpath of one part, the message with the ID. `hb_uri'
%%% reads an `ao:' URI with the `from' key of this device, which loads the
%%% hashpath with `hb_hashpath'.
%%%
%%% `to' with `type: uri' writes the URI `ao:<ID>' of a message. `<ID>' is the
%%% ID of the message with all of its commitments.
-module(dev_ao).
-export([from/3, to/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Loads the message that an `ao:' URI identifies. The body is the rest
%% of the URI after `ao:', or a link if the rest is itself an `ao:' URI.
-spec from(
    #{ body := any(), _ => _ },
    #{ type := binary(), _ => _ },
    #{ _ => _ }
) -> {ok, _} | {error, _}.
from(#{ <<"body">> := {link, URI, _} }, #{ <<"type">> := <<"uri">> }, Opts) ->
    hb_uri:from(URI, Opts);
from(#{ <<"body">> := Body }, #{ <<"type">> := <<"uri">> }, Opts) ->
    hb_hashpath:load(<<"ao:", Body/binary>>, Opts).

%% @doc Writes the `ao:' URI of a message.
-spec to(
    #{ _ => _ },
    #{ type := binary(), _ => _ },
    #{ _ => _ }
) -> {ok, binary()} | {error, _}.
to(Msg, #{ <<"type">> := <<"uri">> }, Opts) ->
    {ok, <<"ao:", (hb_message:id(Msg, all, Opts))/binary>>}.

%%% Tests

%% @doc Reads three `ao:' URIs: `ao:<ID>', the hashpath `ao:<ID>/a', and
%% `ao:with-commitments=<ID>,<ID>', the message with two commitments.
uri_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"priv-wallet">> => ar_wallet:new()
    },
    Msg = #{ <<"a">> => <<"1">> },
    {ok, ID} = hb_cache:write(Msg, Opts),
    ?assertEqual({ok, <<"ao:", ID/binary>>}, hb_uri:to(Msg, #{}, Opts)),
    ?assertEqual(
        {ok, <<"1">>},
        hb_ao:resolve(<<"ao:", ID/binary>>, <<"a">>, Opts)
    ),
    ?assertEqual(
        {ok, <<"1">>},
        hb_ao:resolve_many([<<"ao:", ID/binary, "/a">>], Opts)
    ),
    % With `force-message' set, as in an HTTP request, the literal that a URI
    % reads stays a literal.
    ?assertEqual(
        {ok, <<"1">>},
        hb_uri:from(
            <<"ao:", ID/binary, "/a">>,
            Opts#{ <<"force-message">> => true }
        )
    ),
    % A GET of `/ao:<ID>/a' reads the key `a' of the message.
    ?assertEqual(
        {ok, <<"1">>},
        hb_http:get(
            hb_http_server:start_node(Opts),
            <<"/ao:", ID/binary, "/a">>,
            Opts
        )
    ),
    Signed =
        hb_message:commit(
            hb_message:commit(Msg, Opts),
            Opts#{ <<"priv-wallet">> => ar_wallet:new() }
        ),
    {ok, _} = hb_cache:write(Signed, Opts),
    IDs = maps:keys(maps:get(<<"commitments">>, Signed)),
    {ok, Read} =
        hb_ao:resolve_many(
            [
                <<
                    "ao:with-commitments=",
                    (hb_util:bin(lists:join(<<",">>, IDs)))/binary
                >>
            ],
            Opts
        ),
    ?assertEqual(
        hb_message:id(Signed, all, Opts),
        hb_message:id(Read, all, Opts)
    ),
    % The cache writes that path at the combined ID. Reading the combined ID
    % gives the message with both commitments.
    {ok, ReadByID} = hb_cache:read(hb_message:id(Signed, all, Opts), Opts),
    ?assertEqual(
        hb_message:id(Signed, all, Opts),
        hb_message:id(ReadByID, all, Opts)
    ).
