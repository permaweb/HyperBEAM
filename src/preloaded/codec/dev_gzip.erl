%%% @doc Encode and decode data using the `zlib` standard library.
-module(dev_gzip).
-export([unzip/3, zip/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Unzip a message with a `content-encoding' key of `gzip' and a `body' key, 
%% containting a gzip-encoded payload. Returns the rest of the base message 
%% unchanged, with the `content-encoding' key unset.
%% 
-spec unzip(#{ body => _, 'content-encoding' => binary(), _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ body => _, _ => _ }}.
unzip(Base, _Req, Opts) ->
    case hb_maps:get(<<"content-encoding">>, Base, <<"gzip">>, Opts) of
        <<"gzip">> ->
            case hb_maps:find(<<"body">>, Base, Opts) of
                error ->
                    ?event(
                        debug_gzip,
                        {unzip_ignoring_no_body, Base},
                        Opts
                    ),
                    {ok, Base};
                {ok, RawBody} ->
                    Body = hb_util:bin(RawBody),
                    ?event(
                        debug_gzip,
                        {unzipping_body, {size, byte_size(Body)}},
                        Opts
                    ),
                    {
                        ok,
                        hb_ao:set(
                            Base,
                            #{
                                <<"body">> => zlib:gunzip(Body),
                                <<"content-encoding">> => unset
                            },
                            Opts
                        )
                    }
            end;
        _ ->
            ?event(
                debug_gzip,
                {unzip_ignoring_unencoded, Base},
                Opts
            ),
            {ok, Base}
    end.

%% @doc Take a base message with a `body' key and return it zipped, in-place.
%% Add a `content-encoding' key with the value `gzip'.
-spec zip(#{ body => binary(), _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ body := binary(), 'content-encoding' := binary(), _ => _ }} | {error, binary()}.
zip(Base, _Req, Opts) ->
    case hb_maps:find(<<"body">>, Base, Opts) of
        {ok, Body} ->
            {
                ok,
                hb_ao:set(
                    Base,
                    #{
                        <<"body">> => zlib:gzip(Body),
                        <<"content-encoding">> => <<"gzip">>
                    },
                    Opts
                )
            };
        error ->
            {error, <<"No `body' key to zip found in message.">>}
    end.

%%% Tests

%% @doc Identity encoding preserves structured bodies without loading links.
unzip_identity_body_test() ->
    Opts = #{
        <<"store">> => hb_test_utils:test_store(),
        <<"cache-control">> => [<<"no-cache">>, <<"no-store">>],
        <<"hashpath">> => ignore
    },
    lists:foreach(
        fun(Body) ->
            Base = #{
                <<"device">> => <<"gzip@1.0">>,
                <<"content-encoding">> => <<"identity">>,
                <<"body">> => Body
            },
            ?assertEqual({ok, Base}, hb_ao:resolve(Base, <<"unzip">>, Opts))
        end,
        [#{ <<"nested">> => 42 }, {link, <<"missing">>, #{}}]
    ).

unzip_encoded_response_test() ->
    Opts = #{},
    Base = #{ <<"body">> => <<"Hello, world!">> },
    {ok, ID} = hb_cache:write(Base, Opts),
    {ok, Encoded} = hb_ao:resolve(<<ID/binary, "/zip~gzip@1.0">>, Opts),
    {ok, EncodedID} = hb_cache:write(Encoded, Opts),
    {ok, Unzipped} =
        hb_ao:resolve(
            <<EncodedID/binary, "/unzip~gzip@1.0/body">>,
            Opts
        ),
    ?assertEqual(<<"Hello, world!">>, Unzipped).
