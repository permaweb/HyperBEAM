%%% @doc Encode and decode data using the `zlib` standard library.
-module(dev_gzip).
-export([unzip/3, zip/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Unzip a message with a `content-encoding' key of `gzip' and a `body' key, 
%% containting a gzip-encoded payload. Returns the rest of the base message 
%% unchanged, with the `content-encoding' key unset.
%% 
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
                {ok, Body} ->
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