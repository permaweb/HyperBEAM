%%% @doc Encode and decode data using the `zlib` standard library.
-module(dev_gzip).
-specification("../../../specs/gzip@1.0.md").
-export([unzip/3, zip/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc The largest body that `unzip/3' produces, unless the node's
%% `gzip-max-size' option gives another.
-define(DEFAULT_MAX_SIZE, 128 * 1024 * 1024).

%% @doc Unzip a message with a `content-encoding' key of `gzip' and a `body' key, 
%% containting a gzip-encoded payload. Returns the rest of the base message 
%% unchanged, with the `content-encoding' key unset.
%% 
-spec unzip(
    #{ body => _, 'content-encoding' => binary(), _ => _ },
    #{ _ => _ },
    #{ _ => _ }
) -> {ok, #{ body => _, _ => _ }} | {error, map()}.
unzip(Base, _Req, Opts) ->
    case maps:get(<<"content-encoding">>, Base, <<"gzip">>) of
        <<"gzip">> ->
            case maps:find(<<"body">>, Base) of
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
                    case unzip_body(Body, Opts) of
                        {ok, Unzipped} ->
                            {
                                ok,
                                hb_ao:set(
                                    Base,
                                    #{
                                        <<"body">> => Unzipped,
                                        <<"content-encoding">> => unset
                                    },
                                    Opts
                                )
                            };
                        {error, _} = Error ->
                            Error
                    end
            end;
        _ ->
            ?event(
                debug_gzip,
                {unzip_ignoring_unencoded, Base},
                Opts
            ),
            {ok, Base}
    end.

%% @doc Inflate a gzip body, refusing once its output exceeds the maximum.
%% `zlib:safeInflate/2' returns bounded chunks, so an over-limit stream stops
%% before the full body is buffered.
unzip_body(Body, Opts) ->
    MaxSize = hb_opts:get(<<"gzip-max-size">>, ?DEFAULT_MAX_SIZE, Opts),
    Z = zlib:open(),
    try
        % `16 + 15' selects gzip framing; `reset' accepts concatenated members.
        ok = zlib:inflateInit(Z, 16 + 15, reset),
        case inflate_body(Z, Body, MaxSize, 0, []) of
            {ok, Chunks} ->
                ok = zlib:inflateEnd(Z),
                {ok, iolist_to_binary(lists:reverse(Chunks))};
            {error, _} = Error ->
                Error
        end
    after
        zlib:close(Z)
    end.

inflate_body(Z, Input, MaxSize, Total, Chunks) ->
    {Status, Output} = zlib:safeInflate(Z, Input),
    NewTotal = Total + iolist_size(Output),
    case NewTotal > MaxSize of
        true ->
            {error, #{
                <<"status">> => 413,
                <<"body">> => <<"Decompressed body above the maximum.">>
            }};
        false ->
            case Status of
                continue ->
                    inflate_body(Z, [], MaxSize, NewTotal, [Output | Chunks]);
                finished ->
                    {ok, [Output | Chunks]}
            end
    end.

%% @doc Take a base message with a `body' key and return it zipped, in-place.
%% Add a `content-encoding' key with the value `gzip'.
-spec zip(#{ body => binary(), _ => _ }, #{ _ => _ }, #{ _ => _ }) ->
    {ok, #{ body := binary(), 'content-encoding' := binary(), _ => _ }}
    | {error, binary()}.
zip(Base, _Req, Opts) ->
    case maps:find(<<"body">>, Base) of
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
