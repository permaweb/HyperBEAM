%%% @doc Read-only LBRY blob source store.
%%%
%%% The native blob key is the 96-character SHA-384 blob hash. This store maps
%%% native blob keys onto `hb_store_odysee' fetch mechanics and returns
%%% `~lbry-blob@1.0' source-committed messages.
-module(hb_store_lbry_blob).
-export([start/3, stop/3, reset/3, scope/0, scope/1]).
-export([read/3, type/3, resolve/3, list/3]).
-export([write/3, group/3, link/3]).
-include_lib("eunit/include/eunit.hrl").

-define(SHA384_HEX_SIZE, 96).

start(_StoreOpts, _Req, _NodeOpts) ->
    ok.

stop(_StoreOpts, _Req, _NodeOpts) ->
    ok.

reset(_StoreOpts, _Req, _NodeOpts) ->
    ok.

scope() ->
    remote.

scope(#{ <<"scope">> := Scope }) ->
    Scope;
scope(_StoreOpts) ->
    scope().

resolve(_StoreOpts, #{ <<"resolve">> := Key }, _NodeOpts) ->
    case blob_path(Key) of
        {ok, Path} -> {ok, Path};
        Error -> Error
    end.

type(StoreOpts, #{ <<"type">> := Key }, NodeOpts) ->
    case read(StoreOpts, #{ <<"read">> => Key }, NodeOpts) of
        {ok, Msg} when is_map(Msg) -> {ok, composite};
        {ok, _Bin} -> {ok, simple};
        Error -> Error
    end.

read(StoreOpts, #{ <<"read">> := Key }, NodeOpts) ->
    case blob_path(Key) of
        {ok, Path} ->
            hb_store_odysee:read(StoreOpts, #{ <<"read">> => Path }, NodeOpts);
        Error ->
            Error
    end.

list(_StoreOpts, _Req, _NodeOpts) ->
    {error, not_found}.

write(_StoreOpts, _Req, _NodeOpts) ->
    {error, read_only}.

group(_StoreOpts, _Req, _NodeOpts) ->
    {error, read_only}.

link(_StoreOpts, _Req, _NodeOpts) ->
    {error, read_only}.

blob_path(Key0) ->
    Key = normalize_key(Key0),
    case Key of
        <<"lbry/blob/", Hash/binary>> -> hash_path(Hash);
        <<"lbry/blob-id/", Hash/binary>> -> hash_path(Hash);
        <<"odysee/blob/", _Hash/binary>> -> {ok, Key};
        <<"odysee/blob-id/", Hash/binary>> -> hash_path(Hash);
        Hash when byte_size(Hash) =:= ?SHA384_HEX_SIZE -> hash_path(Hash);
        _ -> {error, not_found}
    end.

hash_path(Hash0) ->
    Hash = normalize_hex(Hash0),
    case byte_size(Hash) of
        ?SHA384_HEX_SIZE -> {ok, <<"odysee/blob/", Hash/binary>>};
        _ -> {error, invalid_blob_hash}
    end.

normalize_key(<<"/", Rest/binary>>) ->
    normalize_key(Rest);
normalize_key(Key) when is_binary(Key) ->
    Key;
normalize_key(Key) ->
    hb_path:to_binary(Key).

normalize_hex(Hex) when is_binary(Hex) ->
    hb_util:bin(string:lowercase(binary_to_list(Hex))).

-ifdef(TEST).

read_native_hash_fixture_test() ->
    Body = <<"encrypted blob">>,
    BlobHash = hb_util:to_hex(crypto:hash(sha384, Body)),
    Store = #{
        <<"store-module">> => hb_store_lbry_blob,
        <<"fixtures">> => #{
            <<"odysee/blob/", BlobHash/binary>> => #{
                <<"device">> => <<"lbry-blob@1.0">>,
                <<"content-type">> => <<"application/octet-stream">>,
                <<"body">> => Body,
                <<"blob-hash">> => BlobHash,
                <<"blob-size">> => byte_size(Body)
            }
        }
    },
    {ok, Msg} = hb_store:read(Store, BlobHash, #{}),
    ?assertEqual(<<"lbry-blob@1.0">>, hb_maps:get(<<"device">>, Msg, #{})),
    ?assert(lists:member(<<"lbry-blob@1.0">>, hb_message:commitment_devices(Msg, #{}))),
    ?assert(hb_message:verify(Msg, source_verify_req(Msg), #{})).

source_verify_req(Msg) ->
    #{
        <<"commitment-ids">> =>
            [
                ID
            ||
                {ID, Commitment} <- maps:to_list(hb_maps:get(<<"commitments">>, Msg, #{}, #{})),
                hb_maps:get(<<"commitment-device">>, Commitment, not_found, #{})
                    =:= <<"lbry-blob@1.0">>
            ]
    }.

-endif.
