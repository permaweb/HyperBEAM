-module(hb_keccak).
-export([sha3_256/1]).
-export([keccak_256/1]).
-export([key_to_ethereum_address/1]).
-include_lib("eunit/include/eunit.hrl").

-on_load(init/0).

-define(APPNAME, keccak).
-define(LIBNAME, keccak_nif).

%% NIF Initialization
init() ->
    SoName = filename:join([code:priv_dir(hb), "hb_keccak"]),
    erlang:load_nif(SoName, 0).

sha3_256(_Bin) ->
    erlang:nif_error(not_loaded).

keccak_256(_Bin) ->
    erlang:nif_error(not_loaded).

to_hex(Bin) when is_binary(Bin) ->
	binary:encode_hex(Bin).

key_to_ethereum_address(Key) when is_binary(Key) ->
	<<_Prefix: 1/binary, NoCompressionByte/binary>> = Key,
	Prefix = hb_util:to_hex(hb_keccak:keccak_256(NoCompressionByte)),
	Last40 = binary:part(Prefix, byte_size(Prefix) - 40, 40),

	Hash = hb_keccak:keccak_256(Last40),
	HashHex = hb_util:to_hex(Hash),
	
	ChecksumAddress = hash_to_checksum_address(Last40, HashHex),
	ChecksumAddress.

hash_to_checksum_address(Last40, Hash) when
	is_binary(Last40),
	is_binary(Hash),
	byte_size(Last40) =:= 40  ->

	Checksummed = lists:zip(binary:bin_to_list(Last40), binary:bin_to_list(binary:part(Hash, 0, 40))),
    Formatted = lists:map(fun({Char, H}) ->
        case H >= $8 of
            true -> string:to_upper([Char]);
            false -> [Char]
        end
    end, Checksummed),
	<<"0x", (list_to_binary(lists:append(Formatted)))/binary>>.

%% Test functions
keccak_256_test() ->
	Input = <<"testing">>,
	Expected = <<"5F16F4C7F149AC4F9510D9CF8CF384038AD348B3BCDC01915F95DE12DF9D1B02">>,
	Actual = to_hex(hb_keccak:keccak_256(Input)),
    ?assertEqual(Expected, Actual).

keccak_256_key_test() ->
	Input = <<"BAoixXds4JhW42pzlLb83B3-I21lX78j3Q7cPaoFiCjMgjYwYLDj-xL132J147ifZFwRBmzmEMC8eYAXzbRNWuA">>,
	BinaryInput = hb_util:decode(Input),
	<<_Prefix: 1/binary, NoCompressionByte/binary>> = BinaryInput,

	Prefix = hb_keccak:keccak_256(NoCompressionByte),
	PrefixHex = hb_util:to_hex(Prefix),
	?assertEqual(PrefixHex, <<"12f9afe6abd38444cab38e8cb7b4360f7f6298de2e7a11009270f35f189bd77e">>),
	
	Last40 = binary:part(PrefixHex, byte_size(PrefixHex) - 40, 40),
	?assertEqual(Last40, <<"b7b4360f7f6298de2e7a11009270f35f189bd77e">>),

	Hash = hb_keccak:keccak_256(Last40),
	HashHex = hb_util:to_hex(Hash),
	
	ChecksumAddress = hash_to_checksum_address(Last40, HashHex),
	?assertEqual(ChecksumAddress, <<"0xb7B4360F7F6298dE2e7a11009270F35F189Bd77E">>).

keccak_256_key_to_address_test() ->
	Input = <<"BAoixXds4JhW42pzlLb83B3-I21lX78j3Q7cPaoFiCjMgjYwYLDj-xL132J147ifZFwRBmzmEMC8eYAXzbRNWuA">>,
	ChecksumAddress = key_to_ethereum_address(hb_util:decode(Input)),
	?assertEqual(ChecksumAddress, <<"0xb7B4360F7F6298dE2e7a11009270F35F189Bd77E">>).

sha3_256_test() ->
    %% "abc" => known SHA3-256 hash from NIST
    Input = <<"testing">>,
    Expected = <<"7F5979FB78F082E8B1C676635DB8795C4AC6FABA03525FB708CB5FD68FD40C5E">>,
	Actual = to_hex(hb_keccak:sha3_256(Input)),
    ?assertEqual(Expected, Actual).
