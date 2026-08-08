-module(ar_deep_hash).
-export([hash/1]).

hash(List) when is_list(List) -> hash_bin_or_list(List).

%%% INTERNAL

hash_bin_or_list(Bin) when is_binary(Bin) ->
    Tag = <<"blob", (integer_to_binary(byte_size(Bin)))/binary>>,
    hash_bin(<<(hash_bin(Tag))/binary, (hash_bin(Bin))/binary>>);
hash_bin_or_list(List) when is_list(List) ->
    Tag = <<"list", (integer_to_binary(length(List)))/binary>>,
    hash_list(List, hash_bin(Tag)).

hash_list([], Acc) ->
    Acc;
hash_list([Head | List], Acc) ->
    HashPair = <<Acc/binary, (hash_bin_or_list(Head))/binary>>,
    NewAcc = hash_bin(HashPair),
    hash_list(List, NewAcc).

%% VENDOR: `?DEEP_HASH_ALG' upstream, inlined here as its definition
%% (`ar.hrl:59', `sha384') so this module needs no include. The algorithm
%% is part of every Arweave signature preimage -- changing it would change
%% every transaction and block id, so it is inlined rather than
%% parameterised.
hash_bin(Bin) when is_binary(Bin) ->
    crypto:hash(sha384, Bin).