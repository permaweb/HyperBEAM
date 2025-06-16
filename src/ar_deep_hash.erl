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


hash_bin(Bin) when is_binary(Bin) ->
    crypto:hash(sha384, Bin).
