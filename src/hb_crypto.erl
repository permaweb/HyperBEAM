%%% @doc Implements the cryptographic functions and wraps the primitives
%%% used in HyperBEAM. Abstracted such that this (extremely!) dangerous code 
%%% can be carefully managed.
%%% 
%%% HyperBEAM currently implements two hashpath algorithms:
%%% 
%%% * `sha-256-chain': A simple chained SHA-256 hash.
%%% 
%%% * `accumulate-256': A SHA-256 hash that chains the given IDs and accumulates
%%%   their values into a single commitment.
%%% 
%%% The accumulate algorithm is experimental and at this point only exists to
%%% allow us to test multiple HashPath algorithms in HyperBEAM.
-module(hb_crypto).
-export([sha256/1, sha256_chain/2, accumulate/2]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Add a new ID to the end of a SHA-256 hash chain.
sha256_chain(ID1, ID2) when ?IS_ID(ID1) ->
    sha256(<<ID1:32/binary, ID2/binary>>);
sha256_chain(ID1, ID2) ->
    throw({cannot_chain_bad_ids, ID1, ID2}).

%% @doc Accumulate two IDs into a single commitment.
%% Experimental! This is not necessarily a cryptographically-secure operation.
accumulate(ID1 = << ID1Int:256 >>, ID2) when ?IS_ID(ID1) ->
    << ID2Int:256 >> = sha256_chain(ID1, ID2),
    << (ID1Int + ID2Int):256 >>;
accumulate(ID1, ID2) ->
    throw({cannot_accumulate_bad_ids, ID1, ID2}).

%% @doc Wrap Erlang's `crypto:hash/2' to provide a standard interface.
%% Under-the-hood, this uses OpenSSL.
sha256(Data) ->
    crypto:hash(sha256, Data).

%%% Tests

%% @doc Count the number of leading zeroes in a bitstring.
count_zeroes(<<>>) ->
    0;
count_zeroes(<<0:1, Rest/bitstring>>) ->
    1 + count_zeroes(Rest);
count_zeroes(<<_:1, Rest/bitstring>>) ->
    count_zeroes(Rest).

%% @doc Check that `sha-256-chain' correctly produces a hash matching
%% the machine's OpenSSL lib's output. Further (in case of a bug in our
%% or Erlang's usage of OpenSSL), check that the output has at least has
%% a high level of entropy.
sha256_chain_test() ->
    ID1 = <<1:256>>,
    ID2 = <<2:256>>,
    ID3 = sha256_chain(ID1, ID2),
    HashBase = << ID1/binary, ID2/binary >>,
    ?assertEqual(ID3, crypto:hash(sha256, HashBase)),
    % Basic entropy check.
    Avg = count_zeroes(ID3) / 256,
    ?assert(Avg > 0.4),
    ?assert(Avg < 0.6).