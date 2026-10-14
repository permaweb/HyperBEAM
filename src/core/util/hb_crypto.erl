%%% @doc Implements the cryptographic functions and wraps the primitives
%%% used in HyperBEAM. Abstracted such that this (extremely!) dangerous code 
%%% can be carefully managed.
%%% 
%%% HyperBEAM implements one hashpath algorithm, `sha-256-chain': a simple
%%% chained SHA-256 hash.
-module(hb_crypto).
-export([sha256/1, sha256_chain/2, accumulate/1]).
-export([pbkdf2/5]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Add a new ID to the end of a SHA-256 hash chain.
sha256_chain(ID1, ID2) when byte_size(ID1) =:= 32 ->
    sha256(<<ID1:32/binary, ID2/binary>>);
sha256_chain(ID1, ID2) ->
    throw({cannot_chain_bad_ids, ID1, ID2}).

%% @doc Combine a list of IDs into one, whatever their order: the SHA-256 of
%% their text forms, sorted and joined by newlines, as of a file that lists
%% them. No ID may contain a newline.
accumulate(IDs) when is_list(IDs) ->
    [] = [ID || ID <- IDs, binary:match(ID, <<"\n">>) =/= nomatch],
    sha256(lists:join(<<"\n">>, lists:sort(IDs))).

%% @doc Wrap Erlang's `crypto:hash/2' to provide a standard interface.
%% Under-the-hood, this uses OpenSSL.
sha256(Data) ->
    crypto:hash(sha256, Data).

%% @doc Wrap Erlang's `crypto:pbkdf2_hmac/5' to provide a standard interface.
pbkdf2(Alg, Password, Salt, Iterations, KeyLength) ->
    case crypto:pbkdf2_hmac(Alg, Password, Salt, Iterations, KeyLength) of
        Key when is_binary(Key) -> {ok, Key};
        {Tag, CFileInfo, Desc} ->
            ?event(
                {pbkdf2_error,
                    {tag, Tag},
                    {desc, Desc},
                    {c_file_info, CFileInfo}
                }
            ),
            {error, Desc}
    end.

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
    % Only a 32-byte first ID can be chained.
    ?assertThrow({cannot_chain_bad_ids, _, _}, sha256_chain(<<1:336>>, ID2)),
    % Basic entropy check.
    Avg = count_zeroes(ID3) / 256,
    ?assert(Avg > 0.4),
    ?assert(Avg < 0.6).