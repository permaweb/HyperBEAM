%%% @doc An AO-Core interface to Arweave's annotated Merkle trees: validation
%%% of a path against a root at an offset.
%%%
%%% The device is deliberately generic. It carries no knowledge of blocks,
%%% transactions or storage proofs -- it validates an offset-indexed Merkle
%%% path under a named ruleset and nothing more. `~arweave-spora@2.9' composes
%%% it to check the `tx-path'/`data-path' pair of a proof of access, which is
%%% its one caller here.
%%%
%%% Rulesets differ in whether they enforce chunk borders, how strictly they
%%% enforce the chunk split, and whether they permit offset rebasing. The
%%% caller chooses; this device does not infer one.
%%%
%%% Every key here takes base64URL fields and nothing else. Tree construction
%%% is not exposed: a caller has no need of it -- verification only ever
%%% consumes a path -- and exposing it would mean deserialising a caller-supplied
%%% tree, which is a far harder thing to make safe than decoding a bounded
%%% string.
-module(dev_arweave_merkle).
-implements(<<"arweave-merkle@2.9">>).
-export([info/1, validate/3, note/3, extract_root/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Export only the Merkle operations, leaving message manipulation to
%% `message@1.0'.
info(_Base) ->
    #{
        excludes => [<<"keys">>, <<"set">>, <<"set-path">>, <<"remove">>]
    }.

%% @doc Validate a Merkle `proof' against a `root' at `offset', within a tree
%% spanning `size' bytes, under `ruleset'. Returns the leaf and the byte range
%% it covers.
validate(Base, Req, Opts) ->
    maybe
        {ok, Root} ?= decode(required(<<"root">>, Base, Req, Opts)),
        {ok, Path} ?= decode(required(<<"proof">>, Base, Req, Opts)),
        Offset = hb_util:int(required(<<"offset">>, Base, Req, Opts)),
        Size = hb_util:int(required(<<"size">>, Base, Req, Opts)),
        {ok, Ruleset} ?=
            ruleset(get_first(<<"ruleset">>, Base, Req, <<"basic">>, Opts)),
        validate(Root, Path, Offset, Size, Ruleset)
    end.

%% @doc Return the `note' -- the offset annotation -- of a Merkle proof's leaf.
note(Base, Req, Opts) ->
    maybe
        {ok, Path} ?= decode(required(<<"proof">>, Base, Req, Opts)),
        {ok, #{ <<"note">> => ar_merkle:extract_note(Path) }}
    end.

%% @doc Return the root recorded in a Merkle path. Named `extract-root' rather
%% than `root' because a device key shadows a base-message field of the same
%% name: `~arweave-merkle@2.9/root' would dispatch here instead of reading the
%% `root' a proof message carries.
extract_root(Base, Req, Opts) ->
    maybe
        {ok, Path} ?= decode(required(<<"proof">>, Base, Req, Opts)),
        {ok, Root} ?= extract_root(Path),
        {ok, #{ <<"root">> => hb_util:encode(Root) }}
    end.

%%% Internal functions.

%% @doc Run the underlying validation, normalising its result into the AO-Core
%% return convention. `ar_merkle' signals a malformed right bound by throwing,
%% and a failed proof by returning `false'.
validate(Root, Path, Offset, Size, Ruleset) ->
    try
        ar_merkle:validate_path(Root, Offset, Size, Path, Ruleset)
    of
        false ->
            {error, error_message(<<"invalid-merkle-path">>,
                <<"The path does not resolve to the given root.">>)};
        {Leaf, StartOffset, EndOffset} ->
            {ok,
                #{
                    <<"leaf">> => hb_util:encode(Leaf),
                    <<"start-offset">> => StartOffset,
                    <<"end-offset">> => EndOffset
                }
            }
    catch
        throw:invalid_right_bound ->
            {error, error_message(<<"invalid-right-bound">>,
                <<"The tree size must be greater than zero.">>)}
    end.

%% @doc Read the root out of a path, normalising the result into the AO-Core
%% return convention. A path shorter than one node carries no root, and
%% `ar_merkle' answers `{error, invalid_proof}' for it rather than raising --
%% so the encoder must never be reached with anything but a binary.
extract_root(Path) ->
    case ar_merkle:extract_root(Path) of
        {ok, Root} ->
            {ok, Root};
        {error, invalid_proof} ->
            {error, error_message(<<"invalid-merkle-proof">>,
                <<"The proof is too short to carry a root.">>)}
    end.

%% @doc Map a request's dashed ruleset name onto the atom `ar_merkle' expects.
%% The mapping is explicit rather than derived, both because the vendored names
%% carry a `_ruleset' suffix the wire form does not, and because an unknown
%% ruleset must be an error rather than a coerced atom. The caller chooses this
%% value, so an unknown one is a rejection with a message rather than a throw
%% the resolver renders as a 500.
ruleset(<<"basic">>) -> {ok, basic_ruleset};
ruleset(<<"strict-borders">>) -> {ok, strict_borders_ruleset};
ruleset(<<"strict-data-split">>) -> {ok, strict_data_split_ruleset};
ruleset(<<"offset-rebase-support">>) -> {ok, offset_rebase_support_ruleset};
ruleset(_Unknown) ->
    {error, error_message(<<"unknown-ruleset">>,
        <<"`ruleset' must be one of `basic', `strict-borders', "
            "`strict-data-split' or `offset-rebase-support'.">>)}.

%% @doc Read a field from the request, falling back to the base message.
%%
%% These are data fields, not keys to resolve, so they are read with
%% `hb_maps:get/4' rather than `hb_ao:get/4'. Resolving a key against a message
%% that names this device dispatches back into the device -- so reading the
%% `root' field with `hb_ao:get' would invoke `extract-root' instead of
%% returning the field. `hb_maps:get/4' reads the value directly while still
%% loading it if it is a link.
get_first(Key, Base, Req, Default, Opts) ->
    case hb_maps:get(Key, Req, not_found, Opts) of
        not_found -> hb_maps:get(Key, Base, Default, Opts);
        Value -> Value
    end.

%% @doc Read a key that has no meaningful default.
required(Key, Base, Req, Opts) ->
    case get_first(Key, Base, Req, not_found, Opts) of
        not_found -> throw({missing_key, Key});
        Value -> Value
    end.

%% @doc Decode a base64URL field that arrived from a caller.
%%
%% Every binary this device is handed is untrusted -- proofs and roots reach it
%% from Arweave peers by way of `~arweave-spora@2.9'. `hb_util:decode/1' raises
%% on malformed input; `safe_decode/1' is the checked form, so a corrupt proof
%% becomes a validation failure rather than an exception escaping the device.
decode(Encoded) ->
    case hb_util:safe_decode(Encoded) of
        {ok, Decoded} ->
            {ok, Decoded};
        {error, _} ->
            {error, error_message(<<"invalid-base64">>,
                <<"A field was not valid base64URL.">>)}
    end.

%% @doc Build the standard error body.
error_message(Message, Detail) ->
    #{
        <<"status">> => 422,
        <<"message">> => Message,
        <<"detail">> => Detail
    }.

%%% Tests.

%% @doc A tree of known chunks validates every one of its own paths, and
%% reports the byte range each covers.
validates_every_path_of_a_tree_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Leaves = [ {crypto:strong_rand_bytes(32), N * 262144} || N <- lists:seq(1, 8) ],
    {RootID, Tree} = ar_merkle:generate_tree(Leaves),
    lists:foreach(
        fun({ID, Offset}) ->
            Path = ar_merkle:generate_path(RootID, Offset - 1, Tree),
            {ok, Res} =
                hb_ao:resolve(
                    #{
                        <<"device">> => <<"arweave-merkle@2.9">>,
                        <<"root">> => hb_util:encode(RootID),
                        <<"proof">> => hb_util:encode(Path),
                        <<"offset">> => Offset - 1,
                        <<"size">> => 8 * 262144
                    },
                    <<"validate">>,
                    Opts
                ),
            % Assert per key rather than on the whole message: the resolver
            % attaches its own `priv'/hashpath bookkeeping to every result.
            ?assertEqual(hb_util:encode(ID), hb_maps:get(<<"leaf">>, Res, not_found, Opts)),
            ?assertEqual(Offset - 262144, hb_maps:get(<<"start-offset">>, Res, not_found, Opts)),
            ?assertEqual(Offset, hb_maps:get(<<"end-offset">>, Res, not_found, Opts))
        end,
        Leaves
    ).

%% @doc A path that does not belong to the root is rejected, and rejected with
%% the specific error the caller can branch on -- not merely `false'.
reject_foreign_path_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Leaves = [ {crypto:strong_rand_bytes(32), N * 262144} || N <- lists:seq(1, 4) ],
    {RootID, Tree} = ar_merkle:generate_tree(Leaves),
    Path = ar_merkle:generate_path(RootID, 262143, Tree),
    OtherRoot = crypto:strong_rand_bytes(32),
    {error, Error} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-merkle@2.9">>,
                <<"root">> => hb_util:encode(OtherRoot),
                <<"proof">> => hb_util:encode(Path),
                <<"offset">> => 262143,
                <<"size">> => 4 * 262144
            },
            <<"validate">>,
            Opts
        ),
    ?assertEqual(<<"invalid-merkle-path">>, hb_maps:get(<<"message">>, Error, not_found, Opts)).

%% @doc The note recorded in a path is the leaf's end offset. This is the
%% behaviour `dev_arweave_offset' already depends upon.
note_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Leaves = [ {crypto:strong_rand_bytes(32), N * 262144} || N <- lists:seq(1, 4) ],
    {RootID, Tree} = ar_merkle:generate_tree(Leaves),
    Path = ar_merkle:generate_path(RootID, 524287, Tree),
    {ok, Res} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-merkle@2.9">>,
                <<"proof">> => hb_util:encode(Path)
            },
            <<"note">>,
            Opts
        ),
    ?assertEqual(524288, hb_maps:get(<<"note">>, Res, not_found, Opts)).

%% @doc A proof that is not valid base64URL is reported as such, rather than
%% escaping the device as an exception.
%%
%% `hb_util:safe_decode/1' rejects on length -- an input whose size is not a
%% whole number of base64 quanta -- but is tolerant of out-of-alphabet
%% characters, which it decodes to arbitrary bytes. So a corrupted proof
%% usually surfaces as `invalid-merkle-path' rather than `invalid-base64';
%% either way it is a clean rejection, which is what matters here.
reject_malformed_proof_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    {error, Error} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-merkle@2.9">>,
                <<"root">> => hb_util:encode(crypto:strong_rand_bytes(32)),
                <<"proof">> => <<"abcde">>,
                <<"offset">> => 0,
                <<"size">> => 262144
            },
            <<"validate">>,
            Opts
        ),
    ?assertEqual(
        <<"invalid-base64">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc The root a path records is the root of the tree it was generated from.
%% Both node shapes are exercised: a tree of one leaf yields a path whose first
%% node is the leaf, a tree of several yields one whose first node is a branch,
%% and `ar_merkle' reconstitutes the root differently from each.
extract_root_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    lists:foreach(
        fun(Count) ->
            Leaves =
                [
                    {crypto:strong_rand_bytes(32), N * 262144}
                ||
                    N <- lists:seq(1, Count)
                ],
            {RootID, Tree} = ar_merkle:generate_tree(Leaves),
            Path = ar_merkle:generate_path(RootID, 262143, Tree),
            {ok, Res} =
                hb_ao:resolve(
                    #{
                        <<"device">> => <<"arweave-merkle@2.9">>,
                        <<"proof">> => hb_util:encode(Path)
                    },
                    <<"extract-root">>,
                    Opts
                ),
            ?assertEqual(
                hb_util:encode(RootID),
                hb_maps:get(<<"root">>, Res, not_found, Opts)
            )
        end,
        [1, 4]
    ).

%% @doc A proof too short to hold a node carries no root. `ar_merkle' answers
%% `{error, invalid_proof}' for it, which must reach the caller as a rejection
%% rather than as a `badarg' out of the encoder.
reject_rootless_proof_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    {error, Error} =
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-merkle@2.9">>,
                <<"proof">> => hb_util:encode(crypto:strong_rand_bytes(16))
            },
            <<"extract-root">>,
            Opts
        ),
    ?assertEqual(
        <<"invalid-merkle-proof">>,
        hb_maps:get(<<"message">>, Error, not_found, Opts)
    ).

%% @doc An unknown ruleset is refused rather than coerced into an atom.
reject_unknown_ruleset_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Leaves = [ {crypto:strong_rand_bytes(32), N * 262144} || N <- lists:seq(1, 4) ],
    {RootID, Tree} = ar_merkle:generate_tree(Leaves),
    Path = ar_merkle:generate_path(RootID, 262143, Tree),
    ?assertMatch(
        {error, #{ <<"message">> := <<"unknown-ruleset">> }},
        hb_ao:resolve(
            #{
                <<"device">> => <<"arweave-merkle@2.9">>,
                <<"root">> => hb_util:encode(RootID),
                <<"proof">> => hb_util:encode(Path),
                <<"offset">> => 262143,
                <<"size">> => 4 * 262144,
                <<"ruleset">> => <<"made-up">>
            },
            <<"validate">>,
            Opts
        )
    ).
