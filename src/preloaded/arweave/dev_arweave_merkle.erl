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
