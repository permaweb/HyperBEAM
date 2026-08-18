%%% @doc A cache of AO-Core protocol messages and compute results.
%%%
%%% HyperBEAM stores all paths in key value stores, abstracted by the `hb_store'
%%% module. Each store has its own storage backend, but each works with simple
%%% key-value pairs. Each store can write binary keys at paths, and link between
%%% paths.
%%%
%%% There are three layers to HyperBEAMs internal data representation on-disk:
%%%
%%% 1. The raw binary data, written to the store at the hash of the content.
%%%    Storing binary paths in this way effectively deduplicates the data.
%%% 2. The hashpath-graph of all content, stored as a set of links between
%%%    hashpaths, their keys, and the data that underlies them. This allows
%%%    all messages to share the same hashpath space, such that all requests
%%%    from users additively fill-in the hashpath space, minimizing duplicated
%%%    compute.
%%% 3. Messages, referrable by their IDs (committed or uncommitted). These are
%%%    stored as a set of links commitment IDs and the uncommitted message.
%%%
%%% Before writing a message to the store, we convert it to Type-Annotated
%%% Binary Messages (TABMs), such that each of the keys in the message is
%%% either a map or a direct binary.
%%% 
%%% Nested keys are lazily loaded from the stores, such that large deeply
%%% nested messages where only a small part of the data is actually used are
%%% not loaded into memory unnecessarily. In order to ensure that a message is
%%% loaded from the cache after a `read', we can use the `ensure_loaded/1' and
%%% `ensure_all_loaded/1' functions. Ensure loaded will load the exact value
%%% that has been requested, while ensure all loaded will load the entire 
%%% structure of the message into memory.
%%% 
%%% Lazily loadable `links' are expressed as a tuple of the following form:
%%% `{link, ID, LinkOpts}', where `ID' is the path to the data in the store,
%%% and `LinkOpts' is a map of suggested options to use when loading the data.
%%% In particular, this module ensures to stash the `store' option in `LinkOpts',
%%% such that the `read' function can use the correct store without having to
%%% search unnecessarily. By providing an `Opts' argument to `ensure_loaded' or
%%% `ensure_all_loaded', the caller can specify additional options to use when
%%% loading the data -- overriding the suggested options in the link.
-module(hb_cache).
-export([read_all_commitments/2]).
-export([ensure_loaded/1, ensure_loaded/2, ensure_all_loaded/1, ensure_all_loaded/2]).
-export([read/2, read_resolved/3, write/2, write_binary/3, write_hashpath/2, link/3]).
-export([match/2, list/2, list_numbered/2]).
-export([test_unsigned/1, test_signed/1]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-define(MATCH_PREFIX, <<"~match@1.0">>).
-define(DIRECT_VALUE_LENGTH, 60).

%% @doc Ensure that a value is loaded from the cache if it is an ID or a link.
%% If it is not loadable we raise an error. If the value is a message, we will
%% load only the first `layer' of it: Representing all nested messages inside 
%% the result as links. If the value has an associated `type' key in the extra
%% options, we apply it to the read value, 'lazily' recreating a `structured@1.0'
%% form.
ensure_loaded(Msg) ->
    ensure_loaded(Msg, #{}).
ensure_loaded(Msg, Opts) ->
    ensure_loaded([], Msg, Opts).
ensure_loaded(Ref, {Status, Msg}, Opts) when Status == ok; Status == error ->
    {Status, ensure_loaded(Ref, Msg, Opts)};
ensure_loaded(Ref,
        Lk = {link, ID, LkOpts = #{ <<"type">> := <<"link">>, <<"lazy">> := Lazy }},
        RawOpts) ->
    % The link is to a submessage; either in lazy (unresolved) form, or direct
    % form.
    UnscopedOpts = hb_util:deep_merge(RawOpts, LkOpts, RawOpts),
    Opts = hb_store:scope(UnscopedOpts, hb_opts:get(scope, local, LkOpts)),
    _Store = hb_opts:get(store, no_viable_store, Opts),
    ?event_debug(debug_cache,
        {loading_multi_link,
            {link, ID},
            {link_opts, LkOpts},
            {store, _Store}
        }
    ),
    CacheReadResult =
        case hb_opts:get(commitment, undefined, Opts) of
            true ->
                do_read_commitment(ID, hb_util:deep_merge(Opts, LkOpts, Opts));
            _ ->
                hb_cache:read(ID, hb_util:deep_merge(Opts, LkOpts, Opts))
        end,
    case CacheReadResult of
        {ok, Next} ->
            ?event_debug(debug_cache,
                {loaded,
                    {link, ID},
                    {store, _Store}
                }),
            case Lazy of
                true ->
                    % We have resolved the ID of the submessage, so we continue
                    % to load the submessage itself.
                    ensure_loaded(
                        {link,
                            Next,
                            #{
                                <<"type">> => <<"link">>,
                                <<"lazy">> => false
                            }
                        },
                        Opts
                    );
                false ->
                    % The already had the ID of the submessage, so now we have
                    % the data, we simply return it.
                    Next
            end;
        {error, not_found} ->
            report_ensure_loaded_not_found(Ref, Lk, Opts)
    end;
ensure_loaded(Ref, Link = {link, ID, LinkOpts = #{ <<"lazy">> := true }}, RawOpts) ->
    % If the user provided their own options, we merge them and _overwrite_
    % the options that are already set in the link.
    UnscopedOpts = hb_util:deep_merge(RawOpts, LinkOpts, RawOpts),
    Opts = hb_store:scope(UnscopedOpts, hb_opts:get(scope, local, LinkOpts)),
    CacheReadResult = 
        case hb_opts:get(commitment, undefined, Opts) of
            true ->
                do_read_commitment(ID, Opts);
            _ ->
                read(ID, Opts)
        end,
    case CacheReadResult of
        {ok, LoadedMsg} ->
            ?event_debug(debug_caching,
                {lazy_loaded,
                    {link, ID},
                    {msg, LoadedMsg},
                    {link_opts, LinkOpts}
                }
            ),
            case hb_maps:get(<<"type">>, LinkOpts, undefined, Opts) of
                undefined -> LoadedMsg;
                Type -> hb_util:decode(Type, LoadedMsg)
            end;
        {error, not_found} ->
            report_ensure_loaded_not_found(Ref, Link, Opts)
    end;
ensure_loaded(Ref, {link, ID, LinkOpts}, Opts) ->
	ensure_loaded(Ref, {link, ID, LinkOpts#{ <<"lazy">> => true}}, Opts);
ensure_loaded(_Ref, Msg, _Opts) when not ?IS_LINK(Msg) ->
    Msg.

%% @doc Report that a value was not found in the cache. If a key is provided,
%% we report that the key was not found, otherwise we report that the link was
%% not found.
report_ensure_loaded_not_found(Ref, Lk, Opts) ->
    ?event(link_error, {link_not_resolvable, {ref, Ref}, {link, Lk}, {opts, Opts}}),
    throw(
        {necessary_message_not_found,
            hb_path:to_binary(lists:reverse(Ref)),
            hb_link:format_unresolved(Lk, Opts, 0)
        }
    ).

%% @doc Ensure that all of the components of a message (whether a map, list,
%% or immediate value) are recursively fully loaded from the stores into memory.
%% This is a catch-all function that is useful in situations where ensuring a
%% message contains no links is important, but it carries potentially extreme
%% performance costs.
ensure_all_loaded(Msg) ->
    ensure_all_loaded(Msg, #{}).
ensure_all_loaded(Msg, Opts) ->
    ensure_all_loaded([], Msg, Opts).
ensure_all_loaded(Ref, Link, Opts) when ?IS_LINK(Link) ->
    ensure_all_loaded(Ref, ensure_loaded(Ref, Link, Opts), Opts);
ensure_all_loaded(Ref, Msg, Opts) when is_map(Msg) ->
    maps:map(fun(K, V) -> ensure_all_loaded([K|Ref], V, Opts) end, Msg);
ensure_all_loaded(Ref, Msg, Opts) when is_list(Msg) ->
    lists:map(
        fun({N, V}) -> ensure_all_loaded([N|Ref], V, Opts) end,
        hb_util:number(Msg)
    );
ensure_all_loaded(Ref, Msg, Opts) ->
    ensure_loaded(Ref, Msg, Opts).

%% @doc List all items in a directory, assuming they are numbered.
list_numbered(Path, Opts) ->
    SlotDir = hb_path:to_binary(Path),
    [ hb_util:int(Name) || Name <- list(SlotDir, Opts) ].

%% @doc List all items under a given path.
list(Path, Opts) when is_map(Opts) and not is_map_key(<<"store-module">>, Opts) ->
    case hb_opts:get(store, no_viable_store, Opts) of
        not_found -> [];
        Store ->
            list(Path, Store, Opts)
    end;
list(Path, Store) ->
    list(Path, Store, #{}).
list(Path, Store, Opts) ->
    case hb_store:read(Store, Path, Opts) of
        {composite, Names} -> lists:map(fun child_name/1, Names);
        _ -> []
    end.

%% @doc Match a template message against the cache, returning a list of IDs
%% that match the template. We match on the binary representation of values,
%% rather than their types explicitly, such that 'AO-Types' keys that are
%% only partial matches do not cause the match to fail. If the `match_index' key
%% is set, indicating the presence and usage of the `~match@1.0` device, we use
%% it to find the matching messages. This lowers the complexity class of the
%% match to `O(keys * log(cache_size))` instead of `O(cache_size)`.
match(MatchSpec, Opts) ->
    ReadMode = hb_opts:get(cache_read_mode, normal, Opts),
    NormalizedSpec = normalize_match_spec(MatchSpec, ReadMode, Opts),
    case (ReadMode == raw) orelse
        (hb_opts:get(match_index, false, Opts) == false)
    of
        true -> store_match(NormalizedSpec, Opts);
        false ->
            case
                hb_ao:raw(
                    <<"match@1.0">>,
                    <<"all">>,
                    NormalizedSpec,
                    #{},
                    Opts
                )
            of
                {ok, Matches} when length(Matches) > 0 -> {ok, Matches};
                _ -> {error, not_found}
            end
    end.

%% @doc Normalize match values for store reverse-index lookups.
normalize_match_spec(MatchSpec, raw, Opts) ->
    maps:without([<<"ao-types">>], hb_ao:normalize_keys(MatchSpec, Opts));
normalize_match_spec(MatchSpec, _ReadMode, Opts) ->
    Spec = hb_message:convert(MatchSpec, tabm, <<"structured@1.0">>, Opts),
    maps:without([<<"ao-types">>], hb_ao:normalize_keys(Spec, Opts)).

%% @doc Match using the store's reverse index.
store_match(NormalizedSpec, Opts) ->
    ConvertedMatchSpec =
        maps:map(
            fun(Key, Value) -> store_match_value(Key, Value, Opts) end,
            NormalizedSpec
        ),
    case hb_store:match(
        hb_opts:get(store, no_viable_store, Opts),
        ConvertedMatchSpec,
        Opts
    ) of
        {ok, []} -> {error, not_found};
        {ok, Matches} -> {ok, Matches};
        _ -> {error, not_found}
    end.

%% @doc Generate the path at which a binary value should be stored.
generate_binary_path(Bin, Opts) ->
    Hashpath = hb_path:hashpath(Bin, Opts),
    <<"data/", Hashpath/binary>>.

%% @doc Write a message to the cache. For raw binaries, we write the data at
%% the hashpath of the data (by default the SHA2-256 hash of the data). We link
%% the unattended ID's hashpath for the keys (including `/commitments') on the
%% message to the underlying data and recurse. We then link each commitment ID
%% to the uncommitted message, such that any of the committed or uncommitted IDs
%% can be read, and once in memory all of the commitments are available. For
%% deep messages, the commitments will also be read, such that the ID of the
%% outer message (which does not include its commitments) will be built upon
%% the commitments of the inner messages. We do not, however, store the IDs from
%% commitments on signed _inner_ messages. We may wish to revisit this.
write(RawMsg, Opts) when is_map(RawMsg) ->
    hb_message:paranoid_verify(cache_write, RawMsg, Opts),
    {ok, Msg} = hb_message:with_only_committed(RawMsg, Opts),
    TABM = hb_message:convert(Msg, tabm, <<"structured@1.0">>, Opts),
    ?event_debug(debug_cache, {writing_full_message, {msg, TABM}}),
    try
        do_write_message(
            TABM,
            hb_opts:get(store, no_viable_store, Opts),
            Opts
        )
    catch
        Type:Reason:Stacktrace ->
            ?event(error,
                {cache_write_error,
                    {type, Type},
                    {reason, Reason},
                    {stacktrace, {trace, Stacktrace}}
                },
                Opts
            ),
            erlang:raise(Type, Reason, Stacktrace)
    end;
write(List, Opts) when is_list(List) ->
    write(hb_message:convert(List, tabm, <<"structured@1.0">>, Opts), Opts);
write(Bin, Opts) when is_binary(Bin) ->
    do_write_message(Bin, hb_opts:get(store, no_viable_store, Opts), Opts).

do_write_message(Bin, Store, Opts) when is_binary(Bin) ->
    % Write the binary in the store at its calculated content-hash.
    % Return the path.
    Path = generate_binary_path(Bin, Opts),
    hb_store:write(Store, #{ Path => Bin }, Opts),
    %lists:map(fun(ID) -> hb_store:make_link(Store, Path, ID) end, AllIDs),
    {ok, Path};
do_write_message(List, Store, Opts) when is_list(List) ->
    do_write_message(
        hb_message:convert(List, tabm, <<"structured@1.0">>, Opts),
        Store,
        Opts
    );
do_write_message(Msg, Store, Opts) when is_map(Msg) ->
    {ok, UncommittedID, Ops} = write_message_ops(Msg, Opts),
    run_write_ops(Store, Ops, Opts),
    {ok, UncommittedID}.

write_message_ops(Bin, Opts) when is_binary(Bin) ->
    Path = generate_binary_path(Bin, Opts),
    {ok, Path, [{write, Path, Bin}]};
write_message_ops(List, Opts) when is_list(List) ->
    write_message_ops(
        hb_message:convert(List, tabm, <<"structured@1.0">>, Opts),
        Opts
    );
write_message_ops(Msg, Opts) when is_map(Msg) ->
    ?event_debug(debug_cache, {writing_message, Msg}),
    UncommittedID =
        hb_message:id(Msg, none, Opts#{ <<"linkify-mode">> => discard }),
    AllIDs = calculate_all_ids(Msg, UncommittedID, Opts),
    AltIDs = AllIDs -- [UncommittedID],
    MsgHashpathAlg = hb_path:hashpath_alg(Msg, Opts),
    ?event_debug(debug_cache,
        {writing_message,
            {id, UncommittedID},
            {alt_ids, AltIDs},
            {original, Msg}
        }
    ),
    KeyOps =
        maps:fold(
            fun(Key, Value, Acc) ->
                write_key_ops(UncommittedID, Key, MsgHashpathAlg, Value, Opts, Acc)
            end,
            [{group, UncommittedID}],
            maps:without([<<"priv">>], Msg)
        ),
    MatchOps =
        case match_store(Opts) of
            [] -> KeyOps;
            _ -> [{match, AllIDs, Msg} | KeyOps]
        end,
    Ops =
        lists:foldl(
            fun(AltID, Acc) ->
                ?event_debug(debug_cache,
                    {linking_commitment,
                        {uncommitted_id, UncommittedID},
                        {committed_id, AltID}
                    }
                ),
                [{link, AltID, UncommittedID} | Acc]
            end,
            MatchOps,
            AltIDs
        ),
    {ok, UncommittedID, lists:reverse(Ops)}.

write_key_ops(Base, <<"commitments">>, _HPAlg, RawCommitments, Opts, Acc) ->
    Commitments = prepare_commitments(RawCommitments, Opts),
    CommitmentsBase = commitment_path(Base, Opts),
    ?event(
        {writing_commitments,
            {base, Base},
            {commitments_message, Commitments},
            {commitments_base, CommitmentsBase}
        }
    ),
    Acc1 = [{group, CommitmentsBase} | Acc],
    Acc2 =
        maps:fold(
            fun(BaseCommID, Commitment, InnerAcc) ->
                ?event_debug(debug_cache, {writing_commitment, {commitment, Commitment}}),
                {ok, CommMsgID, CommOps} = write_message_ops(Commitment, Opts),
                [
                    {link, <<CommitmentsBase/binary, "/", BaseCommID/binary>>, CommMsgID}
                    | prepend_reversed(CommOps, InnerAcc)
                ]
            end,
            Acc1,
            Commitments
        ),
    [{link, <<Base/binary, "/commitments">>, CommitmentsBase} | Acc2];
write_key_ops(Base, Key, HPAlg, Value, Opts, Acc) ->
    KeyHashPath =
        hb_path:hashpath(
            Base,
            hb_path:to_binary(Key),
            HPAlg,
            Opts
        ),
    case is_immediate_value(Key, Value) of
        true ->
            [{write, KeyHashPath, encode_immediate_value(Value)} | Acc];
        false ->
            {ok, Path, ValueOps} = write_message_ops(Value, Opts),
            [
                {link, KeyHashPath, Path}
                | prepend_reversed(ValueOps, Acc)
            ]
    end.

%% @doc True when `Value' should be stored at `Key''s path instead of linked
%% through `data/'.
is_immediate_value(Key, Value) ->
    is_binary(Value) andalso
        byte_size(Value) < ?DIRECT_VALUE_LENGTH andalso
        not hb_link:is_link_key(Key).

%% @doc Encode an immediate value so it cannot be confused with cache markers.
%% Most values are stored unchanged. Literal marker-looking values get a single
%% `raw:' prefix, so decoding removes exactly one layer and preserves literal
%% values that start with `link:' or `raw:'.
encode_immediate_value(<<"group">>) -> <<"raw:group">>;
encode_immediate_value(<<"link:", _/binary>> = Value) -> <<"raw:", Value/binary>>;
encode_immediate_value(<<"raw:", _/binary>> = Value) -> <<"raw:", Value/binary>>;
encode_immediate_value(Value) -> Value.

%% @doc Decode a value read from a message key path. Linked payloads under
%% `data/' are not immediate values, so leave them byte-for-byte intact.
decode_immediate_value(<<"data/", _/binary>>, Value) -> Value;
decode_immediate_value(_Path, <<"raw:", Value/binary>>) -> Value;
decode_immediate_value(_Path, Value) -> Value.

store_match_value(Key, Bin, Opts) when is_binary(Bin) ->
    case is_immediate_value(Key, Bin) of
        true -> encode_immediate_value(Bin);
        false -> <<"link:", (generate_binary_path(Bin, Opts))/binary>>
    end;
store_match_value(_Key, Map, Opts) when is_map(Map) ->
    <<"link:",
        (hb_message:id(
            Map,
            none,
            Opts#{ <<"linkify-mode">> => discard }
        ))/binary
    >>;
store_match_value(Key, List, Opts) when is_list(List) ->
    case io_lib:printable_unicode_list(List) of
        true ->
            store_match_value(Key, iolist_to_binary(List), Opts);
        false ->
            store_match_value(
                Key,
                hb_message:convert(List, tabm, <<"structured@1.0">>, Opts),
                Opts
            )
    end;
store_match_value(Key, Other, Opts) ->
    store_match_value(Key, hb_path:to_binary(Other), Opts).

prepend_reversed(Ops, Acc) ->
    lists:foldl(fun(Op, InnerAcc) -> [Op | InnerAcc] end, Acc, Ops).

run_write_ops(Store, Ops, Opts) ->
    run_write_ops(Store, Ops, Opts, []).
run_write_ops(Store, [], Opts, Pending) ->
    flush_write_ops(Store, Pending, Opts);
run_write_ops(Store, [{match, IDs, Msg} | Rest], Opts, Pending) ->
    flush_write_ops(Store, Pending, Opts),
    write_match_index(IDs, Msg, Opts),
    run_write_ops(Store, Rest, Opts, []);
run_write_ops(Store, [Op | Rest], Opts, Pending) ->
    run_write_ops(Store, Rest, Opts, [Op | Pending]).

flush_write_ops(_Store, [], _Opts) ->
    ok;
flush_write_ops(Store, Pending, Opts) ->
    apply_write_ops(Store, lists:reverse(Pending), Opts).

%% @doc Apply a list of write/group/link operations through the store's existing
%% interfaces, collapsing a message's per-key store round-trips into a small
%% constant number of calls: the group markers, then all values in a single
%% `write' map, then all links in a single `link' map. Links stay links -- the
%% store decides how to represent them -- so this is store-agnostic.
apply_write_ops(Store, Ops, Opts) ->
    {Groups, Writes, Links} =
        lists:foldl(
            fun({group, Path}, {Gs, Ws, Ls}) ->
                    {[Path | Gs], Ws, Ls};
               ({write, Path, Value}, {Gs, Ws, Ls}) ->
                    {Gs, Ws#{ Path => Value }, Ls};
               ({link, New, Existing}, {Gs, Ws, Ls}) ->
                    {Gs, Ws, Ls#{ New => Existing }}
            end,
            {[], #{}, #{}},
            Ops
        ),
    lists:foreach(
        fun(Group) -> hb_store:group(Store, Group, Opts) end,
        lists:reverse(Groups)
    ),
    case map_size(Writes) of
        0 -> ok;
        _ -> hb_store:write(Store, Writes, Opts)
    end,
    case map_size(Links) of
        0 -> ok;
        _ -> hb_store:link(Store, Links, Opts)
    end,
    ok.

%% @doc Write all message keys to the optional match index.
write_match_index(IDs, Base, Opts) ->
    case match_store(Opts) of
        [] -> {skip, <<"No store configured for match index.">>};
        Store ->
            IndexBase = hb_message:uncommitted(hb_private:reset(Base)),
            Ops =
                hb_maps:fold(
                fun(RawKey, Value, Acc) ->
                    Key = hb_ao:normalize_key(RawKey),
                    ValuePath = match_value_path(Value, Opts),
                    MatchAddress = match_address(Key, ValuePath),
                    lists:foldl(
                        fun(ID, InnerAcc) ->
                            Address = match_address_id(MatchAddress, ID),
                            [{write, Address, <<"">>} | InnerAcc]
                        end,
                        [{group, MatchAddress} | Acc],
                        IDs
                    )
                end,
                [],
                IndexBase
            ),
            apply_write_ops(Store, lists:reverse(Ops), Opts)
    end.

%% @doc Select the store that should receive reverse match-index writes.
%% A local `match-index' option wins when present. Otherwise, a local
%% `store' means "write this message and its match index to the same
%% caller-provided store". If neither is present, use the global node
%% `match-index' setting. The selected value is interpreted as:
%% `false' disables index writes, `true' uses the normal configured
%% store, and a store definition/list writes the index there.
match_store(Opts) ->
    LocalMatchIndex = maps:get(<<"match-index">>, Opts, undefined),
    LocalStore = maps:get(<<"store">>, Opts, undefined),
    GlobalMatchIndex = hb_opts:get(match_index, false, #{ <<"only">> => global }),
    MatchIndexStore =
        case {LocalMatchIndex, LocalStore} of
            {false, _} ->
                false;
            {[], _} ->
                [];
            {undefined, undefined} ->
                GlobalMatchIndex;
            {undefined, _} ->
                LocalStore;
            {Local, Store}
                    when Store =/= undefined andalso
                        Local =:= GlobalMatchIndex ->
                Store;
            {Local, _} ->
                Local
        end,
    case MatchIndexStore of
        false -> [];
        true -> hb_opts:get(store, [], Opts);
        ResolvedStore when is_list(ResolvedStore) -> ResolvedStore;
        ResolvedStore -> [ResolvedStore]
    end.

%% @doc Calculate the address of a key-value pair in the match index.
match_address(Key, Value) ->
    KeyBin = match_bin(Key),
    ValueBin = match_bin(Value),
    iolist_to_binary([?MATCH_PREFIX, "&", KeyBin, "=", ValueBin]).
match_address_id(Address, ID) ->
    IDBin = match_bin(ID),
    <<Address/binary, "/", IDBin/binary>>.

%% @doc Normalize a match-index path part.
match_bin(Bin) when is_binary(Bin) -> Bin;
match_bin(Atom) when is_atom(Atom) -> atom_to_binary(Atom);
match_bin(Int) when is_integer(Int) -> integer_to_binary(Int);
match_bin(Float) when is_float(Float) -> float_to_binary(Float, [compact]);
match_bin(List) when is_list(List) ->
    try iolist_to_binary(List)
    catch _:_ -> term_to_binary(List)
    end;
match_bin(Other) ->
    term_to_binary(Other).

%% @doc Return the path representation used by cache key-value links.
match_value_path(Bin, Opts) when is_binary(Bin) ->
    generate_binary_path(Bin, Opts);
match_value_path(Map, Opts) when is_map(Map) ->
    hb_message:id(Map, none, Opts#{ <<"linkify-mode">> => discard });
match_value_path(List, Opts) when is_list(List) ->
    case io_lib:printable_unicode_list(List) of
        true ->
            match_value_path(iolist_to_binary(List), Opts);
        false ->
            match_value_path(
                hb_message:convert(List, tabm, <<"structured@1.0">>, Opts),
                Opts
            )
    end;
match_value_path(Other, Opts) ->
    match_value_path(hb_path:to_binary(Other), Opts).

%% @doc The `structured@1.0` encoder does not typically encode `commitments`,
%% subsequently, when we encounter a commitments message we prepare its contents
%% separately, then write each to the store.
prepare_commitments(RawCommitments, Opts) ->
    Commitments = ensure_all_loaded(RawCommitments, Opts),
    maps:map(
        fun(_, StructuredCommitment) ->
            hb_message:convert(StructuredCommitment, tabm, Opts)
        end,
        Commitments
    ).

%% @doc Generate the commitment path for a given base path.
commitment_path(Base, Opts) ->
    hb_path:hashpath(<<Base/binary, "/commitments">>, Opts).

%% @doc Calculate the IDs for a message.
calculate_all_ids(Bin, _UncommittedID, _Opts) when is_binary(Bin) -> [];
calculate_all_ids(Msg, UncommittedID, Opts) ->
    CommIDs = 
        hb_maps:keys(
            hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
            Opts
        ),
    ?event_debug({calculating_ids, {msg, Msg}, {comm_ids, CommIDs}}),
    case length(CommIDs) of
        0 ->
            % With no commitments the `all' id is the uncommitted id we already
            % computed; skip re-serializing the message to recompute it.
            [UncommittedID];
        _ ->
            All = hb_message:id(Msg, all, Opts#{ <<"linkify-mode">> => discard }),
            case lists:member(All, CommIDs) of
                true -> CommIDs;
                false -> [All | CommIDs]
            end
    end.

%% @doc Write a hashpath and its message to the store and link it.
write_hashpath(Msg = #{ <<"priv">> := #{ <<"hashpath">> := HP } }, Opts) ->
    write_hashpath(HP, Msg, Opts);
write_hashpath(MsgWithoutHP, Opts) ->
    write(MsgWithoutHP, Opts).
write_hashpath(HP, Msg, Opts) when is_binary(HP) or is_list(HP) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    ?event_debug({writing_hashpath, {hashpath, HP}, {msg, Msg}, {store, Store}}),
    {ok, Path} = write(Msg, Opts),
    hb_store:link(Store, #{ hb_path:to_binary(HP) => Path }, Opts),
    {ok, Path}.

%% @doc Write a raw binary keys into the store and link it at a given hashpath.
write_binary(Hashpath, Bin, Opts) ->
    write_binary(Hashpath, Bin, hb_opts:get(store, no_viable_store, Opts), Opts).
write_binary(Hashpath, Bin, Store, Opts) ->
    ?event_debug({writing_binary, {hashpath, Hashpath}, {bin, Bin}, {store, Store}}),
    {ok, Path} = do_write_message(Bin, Store, Opts),
    hb_store:link(Store, #{ hb_path:to_binary(Hashpath) => Path }, Opts),
    {ok, Path}.

%% @doc Read the message at a path. Returns in `structured@1.0' format: Either
%% a richly typed map or a direct binary. If `cache-read-mode' is `raw',
%% composite reads return lazy links without decoding `ao-types'.
read(Path, Opts) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    case {
        store_read(Path, Store, Opts),
        hb_opts:get(cache_read_mode, normal, Opts)
    } of
        {{ok, Res}, raw} ->
            {ok, Res};
        {{ok, Res}, _} ->
            hb_message:paranoid_verify(cache_read, Res, Opts),
            {ok, Res};
        {Other, _} ->
            Other
    end.

do_read_commitment(Path, Opts) ->
    store_read(Path, hb_opts:get(store, no_viable_store, Opts), Opts).

%% @doc Load all of the commitments for a message into memory.
read_all_commitments(Msg, Opts) ->
    LocalOpts = hb_store:scope(Opts, local),
    UncommittedID = hb_message:id(Msg, none, Opts#{ <<"linkify-mode">> => discard }),
    CurrentCommitments = hb_maps:get(<<"commitments">>, Msg, #{}, Opts),
    AlreadyLoaded = hb_maps:keys(CurrentCommitments, Opts),
    CommitmentsPath = hb_path:to_binary([UncommittedID, <<"commitments">>]),
    FoundCommitments =
        case hb_store:list(CommitmentsPath, LocalOpts) of
            {ok, CommitmentIDs} ->
                lists:filtermap(
                    fun(CommitmentID) ->
                        ShouldLoad = not lists:member(CommitmentID, AlreadyLoaded),
                        ResolvedCommPath = hb_path:to_binary([CommitmentsPath, CommitmentID]),
                        case
                            ShouldLoad andalso
                                do_read_commitment(ResolvedCommPath, LocalOpts)
                        of
                            {ok, Commitment} ->
                                {
                                    true,
                                    {
                                        CommitmentID,
                                        ensure_all_loaded(
                                            Commitment,
                                            Opts#{ <<"commitment">> => true }
                                        )
                                    }
                                };
                            _ ->
                                false
                        end
                    end,
                    CommitmentIDs
                );
            _ ->
                []
    end,
    NewCommitments =
        hb_maps:merge(
            CurrentCommitments,
            maps:from_list(FoundCommitments)
        ),
    Msg#{ <<"commitments">> => NewCommitments }.
%% @doc List all of the subpaths of a given path and return a map of keys and
%% links to the subpaths, including their types.
store_read(Path, Store, Opts) ->
    store_read(Path, Path, Store, Opts).
store_read(_Target, _Path, no_viable_store, _) ->
    {error, not_found};
store_read(Target, <<"data/", _/binary>> = PathBin, Store, Opts) ->
    read_resolved_path(Target, PathBin, Store, Opts);
store_read(Target, Path, Store, Opts) ->
    PathBin = hb_path:to_binary(Path),
    case hb_store:resolve(Store, PathBin, Opts) of
        {ok, ResolvedFullPath} ->
            ?event_debug({reading,
                {original_path, {string, PathBin}},
                {fully_resolved_path, ResolvedFullPath},
                {store, Store}
            }),
            read_resolved_path(Target, ResolvedFullPath, Store, Opts);
        {error, _} = Error ->
            Error;
        {failure, _} = Failure ->
            Failure
    end.

%% @doc Read a path whose intermediate links are already resolved, skipping the
%% redundant per-segment `hb_store:resolve' round-trip.
read_resolved_path(_Target, _Path, no_viable_store, _) ->
    {error, not_found};
read_resolved_path(Target, ResolvedFullPath, Store, Opts) ->
    case hb_store:read(Store, ResolvedFullPath, Opts) of
        {ok, Bin} ->
            ?event_debug({reading_data, ResolvedFullPath}),
            {ok, decode_immediate_value(ResolvedFullPath, Bin)};
        {composite, Children} ->
            ?event_debug({reading_composite, ResolvedFullPath}),
            Subpaths =
                [
                    hb_util:bin(child_name(Child))
                ||
                    Child <- Children
                ],
            ?event(
                {listed,
                    {original_path, Target},
                    {subpaths, {explicit, Subpaths}}
                }
            ),
            Msg =
                case hb_opts:get(cache_read_mode, normal, Opts) of
                    raw ->
                        maps:from_list(
                            [
                                {
                                    Subpath,
                                    {link,
                                        hb_path:to_binary([ResolvedFullPath, Subpath]),
                                        #{ <<"lazy">> => true, <<"store">> => Store }
                                    }
                                }
                            ||
                                Subpath <- Subpaths,
                                Subpath =/= <<"ao-types">>
                            ]
                        );
                    _ ->
                        Values =
                            maps:from_list([
                                {hb_util:bin(Subpath), Value}
                            ||
                                {Subpath, Value} <- Children
                            ]),
                        prepare_typed_values(
                            Target,
                            ResolvedFullPath,
                            Subpaths,
                            Values,
                            Store,
                            Opts
                        )
                end,
            ?event(
                {completed_read,
                    {resolved_path, ResolvedFullPath},
                    {explicit, Msg}
                }
            ),
            {ok, Msg};
        {error, _} = Error ->
            Error;
        {failure, _} = Failure ->
            Failure
    end.

%% @doc Normalize a composite child entry to its subpath name, dropping any
%% inline value the prefix read supplied alongside it.
child_name({Subpath, _Value}) -> Subpath;
child_name(Subpath) -> Subpath.

%% @doc Prepare child values using `ao-types' where present. Immediate values
%% are returned directly; linked values remain lazy.
prepare_typed_values(Target, RootPath, Subpaths, Values, Store, Opts) ->
    {ok, Implicit, Types} = read_ao_types(RootPath, Subpaths, Values, Store, Opts),
    Res =
        maps:from_list(lists:filtermap(
            fun(<<"ao-types">>) -> false;
                (<<"commitments">>) ->
                    % List the commitments for this message, and load them into
                    % memory. If there no commitments at the path, we exclude
                    % commitments from the list of links. The `commitments' key is
                    % itself a link to the hashpath-addressed commitments group.
                    % The composite read already returned that link inline, so
                    % when it is present we address the commitment straight from
                    % the resolved group, skipping the redundant re-reads of
                    % `RootPath' and `RootPath/commitments' that the scan just
                    % materialised. Only when the inline value is absent (or is
                    % not the usual link) do we fall back to resolving the full
                    % `RootPath/commitments/Target' path from scratch.
                    CommPath =
                        case maps:get(<<"commitments">>, Values, none) of
                            <<"link:", CommGroup/binary>> when byte_size(CommGroup) > 0 ->
                                hb_path:to_binary([CommGroup, Target]);
                            _ ->
                                hb_path:to_binary(
                                    [RootPath, <<"commitments">>, Target])
                        end,
                    ?event(read_commitment,
                        {reading_commitment,
                            {target, Target},
                            {root_path, RootPath},
                            {commitments_path, CommPath}
                        }
                    ),
                    case do_read_commitment(CommPath, Opts) of
                        {ok, Commitment} ->
                            LoadedCommitment =
                                ensure_all_loaded(
                                    Commitment,
                                    Opts#{ <<"commitment">> => true }
                                ),
                            ?event(read_commitment,
                                {found_target_commitment,
                                    {path, CommPath},
                                    {commitment, LoadedCommitment}
                                }
                            ),
                            % We have commitments, so we read each commitment
                            % into memory, and return it as part of the message.
                            {
                                true,
                                {
                                    <<"commitments">>,
                                    #{ Target => LoadedCommitment }
                                }
                            };
                        _ ->
                            false
                    end;
                (Subpath) ->
                    ?event(
                        {returning_link,
                            {subpath, Subpath}
                        }
                    ),
                    SubkeyPath = hb_path:to_binary([RootPath, Subpath]),
                    case hb_link:is_link_key(Subpath) of
                        false ->
                            % The key is a literal value, not a nested composite
                            % message. Subsequently, we return a resolvable link
                            % to the subpath, leaving the key as-is.
                            LinkOpts =
                                (case Types of
                                    #{ Subpath := Type } ->
                                        % We have an `ao-types' entry for the
                                        % subpath, so the link carries its type
                                        % and resolves lazily to the final value.
                                        #{
                                            <<"type">> => Type,
                                            <<"lazy">> => true
                                        };
                                    _ ->
                                        % No `ao-types' entry: the subpath is a
                                        % literal value, resolved lazily.
                                        #{
                                            <<"lazy">> => true
                                        }
                                end)#{ <<"store">> => Store },
                            PreparedValue =
                                case maps:get(Subpath, Values, none) of
                                    <<"raw:", ImmediateValue/binary>> ->
                                        apply_type_to_immediate(ImmediateValue, LinkOpts);
                                    <<"link:", Path/binary>> ->
                                        {link, Path, LinkOpts};
                                    ImmediateValue when is_binary(ImmediateValue) ->
                                        apply_type_to_immediate(ImmediateValue, LinkOpts);
                                    _ ->
                                        {link, SubkeyPath, LinkOpts}
                                end,
                            {true, {Subpath, PreparedValue}};
                        true ->
                            % The key is an encoded link, so we create a resolvable
                            % link to the underlying link. This requires that we
                            % dereference the link twice in order to get the final
                            % value. Returning the data this way avoids having to
                            % read each of the link keys themselves, which may be
                            % a large quantity.
                            {true,
                                {
                                    binary:part(Subpath, 0, byte_size(Subpath) - 5),
                                    {link, SubkeyPath, #{
                                        <<"type">> => <<"link">>,
                                        <<"lazy">> => true
                                    }}
                                }
                            }
                    end
                end,
            Subpaths
        )),
    Merged = maps:merge(Res, Implicit),
    % Convert the message to an ordered list if the ao-types indicate that it
    % should be so.
    case hb_maps:get(<<".">>, Types, undefined, Opts) of
        <<"list">> ->
            hb_util:message_to_ordered_list(Merged, Opts);
        _ ->
            case hb_opts:get(lazy_loading, true, Opts) of
                true -> Merged;
                false -> ensure_all_loaded(Merged, Opts)
            end
    end.

apply_type_to_immediate(Value, #{ <<"type">> := Type }) ->
    hb_util:decode(Type, Value);
apply_type_to_immediate(Value, _LinkOpts) ->
    Value.

%% @doc Read and parse the ao-types for a given path if it is in the supplied
%% list of subpaths, returning a map of keys and their types. Prefix reads
%% already return small inline `ao-types' values, so reuse them when present.
read_ao_types(Path, Subpaths, Values, Store, Opts) ->
    ?event_debug({reading_ao_types, {path, Path}, {subpaths, {explicit, Subpaths}}}),
    case lists:member(<<"ao-types">>, Subpaths) of
        true ->
            TypesPath = hb_path:to_binary([Path, <<"ao-types">>]),
            TypesBin =
                case Values of
                    #{ <<"ao-types">> := <<"raw:", Direct/binary>> } -> Direct;
                    #{ <<"ao-types">> := <<"link:", _/binary>> } ->
                        {ok, Bin} = hb_store:read(Store, TypesPath, Opts),
                        decode_immediate_value(TypesPath, Bin);
                    #{ <<"ao-types">> := Direct } when is_binary(Direct) ->
                        Direct;
                    _ ->
                        {ok, Bin} = hb_store:read(Store, TypesPath, Opts),
                        decode_immediate_value(TypesPath, Bin)
                end,
            Types = structured_decode_types(TypesBin, Opts),
            ?event_debug({parsed_ao_types, {types, Types}}),
            {ok, types_to_implicit(Types), Types};
        false ->
            ?event_debug({no_ao_types_key_found, {path, Path}, {subpaths, Subpaths}}),
            {ok, #{}, #{}}
    end.

%% @doc Convert a map of ao-types to an implicit map of types.
types_to_implicit(Types) ->
    maps:filtermap(
        fun(_K, <<"empty-message">>) -> {true, #{}};
           (_K, <<"empty-list">>) -> {true, []};
           (_K, <<"empty-binary">>) -> {true, <<>>};
           (_, _) -> false
        end,
        Types
    ).

%% @doc Decode the `ao-types' field through the structured device.
structured_decode_types(Types, Opts) ->
    hb_util:ok(
        hb_ao:raw(
            <<"structured@1.0">>,
            <<"decode-types">>,
            Types,
            #{},
            Opts
        )
    ).


%% @doc Read the result of a computation, using heuristics. The supported
%% heuristics are as follows:
%% 1. If the base message is an ID, we try to determine if the message has an
%% explicit device. If it does not, we can simply read the key and return it if
%% it exists, as this is the behavior of `message@1.0'.
%% 2. If the base message is loaded (a map), we determine if it has an explicit,
%% non-direct data access device. If it does, we simply read the key from the
%% message and return it if it exists.
%% 3. If the message has an explicit device, we attempt to read the hashpath to
%% see if it has already been computed.
read_resolved(BaseMsg, Key, Opts) when is_binary(Key) ->
    read_resolved(BaseMsg, #{ <<"path">> => Key }, Opts);
read_resolved({link, ID, LinkOpts}, Req, Opts) ->
    read_resolved(ID, Req, maps:merge(LinkOpts, Opts));
read_resolved(BaseMsgID, Req = #{ <<"path">> := Key }, Opts) when ?IS_ID(BaseMsgID) ->
    Store = hb_opts:get(store, no_viable_store, Opts),
    _NormKey = hb_ao:normalize_key(Key, Opts),
    case hb_device:is_direct_key_access(BaseMsgID, Req, Opts, Store) of
        unknown -> miss;
        false ->
            ?event_debug(read_cached,
                {found_non_message_device,
                    {key, _NormKey}
                }
            ),
            read_hashpath(BaseMsgID, Req, Opts);
        true ->
            % Either the message does not exist in the store, or there is no
            % explicit device in the message. If the message exists this implies
            % that the default (`message@1.0`) device will be used to execute
            % the key. Subsequently, we can simply read the key and return it if
            % it exists.
            ?event_debug(read_cached,
                {skipping_execution_store_lookup,
                    {base_msg, BaseMsgID},
                    {key, _NormKey}
                }
            ),
            case hb_store:resolve(Store, [BaseMsgID, Key], Opts) of
                {ok, KeyPath} -> hashpath_read_result(read(KeyPath, Opts));
                {error, not_found} -> miss;
                Other -> {hit, Other}
            end
    end;
read_resolved(BaseMsg, Req = #{ <<"path">> := Key }, Opts) when is_map(BaseMsg) ->
    % The base message is loaded, so we determine if it has an explicit device
    % and perform a direct lookup if it does not.
    NormKey = hb_ao:normalize_key(Key, Opts),
    case hb_device:is_direct_key_access(BaseMsg, Req, Opts) of
        false -> read_hashpath(BaseMsg, Req, Opts);
        true ->
            ?event_debug(read_cached,
                {skip_execution_memory_lookup,
                    {path, NormKey}
                }
            ),
            {hit, read_in_memory_key(BaseMsg, NormKey, Opts)}
    end;
read_resolved(Base, Req, Opts) ->
    read_hashpath(Base, Req, Opts).

%% @doc Return a key from an in-memory message, returning the same form as
%% a store read (`{Status, Value}').
read_in_memory_key(BaseMsg, NormKey, _Opts) ->
    % For now, just wrap maps:find.
    case maps:find(NormKey, BaseMsg) of
        error ->
            ?event_debug(read_cached, {key_not_found, {key, NormKey}}),
            {error, not_found};
        {ok, Value} ->
            ?event_debug(read_cached, {key_found, {key, NormKey}}),
            {ok, Value}
    end.

%% @doc Read the output of a prior computation, given BaseMsg and Req.
read_hashpath(BaseMsgID, ReqID, Opts) when ?IS_ID(BaseMsgID) and ?IS_ID(ReqID) ->
    ?event_debug({cache_lookup, {base, BaseMsgID}, {req, ReqID}, {opts, Opts}}),
    hashpath_read_result(read(<<BaseMsgID/binary, "/", ReqID/binary>>, Opts));
read_hashpath(BaseMsgID, Req, Opts) when ?IS_ID(BaseMsgID) and is_map(Req) ->
    ReqID = hb_message:id(Req, all, Opts),
    hashpath_read_result(read(<<BaseMsgID/binary, "/", ReqID/binary>>, Opts));
read_hashpath(BaseMsg, Req, Opts) when is_map(BaseMsg) and is_map(Req) ->
    hashpath_read_result(read(hb_path:hashpath(BaseMsg, Req, Opts), Opts));
read_hashpath(_, _, _) -> miss.

hashpath_read_result({ok, Msg}) -> {hit, {ok, Msg}};
hashpath_read_result({error, not_found}) -> miss;
hashpath_read_result(Other) -> {hit, Other}.

%% @doc Make a link from one path to another in the store.
%% Note: Argument order is `link(Src, Dst, Opts)'.
link(Existing, New, Opts) ->
    hb_store:link(
        hb_opts:get(store, no_viable_store, Opts),
        #{ hb_path:to_binary(New) => hb_path:to_binary(Existing) },
        Opts
    ).

%%% Tests

test_unsigned(Data) ->
    #{
        <<"base-test-key">> => <<"base-test-value">>,
        <<"other-test-key">> => Data
    }.

%% Helper function to create signed #tx items.
test_signed(Data) -> test_signed(Data, #{ <<"priv-wallet">> => ar_wallet:new() }).
test_signed(Data, Opts) ->
    hb_message:commit(test_unsigned(Data), Opts).

test_store_binary(Store) ->
    Bin = <<"Simple unsigned data item">>,
    ?event_debug(debug_store_test, {store, Store}),
    Opts = #{ <<"store">> => Store },
    {ok, ID} = write(Bin, Opts),
    {ok, RetrievedBin} = read(ID, Opts),
    ?assertEqual(Bin, RetrievedBin).

test_store_unsigned_empty_message(Store) ->
    ?event_debug(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Item = #{},
    Opts = #{ <<"store">> => Store },
    {ok, Path} = write(Item, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?event(
        {retrieved_item,
            {path, {string, Path}},
            {expected, Item},
            {got, RetrievedItem}
        }
    ),
    MatchRes = hb_message:match(Item, RetrievedItem, strict, Opts),
    ?event_debug({match_result, MatchRes}),
    ?assert(MatchRes).

test_store_unsigned_nested_empty_message(Store) ->
    ?event_debug(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Item =
        #{ <<"layer1">> =>
            #{ <<"layer2">> =>
                #{ <<"layer3">> =>
                    #{ <<"a">> => <<"b">>}
                },
                <<"layer3b">> => #{ <<"c">> => <<"d">>},
                <<"layer3c">> => #{}
            }
        },
    Opts = #{ <<"store">> => Store },
    {ok, Path} = write(Item, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Item, RetrievedItem, strict, Opts)).

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_unsigned_message(Store) ->
    Item = test_unsigned(<<"Simple unsigned data item">>),
    ?event_debug(debug_store_test, {store, Store}),
    Opts = #{ <<"store">> => Store },
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    %% Read the item back
    ID = hb_util:human_id(hb_ao:get(id, Item)),
    {ok, RetrievedItem} = read(ID, Opts),
    ?assert(hb_message:match(Item, RetrievedItem, strict, Opts)),
    ok.

test_store_ans104_message(Store) ->
    ?event_debug(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    Item = #{ <<"type">> => <<"ANS104">>, <<"content">> => <<"Hello, world!">> },
    Committed = hb_message:commit(Item, #{ <<"priv-wallet">> => hb:wallet() }),
    {ok, _Path} = write(Committed, Opts),
    CommittedID = hb_util:human_id(hb_message:id(Committed, all)),
    UncommittedID = hb_util:human_id(hb_message:id(Committed, none)),
    ?event_debug({test_message_ids, {uncommitted, UncommittedID}, {committed, CommittedID}}),
    {ok, RetrievedItem} = read(CommittedID, Opts),
    {ok, RetrievedItemU} = read(UncommittedID, Opts),
    ?assert(hb_message:match(Committed, RetrievedItem, strict, Opts)),
    ?assert(hb_message:match(Committed, RetrievedItemU, strict, Opts)),
    ok.

%% @doc Test storing and retrieving a simple unsigned item
test_store_simple_signed_message(Store) ->
    ?event_debug(debug_store_test, {store, Store}),
    Opts = #{ <<"store">> => Store },
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Address = hb_util:human_id(ar_wallet:to_address(Wallet)),
    Item = test_signed(<<"Simple signed data item">>, #{ <<"priv-wallet">> => Wallet }),
    ?event_debug({writing_test_message, Item}),
    %% Write the simple unsigned item
    {ok, _Path} = write(Item, Opts),
    % %% Read the item back
    % UID = hb_message:id(Item, none, Opts),
    % {ok, RetrievedItemUnsig} = read(UID, Opts),
    % ?event_debug({retreived_unsigned_message, {expected, Item}, {got, RetrievedItemUnsig}}),
    % MatchRes = hb_message:match(Item, RetrievedItemUnsig, strict, Opts),
    % ?event_debug({match_result, MatchRes}),
    % ?assert(MatchRes),
    CommittedID = hb_message:id(Item, [Address], Opts),
    {ok, RetrievedItemSigned} = read(CommittedID, Opts),
    ?event_debug({retrieved_signed_message, {expected, Item}, {got, RetrievedItemSigned}}),
    MatchResSigned = 
        hb_message:match(
            Item,
            hb_message:normalize_commitments(RetrievedItemSigned, Opts),
            strict,
            Opts
        ),
    ?event_debug({match_result_signed, MatchResSigned}),
    ?assert(MatchResSigned),
    ok.

%% @doc Test deeply nested item storage and retrieval
test_deeply_nested_complex_message(Store) ->
    ?event_debug(debug_store_test, {store, Store}),
    hb_store:reset(Store),
    Wallet = ar_wallet:new(),
    Opts = #{ <<"store">> => Store, <<"priv-wallet">> => Wallet },
    %% Create nested data
    Level3SignedSubmessage = test_signed([1,2,3], Opts),
    Outer =
        hb_message:commit(
            #{
                <<"level1">> =>
                    InnerSigned = hb_message:commit(
                        #{
                            <<"level2">> =>
                                #{
                                    <<"level3">> => Level3SignedSubmessage,
                                    <<"e">> => <<"f">>,
                                    <<"z">> => [1,2,3]
                                },
                            <<"c">> => <<"d">>,
                            <<"g">> => [<<"h">>, <<"i">>],
                            <<"j">> => 1337
                        },
                        Opts
                    ),
                <<"a">> => <<"b">>
            },
            Opts
        ),
    UID = hb_message:id(Outer, none, Opts),
    ?event_debug({string, <<"================================================">>}),
    CommittedID = hb_message:id(Outer, signed, Opts),
    ?event_debug({string, <<"================================================">>}),
    ?event_debug({test_message_ids, {uncommitted, UID}, {committed, CommittedID}}),
    %% Write the nested item
    {ok, _} = write(Outer, Opts),
    %% Read the deep value back using subpath
	OuterID = hb_util:human_id(UID),
    {ok, OuterMsg} = read(OuterID, Opts),
	EnsuredLoadedOuter = hb_cache:ensure_all_loaded(OuterMsg, Opts),
    ?event_debug({deep_message, {explicit, EnsuredLoadedOuter}}),
    %% Assert that the retrieved item matches the original deep value
    ?assertEqual(
        [1,2,3],
        hb_ao:get(
            <<"level1/level2/level3/other-test-key">>,
            EnsuredLoadedOuter,
            Opts
        )
    ),
    ?event(
        {deep_message_match,
            {read, EnsuredLoadedOuter},
            {write, Level3SignedSubmessage}
        }
    ),
    ?event_debug({reading_committed_outer, {id, CommittedID}, {expect, Outer}}),
    {ok, CommittedMsg} = read(hb_util:human_id(CommittedID), Opts),
	EnsuredLoadedCommitted = hb_cache:ensure_all_loaded(CommittedMsg, Opts),
	?assertEqual(
        [1,2,3],
        hb_ao:get(
            <<"level1/level2/level3/other-test-key">>,
            EnsuredLoadedCommitted,
            Opts
        )
    ).

test_message_with_list(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    Msg = test_unsigned([<<"a">>, <<"b">>, <<"c">>]),
    ?event_debug({writing_message, Msg}),
    {ok, Path} = write(Msg, Opts),
    {ok, RetrievedItem} = read(Path, Opts),
    ?assert(hb_message:match(Msg, RetrievedItem, strict, Opts)).

test_match_message(Store) when map_get(<<"store-module">>, Store) =/= hb_store_lmdb ->
    skip;
test_match_message(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    % Write two messages that match the template, and a third that does not.
    {ok, ID1} = hb_cache:write(#{ <<"x">> => <<"1">> }, Opts),
    {ok, ID2} = hb_cache:write(#{ <<"y">> => <<"2">>, <<"z">> => <<"3">> }, Opts),
    {ok, ID2b} = hb_cache:write(#{ <<"x">> => <<"4">>, <<"z">> => <<"3">> }, Opts),
    {ok, ID3} = hb_cache:write(#{ <<"z">> => <<"5">>, <<"c">> => <<"d">> }, Opts),
    % Match the template, and ensure that we get two matches.
    {ok, MatchedItems} = match(#{ <<"z">> => <<"3">> }, Opts),
    ?assertEqual(2, length(MatchedItems)),
    ?assert(
        lists:all(
            fun(ID) ->
                {ok, Msg} = read(ID, Opts),
                hb_maps:get(<<"z">>, Msg, Opts) =:= <<"3">> andalso
                    lists:member(ID, [ID2, ID2b])
            end,
            MatchedItems
        )
    ),
    {ok, MatchedItems2} = match(#{ <<"x">> => <<"4">> }, Opts),
    ?assertEqual(1, length(MatchedItems2)),
    ?assertEqual([ID2b], MatchedItems2).

test_match_linked_message(Store) when map_get(<<"store-module">>, Store) =/= hb_store_lmdb ->
    skip;
test_match_linked_message(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    Msg = #{ <<"a">> => Inner = #{ <<"b">> => <<"c">>, <<"d">> => <<"e">> } },
    {ok, _ID} = write(Msg, Opts),
    {ok, [MatchedID]} = match(#{ <<"b">> => <<"c">> }, Opts),
    {ok, Read1} = read(MatchedID, Opts),
    ?assertEqual(
        #{ <<"b">> => <<"c">>, <<"d">> => <<"e">> },
        hb_cache:ensure_all_loaded(Read1, Opts)
    ),
    {ok, [MatchedID2]} = match(#{ <<"a">> => Inner }, Opts),
    {ok, Read2} = read(MatchedID2, Opts),
    ?assertEqual(
        #{ <<"a">> => Inner },
        ensure_all_loaded(Read2, Opts)
    ).

test_match_typed_message(Store) when map_get(<<"store-module">>, Store) =/= hb_store_lmdb ->
    skip;
test_match_typed_message(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    % Add some messages that should not match the template, as well as the main
    % message that should match the template.
    write(#{ <<"atom-value">> => atom, <<"wrong">> => <<"wrong">> }, Opts),
    write(#{ <<"integer-value">> => 1337, <<"wrong">> => <<"wrong-2">> }, Opts),
    Msg =
        #{
            <<"int-key">> => 1337,
            <<"other-key">> => <<"other-test-value">>,
            <<"atom-key">> => atom
        },
    {ok, _ID} = write(Msg, Opts),
    {ok, [MatchedID]} = match(#{ <<"int-key">> => 1337 }, Opts),
    {ok, Read1} = read(MatchedID, Opts),
    ?assertEqual(
        Msg,
        ensure_all_loaded(Read1, Opts)
    ),
    {ok, [MatchedID2]} = match(#{ <<"atom-key">> => atom }, Opts),
    {ok, Read2} = read(MatchedID2, Opts),
    ?assertEqual(
        Msg,
        ensure_all_loaded(Read2, Opts)
    ).

test_raw_match_read(Store) when map_get(<<"store-module">>, Store) =/= hb_store_lmdb ->
    skip;
test_raw_match_read(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    Msg = #{
        <<"content-type">> => <<"application/beam">>,
        <<"implements-device">> => <<"test-spec">>,
        <<"body">> => <<"beam-bytes">>
    },
    {ok, ID} = write(Msg, Opts),
    RawOpts = Opts#{
        <<"cache-read-mode">> => raw,
        <<"match-index">> => [Store]
    },
    ?assertEqual(
        {ok, [ID]},
        match(#{ <<"implements-device">> => <<"test-spec">> }, RawOpts)
    ),
    {ok, RawMsg} = read(ID, RawOpts),
    ?assertEqual(
        <<"beam-bytes">>,
        hb_maps:get(<<"body">>, RawMsg, undefined, RawOpts)
    ).

test_immediate_marker_values(Store) ->
    hb_store:reset(Store),
    Opts = #{ <<"store">> => Store },
    Msg = #{
        <<"groupish">> => <<"group">>,
        <<"linkish">> => <<"link:literal">>,
        <<"rawish">> => <<"raw:literal">>
    },
    {ok, ID} = write(Msg, Opts),
    {ok, Read} = read(ID, Opts),
    ?assertEqual(<<"group">>, hb_maps:get(<<"groupish">>, Read, Opts)),
    ?assertEqual(<<"link:literal">>, hb_maps:get(<<"linkish">>, Read, Opts)),
    ?assertEqual(<<"raw:literal">>, hb_maps:get(<<"rawish">>, Read, Opts)),
    case map_get(<<"store-module">>, Store) of
        hb_store_lmdb ->
            ?assertEqual({ok, [ID]}, match(#{ <<"rawish">> => <<"raw:literal">> }, Opts));
        _ ->
            ok
    end.

match_store_control_test() ->
    Store = hb_test_utils:test_store(hb_store_volatile, <<"match-control">>),
    ?assertEqual([Store], match_store(#{ <<"store">> => Store })),
    ?assertEqual([], match_store(#{ <<"store">> => Store, <<"match-index">> => false })),
    ?assertEqual([], match_store(#{ <<"store">> => Store, <<"match-index">> => [] })).

cache_suite_test_() ->
    hb_store:generate_test_suite([
        {"store unsigned empty message",
            fun test_store_unsigned_empty_message/1},
        {"store binary", fun test_store_binary/1},
        {"store unsigned nested empty message",
            fun test_store_unsigned_nested_empty_message/1},
        {"store simple unsigned message", fun test_store_simple_unsigned_message/1},
        {"store simple signed message", fun test_store_simple_signed_message/1},
        {"deeply nested complex message", fun test_deeply_nested_complex_message/1},
        {"message with list", fun test_message_with_list/1},
        {"match message", fun test_match_message/1},
        {"match linked message", fun test_match_linked_message/1},
        {"match typed message", fun test_match_typed_message/1},
        {"raw match read", fun test_raw_match_read/1},
        {"immediate marker values", fun test_immediate_marker_values/1}
    ]).

%% @doc Test that message whose device is `#{}' cannot be written. If it were to
%% be written, it would cause an infinite loop.
test_device_map_cannot_be_written_test() ->
    try
        Opts = #{ <<"store">> => StoreOpts =
            [#{ <<"store-module">> => hb_store_fs, <<"name">> => <<"cache-TEST">> }] },
        hb_store:reset(StoreOpts),
        Danger = #{ <<"device">> => #{}},
        write(Danger, Opts),
        ?assert(false)
    catch
        _:_:_ -> ?assert(true)
    end.

%% @doc Cache writes are best-effort with respect to the configured store
%% list: a list whose only entry rejects write-class operations (e.g. a
%% read-only store) must still yield `{ok, ID}', not crash. Exercises both
%% the binary path (`hb_store:write') and the map path (`hb_store:group'
%% plus `hb_store:link') so every store side-effect in `do_write_message'
%% is covered.
write_with_only_read_only_store_test() ->
    ReadOnlyStore = #{
        <<"store-module">> => hb_store_volatile,
        <<"name">> => <<"cache-readonly">>,
        <<"access">> => [<<"read">>]
    },
    Opts = #{ <<"store">> => [ReadOnlyStore] },
    ?assertMatch({ok, _}, write(<<"some-binary-payload">>, Opts)),
    ?assertMatch({ok, _}, write(#{ <<"hello">> => <<"world">> }, Opts)).

%% @doc Run a specific test with a given store module.
run_test() ->
    Store = hb_test_utils:test_store(),
    test_match_typed_message(Store).
