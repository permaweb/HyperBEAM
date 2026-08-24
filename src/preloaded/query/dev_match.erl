%%% @doc A reverse index for finding all message IDs with a given key-value pair.
%%%
%%% Under `match-hash-size' the index is a sorted set of rows, each a hash of
%%% one key-value pair followed by the weave offset of an item carrying it.
%%% Every predicate's rows therefore share one ordering, and `all/3' intersects
%%% them by walking in step rather than by reading either in full.
%%%
%%% The walk asks the store for the first row of a predicate at or after an
%%% offset, so the store has to order rows by offset. The store definition of
%%% the design normalizes the offset to a fixed-width big-endian binary, which
%%% orders numerically; a store holding the offsets as decimal text would order
%%% `100' before `99' and answer wrongly.
-module(dev_match).
-export([info/0, all/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% The spans tried when reading the header of the item that begins at an
%% offset, in the order they are tried. A signature, an owner, a target, an
%% anchor and the two tag counts come to 1,108 bytes, and the tags follow, so
%% two kilobytes covers the ordinary item; the wider spans are for a heavily
%% tagged one and the narrower for an item that ends before the wider spans
%% reach.
-define(DEFAULT_VERIFY_SPANS, [2048, 4096, 1536, 1152, 8192]).

%% The largest offset a row can carry. The offset is seven bytes wide in the
%% index, which reaches 72 petabytes -- far past any weave this will index.
-define(MAX_OFFSET, (1 bsl 56) - 1).

%% @doc Default all non-message@1.0 and device keys to match a single key in the
%% index.
info() ->
    #{
        excludes => [<<"set">>, <<"remove">>, <<"id">>, <<"verify">>],
        default => fun match/4
    }.

%% @doc Get the store configured for the match index.
store(Opts) ->
    LocalMatchIndex = maps:get(<<"match-index">>, Opts, undefined),
    LocalStore = maps:get(<<"store">>, Opts, undefined),
    GlobalMatchIndex = hb_opts:get(match_index, false, #{ <<"only">> => global }),
    MatchIndexStore =
        case {LocalMatchIndex, LocalStore} of
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
        ResolvedStore when not is_list(ResolvedStore) -> [ResolvedStore];
        ResolvedStore -> ResolvedStore
    end.

%% @doc Match a single key-value pair in the index, returning the rows the
%% index holds for it. `hb_cache:match_address/3' decides how a pair is
%% addressed, so that the writer and this reader cannot drift apart.
match(Key, Base, _Req, Opts) -> match(Key, Base, Opts).
match(Key, Base, Opts) ->
    Store = store(Opts),
    {ok, Value} = hb_maps:find(Key, Base, Opts),
    case hb_store:list(
        Store,
        hb_cache:match_address(hb_ao:normalize_key(Key), Value, Opts),
        Opts
    ) of
        {ok, Messages} -> {ok, Messages};
        _ -> {error, not_found}
    end.

%% @doc Match the full base message against the index, returning the messages
%% that carry every key-value pair of it.
%%
%% The request may carry `from' -- a weave offset to continue from, which is
%% what a page cursor is -- along with `limit' and `direction'. Continuing is a
%% seek rather than a scan, so the fiftieth page costs what the first does.
all(Base, Req, Opts) ->
    IndexBase = hb_message:uncommitted(hb_private:reset(Base)),
    case hb_opts:get(match_hash_size, false, Opts) of
        false -> intersect(IndexBase, Opts);
        _ -> leapfrog(IndexBase, Req, Opts)
    end.

%% @doc Intersect the predicates of a template by walking their rows in step.
%%
%% Each predicate is asked for its first row at or after a cursor. One that
%% answers with a later row moves the cursor and the walk starts again from the
%% first predicate; when every predicate answers with the cursor itself, that
%% offset carries all of them. No predicate is ever read past the candidates it
%% rejects, so a template whose second pair is selective costs about what its
%% first pair alone would -- rather than the sum of both read in full, which is
%% what intersecting two materialized lists costs.
leapfrog(Base, Req, Opts) ->
    hb_prometheus:declare(
        counter,
        [
            {name, hb_match_index_seeks},
            {labels, []},
            {help, "Seeks made into the match index while intersecting"}
        ]
    ),
    Store = store(Opts),
    Addresses =
        [
            hb_cache:match_address(hb_ao:normalize_key(Key), Value, Opts)
        ||
            {Key, Value} <- hb_maps:to_list(Base, Opts)
        ],
    case Addresses of
        [] ->
            {ok, []};
        _ ->
            Step = step_of(hb_maps:get(<<"direction">>, Req, forward, Opts)),
            Offsets =
                walk(
                    Store,
                    Addresses,
                    hb_util:int(hb_maps:get(<<"from">>, Req, first(Step), Opts)),
                    bound(hb_maps:get(<<"limit">>, Req, unbounded, Opts)),
                    Step,
                    [],
                    Opts
                ),
            verified(Base, Offsets, Opts)
    end.

%% A backward walk runs from high offsets to low, which is the order a query
%% asking for the newest messages first wants.
step_of(Direction) ->
    case hb_util:atom(Direction) of
        backward -> -1;
        forward -> 1
    end.

%% Where a walk starts when the request names no offset: the genesis of the
%% weave going forward, and past its end going backward.
first(1) -> 0;
first(-1) -> ?MAX_OFFSET.

bound(unbounded) -> unbounded;
bound(Limit) -> hb_util:int(Limit).

walk(_Store, _Addresses, _Cursor, 0, _Step, Found, _Opts) ->
    lists:reverse(Found);
walk(_Store, _Addresses, Cursor, _Limit, _Step, Found, _Opts) when Cursor < 0 ->
    lists:reverse(Found);
walk(Store, Addresses, Cursor, Limit, Step, Found, Opts) ->
    case step(Store, Addresses, Cursor, Step, Opts) of
        exhausted ->
            lists:reverse(Found);
        {advance, Next} ->
            walk(Store, Addresses, Next, Limit, Step, Found, Opts);
        carried ->
            walk(
                Store, Addresses, Cursor + Step, decrement(Limit), Step,
                [Cursor | Found], Opts
            )
    end.

decrement(unbounded) -> unbounded;
decrement(Limit) -> Limit - 1.

%% Ask each predicate in turn whether the cursor is one of its rows, stopping
%% at the first that answers with a row further along the walk.
step(_Store, [], _Cursor, _Step, _Opts) ->
    carried;
step(Store, [Address | Rest], Cursor, Step, Opts) ->
    case first_from(Store, Address, Cursor, Step, Opts) of
        exhausted -> exhausted;
        Cursor -> step(Store, Rest, Cursor, Step, Opts);
        Next -> {advance, Next}
    end.

%% @doc The offset of the predicate's first row at or after the given one going
%% forward, or at or before it going backward.
%%
%% This is the whole of the work a walk does in the index, so it is counted:
%% the number of seeks per result is what says whether two predicates are being
%% intersected or merely read.
first_from(Store, Address, Cursor, Step, Opts) ->
    hb_prometheus:inc(counter, hb_match_index_seeks, []),
    Request =
        #{
            <<"list">> => Address,
            <<"from">> => hb_util:bin(Cursor),
            <<"limit">> => 1,
            <<"direction">> => direction_of(Step)
        },
    case hb_store:list(Store, Request, Opts) of
        {ok, [Row]} -> hb_util:int(Row);
        _ -> exhausted
    end.

direction_of(1) -> forward;
direction_of(-1) -> backward.

%% @doc Read the item at each offset and keep the ones that really carry every
%% pair of the template, returning their IDs.
%%
%% A predicate is addressed by a truncated hash, so two of them can share an
%% address and interleave their rows. A reader that took a row's word for it
%% would answer wrongly; one that checks pays a read it was going to make
%% anyway, because the item is what the query returns. A row that does not
%% check out is dropped rather than raised: a collision is not an error.
%%
%% The reads are independent of each other, so they are made at once. Serially
%% a page of a hundred results is a hundred round trips.
verified(Base, Offsets, Opts) ->
    Checked =
        hb_pmap:parallel_map(
            Offsets,
            fun(Offset) -> carries(Offset, Base, Opts) end,
            hb_opts:get(match_verify_workers, 32, Opts)
        ),
    {ok, [ID || {ok, ID} <- Checked]}.

%% @doc Read the item that begins at an offset and report its ID if it carries
%% every pair of the template.
%%
%% Only the item's header is needed -- its tags are what the template is
%% checked against, and its ID is the hash of its signature -- so the read is
%% bounded by `match-verify-span' rather than by the item's own size, which the
%% index does not record.
carries(0, _Base, _Opts) ->
    % Offset zero is what a row carries when the item's position is not known,
    % so there is nothing at it to check.
    skip;
carries(Offset, Base, Opts) ->
    case header(Offset, Opts) of
        {ok, Item} -> checked(Item, Base, Opts);
        % The bytes at the offset are not an item this reader can parse: a
        % transaction whose header sits outside the data tree, or a row whose
        % predicate collided with another's. Neither is an error.
        {error, _} -> skip
    end.

%% @doc Read the item whose header begins at an offset.
%%
%% The index records where an item begins and not how far it runs, so the span
%% is found rather than known. A range of the weave is served only where it is
%% whole, so a span reaching past the end of the data its transaction holds
%% fails outright rather than returning what there is; and a span stopping
%% inside the item's tags parses into fewer tags than the item has, which would
%% lose a match rather than report one. The tags ending inside the read is what
%% says a span was wide enough, and the spans are tried widest-likeliest first.
header(Offset, Opts) ->
    header(Offset, hb_opts:get(match_verify_spans, ?DEFAULT_VERIFY_SPANS, Opts), Opts).
header(_Offset, [], _Opts) ->
    {error, not_found};
header(Offset, [Span | Rest], Opts) ->
    case hb_store_arweave:read_chunks(Offset, hb_util:int(Span), Opts) of
        {ok, Bytes} ->
            case whole(Bytes) of
                {ok, Item} -> {ok, Item};
                truncated -> header(Offset, Rest, Opts)
            end;
        _ ->
            header(Offset, Rest, Opts)
    end.

%% The item's data is dropped: the read covers its header and whatever of its
%% data happened to follow, and the codec would try to unbundle bytes that are
%% not the whole of anything.
whole(Bytes) ->
    try ar_bundles:deserialize_header(Bytes) of
        {ok, HeaderSize, Item} when HeaderSize < byte_size(Bytes) ->
            {ok, ar_tx:reset_ids(Item#tx{ data = <<>>, data_size = 0 })};
        _ ->
            truncated
    catch _:_:_ ->
        truncated
    end.

%% The item's tags, target, anchor and committer are what a template names,
%% and are all that a header carries.
checked(Item, Base, Opts) ->
    Fields =
        hb_message:uncommitted(
            hb_message:convert(
                Item,
                <<"structured@1.0">>,
                <<"ans104@1.0">>,
                Opts
            ),
            Opts
        ),
    Matches =
        hb_maps:fold(
            fun(_Key, _Value, false) -> false;
               (Key, Value, true) ->
                    hb_maps:get(hb_ao:normalize_key(Key), Fields, not_found, Opts)
                        == Value
            end,
            true,
            Base,
            Opts
        ),
    case Matches of
        true -> {ok, hb_util:encode(ar_bundles:id(Item, signed))};
        false -> skip
    end.

%% @doc Intersect the predicates of a template by reading each in full. The
%% rows of the index this reads name messages rather than positions, so they
%% carry no ordering that two predicates share and there is nothing to walk.
intersect(IndexBase, Opts) ->
    case hb_maps:keys(IndexBase) of
        [] -> {ok, []};
        [FirstKey | Rest] ->
            case match(FirstKey, IndexBase, Opts) of
                {ok, FirstMatches} ->
                    lists:foldl(
                        fun(Key, {ok, Acc}) ->
                            case match(Key, IndexBase, Opts) of
                                {ok, Matches} ->
                                    {ok, hb_util:list_with(Acc, Matches)};
                                _ ->
                                    {error, not_found}
                            end;
                           (_Key, Error) ->
                                Error
                        end,
                        {ok, FirstMatches},
                        Rest
                    );
                _ ->
                    {error, not_found}
            end
    end.

%%% Tests

%% Six ANS-104 items in block 1,889,322 of the weave, each tagged
%% `App-Name: ArDrive-App'; three of them are also tagged `Entity-Type: file',
%% and one of those carries a `Cipher' its siblings do not. Their offsets and
%% IDs are what the weave holds, so a walk that reaches them and a reader that
%% checks them are both being held to the real thing.
-define(WEAVE_ITEMS,
    [
        {386310990766550, <<"zvFNNmZwXxeznEjO5fHc6D7_bJWyTEmrSQWcKw_Z0wQ">>},
        {386310990767812, <<"SD9obWV59R7JZuLIqzEztuaWaJ7FGY8N9XRy0JXwDGc">>},
        {386310990769443, <<"Mx-GlwBslqsd-OkXGY84PxBzN_dhCPva_XANecNXKPs">>},
        {386310990770705, <<"npAzk_BomjWBQQr_xnmlhdxjyl97EJnNv_MAaXffs1s">>},
        {386310990772336, <<"SyLRPOOdz4MrJEupDwhOh8zYagCLoJuWF1RYxRr85X4">>},
        {386310990773598, <<"Vlw8xwVZRl-GulRjelEpOZm9xJowjluKOmVRtFQmIjE">>}
    ]).

%% The index the walk reads, and the node options that reach the weave.
test_opts() ->
    Content =
        #{
            <<"store-module">> => hb_store_lmdb,
            <<"name">> =>
                <<"cache-TEST/match-content-",
                    (hb_util:bin(erlang:unique_integer([positive])))/binary>>
        },
    Index =
        #{
            <<"store-module">> => hb_store_lmdb,
            <<"name">> =>
                <<"cache-TEST/match-index-",
                    (hb_util:bin(erlang:unique_integer([positive])))/binary>>,
            <<"sorted-set">> => true,
            <<"prefix">> => <<"~match@1.0/">>,
            <<"path-normalization">> =>
                [<<"decode-base64url">>, <<"decode-int-56">>],
            <<"strip-slashes">> => true
        },
    Opts =
        #{
            <<"store">> =>
                [
                    Content,
                    #{
                        <<"store-module">> => hb_store_arweave,
                        <<"name">> => <<"cache-arweave">>,
                        <<"index-store">> => [Content],
                        <<"arweave-node">> => <<"https://arweave.net">>
                    }
                ],
            <<"match-index">> => [Index],
            <<"match-hash-size">> => 8,
            <<"match-offsets">> => <<"lookup">>
        },
    ok = hb_store:reset(Index, #{}),
    {Index, Opts}.

%% Write the rows of one predicate at the given offsets.
test_rows(Index, Key, Value, Offsets, Opts) ->
    Address = hb_cache:match_address(Key, Value, Opts),
    ok =
        hb_store:write(
            [Index],
            hb_maps:from_list(
                [
                    {<<Address/binary, "/", (hb_util:bin(Offset))/binary>>, <<>>}
                ||
                    Offset <- Offsets
                ]
            ),
            Opts
        ).

%% The seeks the index has answered, which is the work a walk does in it.
index_seeks() ->
    case catch prometheus_counter:value(hb_match_index_seeks, []) of
        Value when is_number(Value) -> Value;
        _ -> 0
    end.

%% @doc A template of several predicates is answered by walking their rows in
%% step, and every result is checked against the item it names.
%%
%% Each predicate is written at the offsets of the items that really carry it,
%% so a walk that reaches the right offsets and a reader that reads the right
%% items are both being held to what the weave holds.
leapfrog_test_() ->
    {timeout, 300, fun walks_in_step/0}.
walks_in_step() ->
    {Index, Opts} = test_opts(),
    All = [Offset || {Offset, _ID} <- ?WEAVE_ITEMS],
    Files = [386310990767812, 386310990770705, 386310990773598],
    Ciphered = [386310990767812],
    test_rows(Index, <<"app-name">>, <<"ArDrive-App">>, All, Opts),
    test_rows(Index, <<"entity-type">>, <<"file">>, Files, Opts),
    test_rows(Index, <<"cipher">>, <<"AES256-GCM">>, Ciphered, Opts),
    IDs = hb_maps:from_list([{Offset, ID} || {Offset, ID} <- ?WEAVE_ITEMS]),
    Expected = fun(Offsets) -> [hb_maps:get(O, IDs, Opts) || O <- Offsets] end,
    ?assertEqual(
        {ok, Expected(All)},
        hb_cache:match(#{ <<"app-name">> => <<"ArDrive-App">> }, Opts)
    ),
    Two = #{ <<"app-name">> => <<"ArDrive-App">>, <<"entity-type">> => <<"file">> },
    Before = index_seeks(),
    ?assertEqual({ok, Expected(Files)}, hb_cache:match(Two, Opts)),
    TwoSeeks = index_seeks() - Before,
    Three = Two#{ <<"cipher">> => <<"AES256-GCM">> },
    ?assertEqual({ok, Expected(Ciphered)}, hb_cache:match(Three, Opts)),
    % A third predicate that rules almost everything out costs no more seeks
    % than the two it is added to: the walk is stopped by whichever predicate
    % runs out first rather than by how many there are. Intersecting three
    % materialized lists would cost the sum of all three.
    ThreeBefore = index_seeks(),
    ?assertEqual({ok, Expected(Ciphered)}, hb_cache:match(Three, Opts)),
    ?assert(index_seeks() - ThreeBefore =< TwoSeeks).

%% @doc A page of results is a seek into the index rather than a scan up to it,
%% so continuing from a cursor costs what starting did.
leapfrog_pages_test_() ->
    {timeout, 300, fun pages_by_cursor/0}.
pages_by_cursor() ->
    {Index, Opts} = test_opts(),
    All = [Offset || {Offset, _ID} <- ?WEAVE_ITEMS],
    test_rows(Index, <<"app-name">>, <<"ArDrive-App">>, All, Opts),
    IDs = hb_maps:from_list([{Offset, ID} || {Offset, ID} <- ?WEAVE_ITEMS]),
    Template = #{ <<"app-name">> => <<"ArDrive-App">> },
    Page =
        fun(Bounds) ->
            {ok, Found} = hb_cache:match(Template, Bounds, Opts),
            Found
        end,
    ?assertEqual(
        [hb_maps:get(O, IDs, Opts) || O <- lists:sublist(All, 2)],
        Page(#{ <<"limit">> => 2 })
    ),
    % Continuing from the second offset takes the pages after it.
    ?assertEqual(
        [hb_maps:get(O, IDs, Opts) || O <- lists:sublist(All, 3, 2)],
        Page(#{ <<"from">> => hb_util:bin(lists:nth(2, All) + 1), <<"limit">> => 2 })
    ),
    % A later page costs no more seeks than the first: `from' is where the
    % cursor is put down, not how far it is walked.
    Before = index_seeks(),
    _ = Page(#{ <<"limit">> => 2 }),
    First = index_seeks() - Before,
    Middle = index_seeks(),
    _ = Page(#{ <<"from">> => hb_util:bin(lists:nth(4, All)), <<"limit">> => 2 }),
    ?assert(index_seeks() - Middle =< First),
    % Walking backward takes the newest first, which is what a query asking for
    % the most recent messages wants.
    ?assertEqual(
        [hb_maps:get(O, IDs, Opts) || O <- lists:reverse(lists:nthtail(4, All))],
        Page(#{ <<"limit">> => 2, <<"direction">> => backward })
    ).
