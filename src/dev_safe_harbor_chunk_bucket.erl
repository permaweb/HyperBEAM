%%% @doc Read-only MinIO-backed chunk reseed helper for safe harbor recovery.
-module(dev_safe_harbor_chunk_bucket).
-export([enabled/1, reseed/2]).

-include("include/hb.hrl").

-ifdef(EUNIT).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(DEFAULT_BUCKET_NAME, <<"ario">>).
-define(DEFAULT_BUCKET_REGION, <<"us-east-1">>).
-define(DEFAULT_NODE_POST_CONCURRENCY, 5).

enabled(Opts) ->
    lists:all(
        fun has_value/1,
        [
            bucket_endpoint(Opts),
            bucket_access_key(Opts),
            bucket_secret_key(Opts)
        ]
    ).

reseed(ID, Opts) ->
    RequestedID = hb_util:human_id(ID),
    log_stage(RequestedID, resolve_root_tx),
    case hb_gateway_client:root(RequestedID, Opts) of
        {ok, RootTXID} ->
            case fetch_root_tx_meta(RootTXID, Opts) of
                {ok, Meta} ->
                    do_reseed(RequestedID, Meta, Opts);
                {skip, _Stage, _Reason} = Skip ->
                    Skip
            end;
        {error, Reason} ->
            {skip, root_tx_resolve, Reason}
    end.

do_reseed(
    RequestedID,
    Meta = #{
        root_tx_id := RootTXID,
        data_root := DataRoot,
        data_size := DataSize
    },
    Opts
) ->
    log_stage(RequestedID, RootTXID, bucket_list),
    case list_bucket_keys(DataRoot, Opts) of
        {ok, []} ->
            {skip, bucket_list, #{ root_tx_id => RootTXID, data_root => DataRoot, reason => empty }};
        {ok, Keys} ->
            log_stage(RequestedID, RootTXID, tree_build),
            case build_tree(Keys, DataRoot, DataSize, Opts) of
                {ok, Root, Tree, ProofMeta} ->
                    NumberedProofMeta = number_proofs(ProofMeta, 1),
                    log_stage(RequestedID, RootTXID, tx_offset_fetch),
                    case fetch_tx_start_offset(RootTXID, Opts) of
                        {ok, StartOffset} ->
                            log_stage(RequestedID, RootTXID, proof_seed),
                            ?event(
                                safe_harbor,
                                {bucket_reseed_proofs,
                                    {id, RequestedID},
                                    {root_tx_id, RootTXID},
                                    {proofs, length(NumberedProofMeta)}
                                }
                            ),
                            case
                                seed_keys(
                                    RequestedID,
                                    RootTXID,
                                    Root,
                                    Tree,
                                    NumberedProofMeta,
                                    DataSize,
                                    StartOffset,
                                    Opts
                                )
                            of
                                {ok, ChunkPosts} ->
                                    {ok,
                                        Meta#{
                                            proofs => length(NumberedProofMeta),
                                            chunk_posts => ChunkPosts
                                        }};
                                {error, Stage, Reason} ->
                                    {skip, Stage, Reason}
                            end;
                        {error, Reason} ->
                            {skip,
                                tx_offset_fetch,
                                #{ root_tx_id => RootTXID, reason => Reason }}
                    end;
                {error, Stage, Reason} ->
                    {skip, Stage, Reason}
            end;
        {error, Reason} ->
            {skip,
                bucket_list,
                #{ root_tx_id => RootTXID, data_root => DataRoot, reason => Reason }}
    end.

fetch_root_tx_meta(RootTXID, Opts) ->
    log_stage(RootTXID, tx_header_fetch),
    case fetch_tx_header(RootTXID, Opts) of
        {ok, HeaderMsg} ->
            TX = hb_message:convert(
                HeaderMsg,
                <<"tx@1.0">>,
                <<"structured@1.0">>,
                Opts
            ),
            case {TX#tx.data_root, TX#tx.data_size} of
                {<<>>, _} ->
                    {skip, tx_header_fetch, #{ root_tx_id => RootTXID, reason => empty_data_root }};
                {_DataRoot, DataSize} when DataSize =< 0 ->
                    {skip, tx_header_fetch, #{ root_tx_id => RootTXID, reason => empty_data_size }};
                {DataRoot, DataSize} ->
                    {ok,
                        #{
                            root_tx_id => RootTXID,
                            data_root => hb_util:encode(DataRoot),
                            data_size => DataSize
                        }}
            end;
        {error, Reason} ->
            {skip, tx_header_fetch, #{ root_tx_id => RootTXID, reason => Reason }}
    end.

fetch_tx_header(ID, Opts) ->
    hb_ao:resolve(
        #{ <<"device">> => <<"arweave@2.9">> },
        #{
            <<"path">> => <<"tx">>,
            <<"tx">> => ID,
            <<"exclude-data">> => true
        },
        Opts
    ).

fetch_tx_start_offset(ID, _Opts) ->
    case hb_http:request(
        #{
            <<"method">> => <<"GET">>,
            <<"path">> => <<"/arweave/tx/", ID/binary, "/offset">>
        },
        request_opts()
    ) of
        {ok, Response} ->
            OffsetMsg = hb_json:decode(maps:get(<<"body">>, Response)),
            EndOffset = hb_util:int(maps:get(<<"offset">>, OffsetMsg)),
            Size = hb_util:int(maps:get(<<"size">>, OffsetMsg)),
            {ok, EndOffset - Size};
        Error ->
            {error, Error}
    end.

list_bucket_keys(DataRoot, Opts) ->
    Prefix = <<"chunks/", DataRoot/binary, "/">>,
    list_bucket_keys(Prefix, undefined, Opts, []).

list_bucket_keys(Prefix, Token, Opts, Acc) ->
    case signed_get(list_url(Prefix, Token, Opts), Opts) of
        {ok, XML} ->
            Keys = extract_all(XML, <<"<Key>([^<]+)</Key>">>),
            NextToken =
                extract_one(
                    XML,
                    <<"<NextContinuationToken>([^<]+)</NextContinuationToken>">>
                ),
            case NextToken of
                undefined ->
                    {ok, sort_keys(Acc ++ Keys)};
                _ ->
                    list_bucket_keys(Prefix, NextToken, Opts, Acc ++ Keys)
            end;
        Error ->
            Error
    end.

list_url(Prefix, undefined, Opts) ->
    <<
        (bucket_endpoint(Opts))/binary,
        "/",
        (bucket_name(Opts))/binary,
        "?list-type=2&prefix=",
        (hb_escape:encode(Prefix))/binary,
        "&max-keys=1000"
    >>;
list_url(Prefix, Token, Opts) ->
    <<
        (list_url(Prefix, undefined, Opts))/binary,
        "&continuation-token=",
        (hb_escape:encode(Token))/binary
    >>.

sort_keys(Keys) ->
    lists:sort(fun(A, B) -> key_end(A) =< key_end(B) end, Keys).

key_end(Key) ->
    Segments = binary:split(Key, <<"/">>, [global]),
    hb_util:int(lists:last(Segments)).

build_tree(Keys, DataRoot, DataSize, Opts) ->
    case collect_tree_meta(Keys, 0, Opts, []) of
        {ok, []} ->
            {error, tree_build, #{ data_root => DataRoot, reason => empty }};
        {ok, Meta} ->
            LastEnd = maps:get(end_offset, lists:last(Meta)),
            case LastEnd =:= DataSize of
                false ->
                    {error,
                        tree_build,
                        #{
                            data_root => DataRoot,
                            data_size => DataSize,
                            last_end => LastEnd
                        }};
                true ->
                    ChunkIDSizes =
                        [
                            {maps:get(chunk_id, Item), maps:get(end_offset, Item)}
                        ||
                            Item <- Meta
                        ],
                    {Root, Tree} = ar_merkle:generate_tree(ChunkIDSizes),
                    case Root =:= hb_util:decode(DataRoot) of
                        true ->
                            {ok, Root, Tree, Meta};
                        false ->
                            {error,
                                data_root_mismatch,
                                #{
                                    expected => DataRoot,
                                    actual => hb_util:encode(Root)
                                }}
                    end
            end;
        {error, Stage, Reason} ->
            {error, Stage, Reason}
    end.

collect_tree_meta([], _PrevEnd, _Opts, Acc) ->
    {ok, lists:reverse(Acc)};
collect_tree_meta([Key | Rest], PrevEnd, Opts, Acc) ->
    EndOffset = key_end(Key) + 1,
    ExpectedSize = EndOffset - PrevEnd,
    case get_object(Key, Opts) of
        {ok, Chunk} when byte_size(Chunk) =:= ExpectedSize ->
            collect_tree_meta(
                Rest,
                EndOffset,
                Opts,
                [
                    #{
                        key => Key,
                        end_offset => EndOffset,
                        chunk_id => ar_tx:generate_chunk_id(Chunk)
                    }
                    | Acc
                ]
            );
        {ok, Chunk} ->
            {error,
                chunk_fetch,
                #{
                    key => Key,
                    expected_size => ExpectedSize,
                    actual_size => byte_size(Chunk)
                }};
        {error, Reason} ->
            {error, chunk_fetch, #{ key => Key, reason => Reason }}
    end.

seed_keys(
    _RequestedID,
    _RootTXID,
    _Root,
    _Tree,
    [],
    _DataSize,
    _StartOffset,
    _Opts
) ->
    {ok, 0};
seed_keys(
    RequestedID,
    RootTXID,
    Root,
    Tree,
    [Item | Rest],
    DataSize,
    StartOffset,
    Opts
) ->
    EndOffset = maps:get(end_offset, Item),
    Offset = EndOffset - 1,
    AbsoluteOffset = StartOffset + Offset + 1,
    DataPath = ar_merkle:generate_path(Root, Offset, Tree),
    Key = maps:get(key, Item),
    maybe_log_proof_progress(
        RequestedID,
        RootTXID,
        length(Rest) + 1,
        Offset,
        AbsoluteOffset,
        maps:get(proof_index, Item, undefined)
    ),
    case get_object(Key, Opts) of
        {ok, Chunk} ->
            case
                seed_one_proof(
                    RequestedID,
                    RootTXID,
                    Root,
                    Chunk,
                    DataPath,
                    Offset,
                    DataSize,
                    AbsoluteOffset,
                    Opts
                )
            of
                {ok, Added} ->
                    case
                        seed_keys(
                            RequestedID,
                            RootTXID,
                            Root,
                            Tree,
                            Rest,
                            DataSize,
                            StartOffset,
                            Opts
                        )
                    of
                        {ok, RestCount} -> {ok, Added + RestCount};
                        Error -> Error
                    end;
                {error, Stage, Reason} ->
                    {error, Stage, Reason}
            end;
        {error, Reason} ->
            {error, chunk_fetch, #{ key => Key, reason => Reason }}
    end.

number_proofs([], _Index) ->
    [];
number_proofs([Item | Rest], Index) ->
    [Item#{ proof_index => Index } | number_proofs(Rest, Index + 1)].

seed_one_proof(
    RequestedID,
    RootTXID,
    Root,
    Chunk,
    DataPath,
    Offset,
    DataSize,
    AbsoluteOffset,
    Opts
) ->
    Body =
        hb_json:encode(
            #{
                <<"chunk">> => hb_util:encode(Chunk),
                <<"data_path">> => hb_util:encode(DataPath),
                <<"offset">> => integer_to_binary(Offset),
                <<"data_size">> => integer_to_binary(DataSize),
                <<"data_root">> => hb_util:encode(Root)
            }
        ),
    case route_nodes(AbsoluteOffset, Opts) of
        {ok, Nodes} ->
            Results = post_to_nodes(Nodes, Body, Opts),
            case ensure_all_200(Results) of
                ok ->
                    {ok, length(Results)};
                {error, Failed} ->
                    {error,
                        proof_post,
                        #{
                            id => RequestedID,
                            root_tx_id => RootTXID,
                            offset => Offset,
                            absolute_offset => AbsoluteOffset,
                            failed => Failed
                        }}
            end;
        {error, Reason} ->
            {error,
                chunk_route,
                #{
                    id => RequestedID,
                    root_tx_id => RootTXID,
                    offset => Offset,
                    absolute_offset => AbsoluteOffset,
                    reason => Reason
                }}
    end.

route_nodes(AbsoluteOffset, Opts) ->
    case explicit_nodes(Opts) of
        auto ->
            case
                dev_router:route(
                    #{
                        <<"method">> => <<"POST">>,
                        <<"path">> => <<"/arweave/chunk">>,
                        <<"route-by">> => AbsoluteOffset
                    },
                    Opts
                )
            of
                {ok, #{ <<"nodes">> := Nodes }} ->
                    {ok, hb_util:message_to_ordered_list(Nodes, Opts)};
                {ok, Node = #{ <<"uri">> := _ }} ->
                    {ok, [Node]};
                {ok, URI} when is_binary(URI) ->
                    {ok, [#{ <<"uri">> => URI, <<"opts">> => #{} }]};
                Error ->
                    Error
            end;
        Nodes ->
            {ok, [#{ <<"uri">> => URI, <<"opts">> => #{} } || URI <- Nodes]}
    end.

post_to_nodes(Nodes, Body, Opts) ->
    hb_pmap:parallel_map(
        Nodes,
        fun(Node) -> post_to_node(Node, Body, 3) end,
        node_post_concurrency(Opts)
    ).

post_to_node(Node, Body, AttemptsLeft) ->
    URI = maps:get(<<"uri">>, Node, maps:get(<<"prefix">>, Node, <<"unknown">>)),
    case hb_http:post(Node, #{ <<"body">> => Body }, request_opts()) of
        {ok, Response} ->
            Result = #{
                uri => URI,
                status => response_status(Response),
                response => Response
            },
            maybe_retry_post(Node, Body, AttemptsLeft, Result);
        Error ->
            Result = #{
                uri => URI,
                status => error,
                response => Error
            },
            maybe_retry_post(Node, Body, AttemptsLeft, Result)
    end.

maybe_retry_post(_Node, _Body, _AttemptsLeft, Result = #{ status := 200 }) ->
    Result;
maybe_retry_post(Node, Body, AttemptsLeft, Result) when AttemptsLeft > 1 ->
    case retryable_post_result(Result) of
        true ->
            timer:sleep(250),
            post_to_node(Node, Body, AttemptsLeft - 1);
        false ->
            Result
    end;
maybe_retry_post(_Node, _Body, _AttemptsLeft, Result) ->
    Result.

retryable_post_result(#{ status := error }) ->
    true;
retryable_post_result(#{ status := Status })
    when is_integer(Status), Status >= 500 ->
    true;
retryable_post_result(_) ->
    false.

response_status(Response) ->
    hb_util:int(hb_ao:get(<<"status">>, Response, 0, #{})).

ensure_all_200(Results) ->
    Failed =
        [
            Result
        ||
            Result <- Results,
            maps:get(status, Result) =/= 200
        ],
    case Failed of
        [] -> ok;
        _ -> {error, Failed}
    end.

request_opts() ->
    #{
        cache_control => [<<"no-cache">>, <<"no-store">>],
        http_only_result => false
    }.

extract_all(Bin, Pattern) ->
    case re:run(Bin, Pattern, [global, {capture, all_but_first, binary}]) of
        {match, Matches} ->
            [Value || [Value] <- Matches];
        nomatch ->
            []
    end.

extract_one(Bin, Pattern) ->
    case re:run(Bin, Pattern, [{capture, [1], binary}]) of
        {match, [Value]} -> Value;
        nomatch -> undefined
    end.

signed_get(URL, Opts) ->
    curl(
        [
            "--silent",
            "--show-error",
            "--max-time",
            "30",
            "--aws-sigv4",
            sigv4_scope(Opts),
            "--user",
            bucket_auth_user(Opts),
            binary_to_list(URL)
        ]
    ).

get_object(Key, Opts) ->
    URL =
        <<
            (bucket_endpoint(Opts))/binary,
            "/",
            (bucket_name(Opts))/binary,
            "/",
            Key/binary
        >>,
    curl(
        [
            "--silent",
            "--show-error",
            "--max-time",
            "60",
            "--aws-sigv4",
            sigv4_scope(Opts),
            "--user",
            bucket_auth_user(Opts),
            binary_to_list(URL)
        ]
    ).

curl(Args) ->
    Exec =
        case os:find_executable("curl") of
            false -> "/usr/bin/curl";
            Path -> Path
        end,
    Port =
        open_port(
            {spawn_executable, Exec},
            [binary, exit_status, use_stdio, stderr_to_stdout, {args, Args}]
        ),
    collect_port(Port, []).

collect_port(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_port(Port, [Data | Acc]);
        {Port, {exit_status, 0}} ->
            {ok, iolist_to_binary(lists:reverse(Acc))};
        {Port, {exit_status, Status}} ->
            {error,
                #{
                    exit_status => Status,
                    output => iolist_to_binary(lists:reverse(Acc))
                }}
    end.

log_stage(ID, Stage) ->
    ?event(safe_harbor, {bucket_reseed_stage, {id, {string, ID}}, {stage, Stage}}).

log_stage(ID, RootTXID, Stage) ->
    ?event(
        safe_harbor,
        {bucket_reseed_stage,
            {id, {string, ID}},
            {root_tx_id, {string, RootTXID}},
            {stage, Stage}
        }
    ).

maybe_log_proof_progress(
    RequestedID,
    RootTXID,
    TotalLeft,
    Offset,
    AbsoluteOffset,
    undefined
) ->
    maybe_log_proof_progress(
        RequestedID,
        RootTXID,
        TotalLeft,
        Offset,
        AbsoluteOffset,
        1
    );
maybe_log_proof_progress(
    RequestedID,
    RootTXID,
    TotalLeft,
    Offset,
    AbsoluteOffset,
    Index
) ->
    Total = Index + TotalLeft - 1,
    case should_log_proof(Index, Total) of
        true ->
            ?event(
                safe_harbor,
                {bucket_reseed_proof,
                    {id, RequestedID},
                    {root_tx_id, RootTXID},
                    {index, Index},
                    {total, Total},
                    {offset, Offset},
                    {absolute_offset, AbsoluteOffset}
                }
            );
        false ->
            ok
    end.

should_log_proof(Index, Total) ->
    Total =< 10
        orelse Index =:= 1
        orelse Index =:= Total
        orelse (Index rem 25) =:= 0.

explicit_nodes(Opts) ->
    normalize_nodes(
        opt_value([safe_harbor_chunk_bucket_nodes], auto, Opts)
    ).

normalize_nodes(auto) ->
    auto;
normalize_nodes(Nodes) when is_list(Nodes), Nodes =:= [] ->
    auto;
normalize_nodes([NodeChar | _] = Nodes) when is_list(Nodes), is_integer(NodeChar) ->
    normalize_nodes(hb_util:bin(Nodes));
normalize_nodes(Nodes) when is_binary(Nodes) ->
    [
        trim(Node)
    ||
        Node <- binary:split(Nodes, <<",">>, [global]),
        trim(Node) =/= <<>>
    ];
normalize_nodes(Nodes) when is_list(Nodes) ->
    [hb_util:bin(Node) || Node <- Nodes];
normalize_nodes(Node) ->
    [hb_util:bin(Node)].

node_post_concurrency(Opts) ->
    opt_value(
        [safe_harbor_chunk_bucket_node_post_concurrency],
        ?DEFAULT_NODE_POST_CONCURRENCY,
        Opts
    ).

bucket_endpoint(Opts) ->
    opt_value(
        [priv_safe_harbor_bucket_endpoint],
        undefined,
        Opts
    ).

bucket_access_key(Opts) ->
    opt_value(
        [priv_safe_harbor_bucket_access_key],
        undefined,
        Opts
    ).

bucket_secret_key(Opts) ->
    opt_value(
        [priv_safe_harbor_bucket_secret_key],
        undefined,
        Opts
    ).

bucket_name(Opts) ->
    opt_value(
        [priv_safe_harbor_bucket_name],
        ?DEFAULT_BUCKET_NAME,
        Opts
    ).

bucket_region(Opts) ->
    opt_value(
        [priv_safe_harbor_bucket_region],
        ?DEFAULT_BUCKET_REGION,
        Opts
    ).

bucket_auth_user(Opts) ->
    binary_to_list(
        <<
            (bucket_access_key(Opts))/binary,
            ":",
            (bucket_secret_key(Opts))/binary
        >>
    ).

sigv4_scope(Opts) ->
    binary_to_list(<<"aws:amz:", (bucket_region(Opts))/binary, ":s3">>).

opt_value([], Default, _Opts) ->
    Default;
opt_value([Key | Rest], Default, Opts) ->
    case hb_opts:get(Key, hb_opts_not_found, Opts) of
        hb_opts_not_found -> opt_value(Rest, Default, Opts);
        Value ->
            case has_value(Value) of
                true -> Value;
                false -> opt_value(Rest, Default, Opts)
            end
    end.

has_value(undefined) -> false;
has_value(false) -> false;
has_value(<<>>) -> false;
has_value([]) -> false;
has_value(_) -> true.

trim(Binary) ->
    trim_right(trim_left(Binary)).

trim_left(<<C, Rest/binary>>) when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
    trim_left(Rest);
trim_left(Binary) ->
    Binary.

trim_right(Binary) ->
    trim_right(Binary, byte_size(Binary)).

trim_right(_Binary, 0) ->
    <<>>;
trim_right(Binary, Size) ->
    case binary:at(Binary, Size - 1) of
        C when C =:= $\s; C =:= $\t; C =:= $\n; C =:= $\r ->
            trim_right(Binary, Size - 1);
        _ ->
            binary:part(Binary, 0, Size)
    end.

-ifdef(EUNIT).

enabled_with_bucket_config_test() ->
    ?assert(
        enabled(
            #{
                priv_safe_harbor_bucket_endpoint => <<"http://bucket">>,
                priv_safe_harbor_bucket_access_key => <<"access">>,
                priv_safe_harbor_bucket_secret_key => <<"secret">>
            }
        )
    ).

sort_keys_test() ->
    ?assertEqual(
        [
            <<"chunks/root/1">>,
            <<"chunks/root/10">>,
            <<"chunks/root/100">>
        ],
        sort_keys(
            [
                <<"chunks/root/100">>,
                <<"chunks/root/1">>,
                <<"chunks/root/10">>
            ]
        )
    ).

normalize_nodes_binary_test() ->
    ?assertEqual(
        [<<"http://tip-1">>, <<"http://tip-2">>],
        normalize_nodes(<<"http://tip-1,http://tip-2">>)
    ).

-endif.
