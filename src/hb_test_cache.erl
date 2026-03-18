%%% @doc Transparent HTTP response cache for tests. Activated by setting
%%% `http_client' to `hb_test_cache' (the test rebar3 profile does this
%%% automatically via DEFAULT_HTTP_CLIENT). Caches successful GET responses
%%% to external hosts on disk so repeated test runs avoid redundant network
%%% round-trips.
-module(hb_test_cache).
-export([request/2]).

-define(CACHE_DIR, "/tmp/hb_test_cache/").

%% @doc Handle an HTTP request: serve from cache when possible, otherwise
%% delegate to hb_http_client and cache the result.
request(Args, Opts) ->
    #{method := Method, peer := Peer, path := Path} = Args,
    case is_cacheable(Method, Peer) of
        false ->
            hb_http_client:request(Args, Opts#{http_client => hackney});
        true ->
            Key = cache_key(Path),
            case read_cache(Key) of
                {ok, Response} ->
                    io:put_chars(standard_error,
                        io_lib:format("[cache] HIT ~s~s~n", [Peer, Path])),
                    Response;
                miss ->
                    Response =
                        hb_http_client:request(
                            Args,
                            Opts#{http_client => hackney}
                        ),
                    write_cache(Key, Response),
                    Response
            end
    end.

%% @doc Return true when the request is a GET to a non-local peer.
is_cacheable(Method, Peer) ->
    is_get(Method) andalso is_external(Peer).

%% @doc Match common representations of the GET method.
is_get(<<"GET">>) -> true;
is_get(<<"get">>) -> true;
is_get(get) -> true;
is_get(_) -> false.

%% @doc Return false for localhost/loopback peers, true otherwise.
is_external(<<"http://localhost", _/binary>>) -> false;
is_external(<<"http://127.0.0.1", _/binary>>) -> false;
is_external("http://localhost" ++ _) -> false;
is_external("http://127.0.0.1" ++ _) -> false;
is_external(_) -> true.

%% @doc Produce a hex-encoded SHA-256 hash of the path for use as filename.
cache_key(Path) ->
    Hash = crypto:hash(sha256, to_bin(Path)),
    lists:flatten(
        [io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash]
    ).

%% @doc Coerce a list or binary to binary.
to_bin(B) when is_binary(B) -> B;
to_bin(L) when is_list(L) -> list_to_binary(L).

%% @doc Read a cached response from disk. Returns {ok, Term} or miss.
read_cache(Key) ->
    case file:read_file(cache_path(Key)) of
        {ok, Data} ->
            try {ok, binary_to_term(Data, [safe])}
            catch _:_ -> miss
            end;
        {error, _} -> miss
    end.

%% @doc Write a successful response (status < 400) to the cache directory.
%% Non-cacheable responses are silently ignored.
write_cache(Key, {ok, Status, _, _} = Response)
        when Status < 400 ->
    Path = cache_path(Key),
    filelib:ensure_dir(Path),
    file:write_file(Path, term_to_binary(Response));
write_cache(_, _) ->
    ok.

%% @doc Build the full filesystem path for a cache key.
cache_path(Key) ->
    ?CACHE_DIR ++ Key ++ ".bin".
