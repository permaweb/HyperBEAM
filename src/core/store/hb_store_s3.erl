%%% @doc A store module that keeps content-addressed blobs in an S3-compatible
%%% bucket. The bucket holds only `data/<hash>' values, where `<hash>' is the
%%% base64url SHA-256 of the bytes: every other path, and every link, group,
%%% list and match, answers `not_found' without a request. Bytes are checked
%%% against their key before a write is sent and after a read is received,
%%% an object the bucket holds with the MD5 of the bytes as its `ETag' is not
%%% sent again while any other object under the key is replaced, and each
%%% object is one request, buffered whole in memory. `hb_cache' keeps
%%% values under 60 bytes inline under other keys, so a `min-value-size' of
%%% at least 60 keeps everything but blobs away from this store.
%%%
%%% Store message keys: `name', `bucket', `endpoint' (`scheme://host[:port]';
%%% `https' must use port 443 and `http' must not) and `region' (default
%%% `us-east-1'). Objects are addressed path-style, `/<bucket>/<key>'.
%%% The credentials are not in the store message: they are the
%%% `access-key-id' and `secret-access-key' under the store's `name' in the
%%% private element (`priv') of the node message.
%%%
%%% Requests are signed with AWS Signature Version 4 and time out after 30
%%% seconds. A 404 is `not_found', and when S3 names a missing bucket in it
%%% an error event says so; any other answer below 500 is a typed error; a
%%% 5xx answer or a transport error is a failure, which `hb_store' retries
%%% once and then passes over. During an outage a write therefore lands in
%%% the next store of the list and stays there, and a read of an object only
%%% the bucket holds fails rather than reporting a missing key.
-module(hb_store_s3).
-behavior(hb_store).
-export([start/3, stop/3, reset/3, scope/0]).
-export([type/3, read/3, write/3, list/3]).
-export([group/3, link/3, match/3, resolve/3]).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").

%% @doc Start the store. A no-op: the store holds no local state.
start(_Store, _Req, _Opts) -> ok.

%% @doc Stop the store. A no-op: the store holds no local state.
stop(_Store, _Req, _Opts) -> ok.

%% @doc Reset the store. A no-op: bucket contents are never deleted.
reset(_Store, _Req, _Opts) -> ok.

%% @doc The store is `local' so that lazy link loads, which scope the store
%% list to `local', can reach blobs that live only in the bucket.
scope() -> local.

%% @doc Read a whole object, refusing bytes that do not hash to their key.
read(Store, #{ <<"read">> := Key }, NodeOpts) ->
    maybe
        {ok, Path} ?= data_path(Key),
        {ok, Body} ?= request(Store, <<"GET">>, Path, <<>>, NodeOpts),
        ok ?= verify(Path, Body),
        {ok, Body}
    end.

%% @doc Write objects. A request that is not entirely `data/' paths with
%% binary values is not the bucket's job and is passed over without a call.
write(Store, Req, NodeOpts) when is_map(Req) ->
    Blobs =
        [
            {Path, Bin}
        ||
            {Key, Bin} <- maps:to_list(Req),
            is_binary(Bin),
            {ok, Path} <- [data_path(Key)]
        ],
    case length(Blobs) == map_size(Req) of
        true -> put_all(Store, Blobs, NodeOpts);
        false -> {error, not_found}
    end.

%% @doc Store each object in turn, stopping at the first that is not stored.
%% Bytes that do not hash to their key are refused before any request.
put_all(_Store, [], _NodeOpts) -> ok;
put_all(Store, [{Path, Bin} | Rest], NodeOpts) ->
    maybe
        ok ?= verify(Path, Bin),
        {ok, _} ?= put_unless_held(Store, Path, Bin, NodeOpts),
        put_all(Store, Rest, NodeOpts)
    end.

%% @doc A `HEAD' of the object and, unless the bucket holds one with the MD5
%% of the bytes as its `ETag', the `PUT', which also replaces an object the
%% key does not belong to. A `HEAD' the service refuses (S3 answers 403 for
%% a missing key without `s3:ListBucket') leads to the `PUT' as well.
put_unless_held(Store, Path, Bin, NodeOpts) ->
    case request(Store, <<"HEAD">>, Path, <<>>, NodeOpts) of
        {ok, Headers} ->
            case held(Headers, Bin) of
                true -> {ok, held};
                false -> request(Store, <<"PUT">>, Path, Bin, NodeOpts)
            end;
        {error, _} -> request(Store, <<"PUT">>, Path, Bin, NodeOpts);
        Failure -> Failure
    end.

%% @doc Whether a `HEAD' answer names an object with the MD5 of the bytes as
%% its `ETag', which S3 reports for a single-part upload without SSE-KMS or
%% SSE-C; any other `ETag' leads to the `PUT'. Header names arrive as the
%% service sent them.
held(Headers, Bin) ->
    ETag = <<$", (hex(crypto:hash(md5, Bin)))/binary, $">>,
    [ETag] == [V || {K, V} <- Headers, hb_util:to_lower(K) == <<"etag">>].

%% @doc Check that bytes hash to the key that their `data/<hash>' path names.
verify(<<"data/", Hash/binary>> = Path, Bin) ->
    case hb_util:encode(crypto:hash(sha256, Bin)) of
        Hash -> ok;
        _ ->
            ?event(store_s3, {hash_mismatch, {path, Path}}),
            {error, {hash_mismatch, Path}}
    end.

%% @doc Lists, groups, links and matches are not the bucket's job.
list(_Store, _Req, _NodeOpts) -> {error, not_found}.
group(_Store, _Req, _NodeOpts) -> {error, not_found}.
link(_Store, _Req, _NodeOpts) -> {error, not_found}.
match(_Store, _Req, _NodeOpts) -> {error, not_found}.

%% @doc The bucket holds no links: a `data/<hash>' path resolves to itself,
%% and any other path is left to the stores that can hold it.
resolve(_Store, #{ <<"resolve">> := Key }, _NodeOpts) ->
    data_path(Key).

%% @doc The binary form of a `data/<hash>' path: any other is `not_found'.
data_path(Key) ->
    case hb_path:to_binary(Key) of
        <<"data/", _, _/binary>> = Path -> {ok, Path};
        _ -> {error, not_found}
    end.

%% @doc Every object in the bucket is a simple value: ask for it with a HEAD.
type(Store, #{ <<"type">> := Key }, NodeOpts) ->
    maybe
        {ok, Path} ?= data_path(Key),
        {ok, _} ?= request(Store, <<"HEAD">>, Path, <<>>, NodeOpts),
        {ok, simple}
    end.

%% @doc Sign a request for one object and send it through the hackney
%% client, which retries nothing: `hb_store' does. A store without
%% credentials, a bucket or an endpoint, or whose endpoint is refused, is a
%% typed error and nothing is sent. Events name the method and path only,
%% never the store or its credentials.
request(Store, Method, Path, Body, NodeOpts) ->
    maybe
        {ok, {KeyID, Secret}} ?= credentials(Store, NodeOpts),
        {ok, Region} ?= find(<<"region">>, Store, <<"us-east-1">>),
        {ok, {Peer, Host, Base}} ?= endpoint(Store),
        URIPath = <<Base/binary, "/", Path/binary>>,
        % The client adds a `content-type' to a PUT, so one is given to sign.
        Headers = #{
            <<"content-type">> => <<"application/octet-stream">>,
            <<"host">> => Host,
            <<"x-amz-content-sha256">> => hex(crypto:hash(sha256, Body)),
            <<"x-amz-date">> => amz_date()
        },
        Authorization = sign(Method, URIPath, Headers, {KeyID, Secret, Region}),
        ?event(store_s3, {request, {method, Method}, {path, Path}}),
        result(
            Method,
            hb_http_client:request(
                #{
                    peer => Peer,
                    path => uri_string:quote(URIPath, "/"),
                    method => Method,
                    headers => Headers#{ <<"authorization">> => Authorization },
                    body => Body
                },
                NodeOpts#{
                    <<"http-client">> => hackney,
                    <<"http-retry">> => 0,
                    <<"http-client-connect-timeout">> => 30_000,
                    <<"http-client-hackney-recv-timeout">> => 30_000
                }
            )
        )
    end.

%% @doc Map an HTTP answer to a store result: the headers of a 2xx to a
%% `HEAD' and the body of any other 2xx, `not_found' for a 404, a typed
%% error for any other status below 500, and a failure, which `hb_store'
%% retries, for a 5xx answer or a transport error. A 404 whose body names a
%% missing bucket, which a `GET' or `PUT' carries and a `HEAD' does not, is
%% reported in an error event before it is a miss.
result(<<"HEAD">>, {ok, Status, Headers, _Body})
        when Status >= 200, Status < 300 ->
    {ok, Headers};
result(_Method, {ok, Status, _Headers, Body})
        when Status >= 200, Status < 300 ->
    {ok, Body};
result(_Method, {ok, 404, _Headers, Body}) ->
    case binary:match(Body, <<"<Code>NoSuchBucket</Code>">>) of
        nomatch -> ok;
        _ -> ?event(error, {no_such_bucket, {answer, Body}})
    end,
    {error, not_found};
result(_Method, {ok, Status, _Headers, Body}) when Status < 500 ->
    {error, #{ <<"status">> => Status, <<"body">> => Body }};
result(_Method, {ok, Status, _Headers, Body}) ->
    {failure, #{ <<"status">> => Status, <<"body">> => Body }};
result(_Method, {error, Reason}) ->
    {failure, Reason}.

%% @doc The key ID and secret the store signs with: those under the store's
%% `name' in the private element of the node message, which keeps them out
%% of the store message and of anything that prints or publishes it.
credentials(Store, NodeOpts) ->
    maybe
        {ok, Name} ?= find(<<"name">>, Store),
        Private =
            case hb_private:from_message(NodeOpts) of
                #{ Name := Found } when is_map(Found) -> Found;
                _ -> #{}
            end,
        {ok, KeyID} ?= find(<<"access-key-id">>, Private),
        {ok, Secret} ?= find(<<"secret-access-key">>, Private),
        {ok, {KeyID, Secret}}
    end.

%% @doc The binary under a key of a message, or the default when the key is
%% absent: a typed `{error, {missing, Key}}' if that is not a binary.
find(Key, Msg) -> find(Key, Msg, undefined).
find(Key, Msg, Default) ->
    case maps:get(Key, Msg, Default) of
        Value when is_binary(Value) -> {ok, Value};
        _ -> {error, {missing, Key}}
    end.

%% @doc The peer to connect to, the `host' header to sign and the path that
%% object keys live under. `hb_http_client' selects TLS by port, so `https'
%% must use port 443 and `http' must not: any other pairing is refused rather
%% than sent over the wrong transport, as is any endpoint that is not of the
%% form `scheme://host[:port]'.
endpoint(Store) ->
    Raw = maps:get(<<"endpoint">>, Store, undefined),
    maybe
        {ok, Bucket} ?= find(<<"bucket">>, Store),
        {ok, _} ?= find(<<"endpoint">>, Store),
        #{ scheme := Scheme, host := <<_, _/binary>> = Host } = URI ?=
            uri_string:parse(Raw),
        {ok, Default} ?=
            maps:find(Scheme, #{ <<"https">> => 443, <<"http">> => 80 }),
        Port = maps:get(port, URI, Default),
        true ?= (Default == 443) == (Port == 443),
        true ?= lists:member(maps:get(path, URI), [<<>>, <<"/">>]),
        HostPort = <<Host/binary, ":", (hb_util:bin(Port))/binary>>,
        SignedHost =
            case Port of
                Default -> Host;
                _ -> HostPort
            end,
        Peer = <<Scheme/binary, "://", HostPort/binary>>,
        {ok, {Peer, SignedHost, <<"/", Bucket/binary>>}}
    else
        {error, _} = Missing -> Missing;
        _ -> {error, {unsupported_endpoint, Raw}}
    end.

%% @doc The current UTC time in `x-amz-date' form: RFC 3339 less `-' and `:'.
amz_date() ->
    Now = erlang:system_time(second),
    RFC3339 = calendar:system_time_to_rfc3339(Now, [{offset, "Z"}]),
    << <<C>> || C <- RFC3339, C =/= $-, C =/= $: >>.

%% @doc The AWS Signature Version 4 `authorization' value of a request to the
%% `s3' service. The path is S3-UriEncoded here, keeping `/'. Every header
%% given is signed: the names must be lowercase and include `host',
%% `x-amz-date' and `x-amz-content-sha256' (the hex SHA-256 of the body).
sign(Method, Path, Headers, {KeyID, Secret, Region}) ->
    #{
        <<"x-amz-date">> := <<Day:8/binary, _/binary>> = Date,
        <<"x-amz-content-sha256">> := PayloadHash
    } = Headers,
    Sorted = lists:sort(maps:to_list(Headers)),
    SignedHeaders = iolist_to_binary(lists:join(";", [K || {K, _} <- Sorted])),
    CanonicalRequest = [
        Method, "\n",
        uri_string:quote(Path, "/"), "\n\n",
        [[K, ":", V, "\n"] || {K, V} <- Sorted], "\n",
        SignedHeaders, "\n",
        PayloadHash
    ],
    Scope = <<Day/binary, "/", Region/binary, "/s3/aws4_request">>,
    StringToSign = [
        "AWS4-HMAC-SHA256\n", Date, "\n", Scope, "\n",
        hex(crypto:hash(sha256, CanonicalRequest))
    ],
    SigningKey =
        lists:foldl(
            fun(Part, Key) -> crypto:mac(hmac, sha256, Key, Part) end,
            <<"AWS4", Secret/binary>>,
            [Day, Region, <<"s3">>, <<"aws4_request">>]
        ),
    Signature = hex(crypto:mac(hmac, sha256, SigningKey, StringToSign)),
    <<
        "AWS4-HMAC-SHA256 Credential=", KeyID/binary, "/", Scope/binary,
        ",SignedHeaders=", SignedHeaders/binary,
        ",Signature=", Signature/binary
    >>.

%% @doc A binary as lowercase hex.
hex(Bin) -> binary:encode_hex(Bin, lowercase).

%%% Tests

%% @doc The `authorization' value of one of AWS's published S3 examples, which
%% sign `Extra', `host', `x-amz-date' and the hash of the body.
aws_example(Method, Path, Body, Extra) ->
    Headers = Extra#{
        <<"host">> => <<"examplebucket.s3.amazonaws.com">>,
        <<"x-amz-content-sha256">> => hex(crypto:hash(sha256, Body)),
        <<"x-amz-date">> => <<"20130524T000000Z">>
    },
    KeyID = <<"AKIAIOSFODNN7EXAMPLE">>,
    Secret = <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>,
    sign(Method, Path, Headers, {KeyID, Secret, <<"us-east-1">>}).

%% @doc AWS's published GET Object example, which signs a `range' header,
%% and its PUT Object example, with a `$' in the key to encode.
sigv4_test() ->
    Range = #{ <<"range">> => <<"bytes=0-9">> },
    Body = <<"Welcome to Amazon S3.">>,
    Extra = #{
        <<"date">> => <<"Fri, 24 May 2013 00:00:00 GMT">>,
        <<"x-amz-storage-class">> => <<"REDUCED_REDUNDANCY">>
    },
    ?assertEqual(
        <<
            "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/"
            "us-east-1/s3/aws4_request,SignedHeaders=host;range;"
            "x-amz-content-sha256;x-amz-date,Signature="
            "f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41"
        >>,
        aws_example(<<"GET">>, <<"/test.txt">>, <<>>, Range)
    ),
    ?assertEqual(
        <<
            "AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/"
            "us-east-1/s3/aws4_request,SignedHeaders=date;host;"
            "x-amz-content-sha256;x-amz-date;x-amz-storage-class,Signature="
            "98ad721746da40c64f1a55b78f14c238d841ea1380cd77a1b5971af0ece108bd"
        >>,
        aws_example(<<"PUT">>, <<"/test$file.text">>, Body, Extra)
    ).

%% @doc Each class of HTTP answer maps to its store result: a `HEAD' yields
%% its headers, a 404 is `not_found', also when S3 names a missing bucket in
%% it, and a 5xx answer or transport error is a failure.
result_test() ->
    Of = fun(Status) -> result(<<"GET">>, {ok, Status, [], <<"b">>}) end,
    NoBucket = <<"<Error><Code>NoSuchBucket</Code></Error>">>,
    Headers = [{<<"ETag">>, <<"e">>}],
    ?assertEqual({ok, <<"b">>}, Of(200)),
    ?assertEqual({ok, Headers}, result(<<"HEAD">>, {ok, 200, Headers, <<>>})),
    ?assertEqual({error, not_found}, Of(404)),
    ?assertEqual(
        {error, not_found},
        result(<<"GET">>, {ok, 404, [], NoBucket})
    ),
    ?assertMatch({error, #{ <<"status">> := 403, <<"body">> := _ }}, Of(403)),
    ?assertMatch({failure, #{ <<"status">> := 503 }}, Of(503)),
    ?assertEqual({failure, timeout}, result(<<"GET">>, {error, timeout})).

%% @doc An object is held when its `ETag', under any header-name case, is the
%% quoted MD5 of the bytes; a different, unquoted or absent `ETag' is not.
held_test() ->
    MD5 = <<$", (hex(crypto:hash(md5, <<"x">>)))/binary, $">>,
    ?assert(held([{<<"ETag">>, MD5}], <<"x">>)),
    ?assert(held([{<<"etag">>, MD5}, {<<"Date">>, <<"d">>}], <<"x">>)),
    ?assertNot(held([{<<"ETag">>, MD5}], <<"y">>)),
    ?assertNot(held([{<<"ETag">>, binary:part(MD5, 1, 32)}], <<"x">>)),
    ?assertNot(held([{<<"ETag">>, <<"\"abc-2\"">>}], <<"x">>)),
    ?assertNot(held([], <<"x">>)).

%% @doc The peer, signed host and base path of each endpoint form, and the
%% endpoints refused: a transport the port would not get, a prefix, no
%% scheme, none at all.
endpoint_test() ->
    Store = #{ <<"bucket">> => <<"b">> },
    At = fun(Endpoint) -> endpoint(Store#{ <<"endpoint">> => Endpoint }) end,
    Refused = fun(Endpoint) -> {error, {unsupported_endpoint, Endpoint}} end,
    AWS = <<"s3.eu-west-1.amazonaws.com">>,
    ?assertEqual(
        {ok, {<<"https://", AWS/binary, ":443">>, AWS, <<"/b">>}},
        At(<<"https://", AWS/binary>>)
    ),
    ?assertEqual(
        {ok, {<<"http://localhost:9000">>, <<"localhost:9000">>, <<"/b">>}},
        At(<<"http://localhost:9000">>)
    ),
    lists:foreach(
        fun(Endpoint) -> ?assertEqual(Refused(Endpoint), At(Endpoint)) end,
        [<<"https://h:9443">>, <<"http://h:443">>, <<"http://h/p">>, <<"h">>]
    ),
    ?assertEqual({error, {missing, <<"endpoint">>}}, At(1)),
    ?assertEqual({error, {missing, <<"endpoint">>}}, endpoint(Store)),
    ?assertEqual({error, {missing, <<"bucket">>}}, endpoint(#{})).

%% @doc A store whose endpoint is a closed port. Any request it sent would be
%% a failure, so any other answer shows that none was sent.
offline_store() ->
    #{
        <<"store-module">> => ?MODULE,
        <<"name">> => <<"s3-offline">>,
        <<"bucket">> => <<"nowhere">>,
        <<"endpoint">> => <<"http://127.0.0.1:1">>,
        <<"max-retries">> => 0
    }.

%% @doc Node options holding credentials for the store of the given name.
private_opts(Name, KeyID, Secret) ->
    Credentials = #{
        <<"access-key-id">> => KeyID,
        <<"secret-access-key">> => Secret
    },
    #{ <<"priv">> => #{ Name => Credentials } }.

%% @doc The `data/<hash>' path the given bytes belong at.
data_path_of(Bin) ->
    <<"data/", (hb_util:encode(crypto:hash(sha256, Bin)))/binary>>.

%% @doc Everything but a `data/' blob is answered without a request.
inert_paths_test() ->
    Store = offline_store(),
    Opts = private_opts(<<"s3-offline">>, <<"key-id">>, <<"secret">>),
    Path = data_path_of(<<"x">>),
    Other = #{ <<"k">> => <<"v">> },
    Mixed = Other#{ Path => <<"x">> },
    NotFound = {error, not_found},
    ?assertEqual(NotFound, hb_store:link(Store, #{ <<"a">> => <<"b">> }, Opts)),
    ?assertEqual(NotFound, hb_store:group(Store, <<"g">>, Opts)),
    ?assertEqual(NotFound, hb_store:list(Store, <<"g">>, Opts)),
    ?assertEqual(NotFound, hb_store:match(Store, Other, Opts)),
    ?assertEqual(NotFound, hb_store:resolve(Store, [<<"a">>, <<"b">>], Opts)),
    ?assertEqual({ok, Path}, hb_store:resolve(Store, Path, Opts)),
    ?assertEqual(NotFound, hb_store:read(Store, <<"k">>, Opts)),
    ?assertEqual(NotFound, hb_store:type(Store, <<"data">>, Opts)),
    ?assertEqual(NotFound, hb_store:write(Store, Other, Opts)),
    ?assertEqual(NotFound, hb_store:write(Store, #{ Path => #{} }, Opts)),
    ?assertEqual(NotFound, hb_store:write(Store, Mixed, Opts)),
    ?assertEqual(ok, hb_store:reset(Store)).

%% @doc Bytes that do not hash to their key are refused before any request,
%% a store without credentials or with a bad store message is a typed error,
%% and an unreachable bucket is a failure rather than `not_found'.
refusal_test() ->
    Store = offline_store(),
    Opts = #{ <<"priv">> := #{ <<"s3-offline">> := Credentials } } =
        private_opts(<<"s3-offline">>, <<"key-id">>, <<"secret">>),
    Path = data_path_of(<<"x">>),
    Read = fun(Misconfigured) -> hb_store:read(Misconfigured, Path, Opts) end,
    TLS = <<"https://127.0.0.1:1">>,
    Failure = {failure, econnrefused},
    ?assertEqual(
        {error, {hash_mismatch, Path}},
        hb_store:write(Store, #{ Path => <<"y">> }, Opts)
    ),
    lists:foreach(
        fun(Key) ->
            Without = #{ <<"s3-offline">> => maps:remove(Key, Credentials) },
            ?assertEqual(
                {error, {missing, Key}},
                hb_store:read(Store, Path, #{ <<"priv">> => Without })
            )
        end,
        [<<"access-key-id">>, <<"secret-access-key">>]
    ),
    ?assertEqual(
        {error, {missing, <<"access-key-id">>}},
        hb_store:read(Store, Path, #{})
    ),
    lists:foreach(
        fun(Key) ->
            Missing = {error, {missing, Key}},
            ?assertEqual(Missing, Read(maps:remove(Key, Store))),
            ?assertEqual(Missing, Read(Store#{ Key => not_a_binary }))
        end,
        [<<"name">>, <<"bucket">>, <<"endpoint">>]
    ),
    ?assertMatch({error, {missing, _}}, Read(Store#{ <<"region">> => 1 })),
    ?assertEqual(
        {error, {unsupported_endpoint, TLS}},
        Read(Store#{ <<"endpoint">> => TLS })
    ),
    ?assertEqual(Failure, Read(Store)),
    ?assertEqual(Failure, hb_store:write(Store, #{ Path => <<"x">> }, Opts)),
    ?assertEqual(Failure, hb_store:type(Store, Path, Opts)).

%% @doc The live suite, run only when `HB_S3_ENDPOINT', `HB_S3_BUCKET',
%% `HB_S3_ACCESS_KEY' and `HB_S3_SECRET_KEY' are set (`HB_S3_REGION' is
%% optional). Objects are written under keys fresh to each run.
live_test_() ->
    Vars = ["ENDPOINT", "BUCKET", "ACCESS_KEY", "SECRET_KEY"],
    Env = [os:getenv("HB_S3_" ++ Var) || Var <- Vars],
    case lists:member(false, Env) of
        true -> [];
        false ->
            [Endpoint, Bucket, KeyID, Secret] = [hb_util:bin(V) || V <- Env],
            Store = #{
                <<"store-module">> => ?MODULE,
                <<"name">> => <<"s3-live">>,
                <<"endpoint">> => Endpoint,
                <<"bucket">> => Bucket,
                <<"region">> =>
                    hb_util:bin(os:getenv("HB_S3_REGION", "us-east-1"))
            },
            Opts = private_opts(<<"s3-live">>, KeyID, Secret),
            [
                {timeout, 120, fun() -> live_object(Store, Opts) end},
                {timeout, 120, fun() -> live_cache(Store, Opts) end}
            ]
    end.

%% @doc Against a real bucket: an object is written, read and typed; a
%% missing key, and any key of a bucket that does not exist, is `not_found';
%% credentials the service refuses are a typed error. Bytes stored under a
%% key they do not hash to (sent with the store's own signed request, past
%% the write check) are refused by a read, and a write of the right bytes to
%% that key replaces them, after which the read succeeds. An object already
%% in the bucket is held: its `ETag' is the MD5 of its bytes.
live_object(Store, Opts) ->
    #{ <<"priv">> := #{ <<"s3-live">> := Credentials } } = Opts,
    BadSecret = Credentials#{ <<"secret-access-key">> => <<"no">> },
    Wrong = #{ <<"priv">> => #{ <<"s3-live">> => BadSecret } },
    Elsewhere = Store#{ <<"bucket">> => <<"hb-s3-no-such-bucket">> },
    Bin = crypto:strong_rand_bytes(100_000),
    Path = data_path_of(Bin),
    Small = crypto:strong_rand_bytes(32),
    Taken = data_path_of(Small),
    Refused = {error, {hash_mismatch, Taken}},
    ?assertEqual(ok, hb_store:write(Store, #{ Path => Bin }, Opts)),
    ?assertEqual({ok, held}, put_unless_held(Store, Path, Bin, Opts)),
    ?assertEqual({ok, Bin}, hb_store:read(Store, Path, Opts)),
    ?assertEqual({ok, simple}, hb_store:type(Store, Path, Opts)),
    ?assertEqual({error, not_found}, hb_store:read(Store, Taken, Opts)),
    ?assertEqual({error, not_found}, hb_store:type(Store, Taken, Opts)),
    ?assertEqual({error, not_found}, hb_store:read(Elsewhere, Path, Opts)),
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        hb_store:read(Store, Path, Wrong)
    ),
    ?assertMatch({ok, _}, request(Store, <<"PUT">>, Taken, <<"bad">>, Opts)),
    ?assertEqual(Refused, hb_store:read(Store, Taken, Opts)),
    ?assertEqual(ok, hb_store:write(Store, #{ Taken => Small }, Opts)),
    ?assertEqual({ok, Small}, hb_store:read(Store, Taken, Opts)).

%% @doc Write a message through `hb_cache' on a size-bounded local store and
%% a real bucket: the large value lands in the bucket alone and reads back.
live_cache(Store, Private) ->
    Local = (hb_test_utils:test_store(hb_store_lmdb, <<"s3-local">>))#{
        <<"max-value-size">> => 1024
    },
    Remote = Store#{ <<"min-value-size">> => 1025 },
    Opts = Private#{ <<"store">> => [Local, Remote] },
    ok = hb_store:start([Local, Remote]),
    Big = crypto:strong_rand_bytes(4096),
    Path = data_path_of(Big),
    {ok, ID} = hb_cache:write(#{ <<"body">> => Big, <<"k">> => <<"v">> }, Opts),
    {ok, Read} = hb_cache:read(ID, Opts),
    ?assertMatch(
        #{ <<"body">> := Big, <<"k">> := <<"v">> },
        hb_cache:ensure_all_loaded(Read, Opts)
    ),
    ?assertEqual({ok, Big}, hb_store:read(Remote, Path, Opts)),
    ?assertEqual({error, not_found}, hb_store:read(Local, Path, Opts)),
    hb_store:reset(Local).
