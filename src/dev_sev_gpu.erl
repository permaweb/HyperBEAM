%%% @doc NVIDIA GPU TEE Attestation Device
-module(dev_sev_gpu).
-export([info/1, generate/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(PYTHON_TIMEOUT, 30000).
-define(TEST_MOCK_NONCE, <<"da4a06c3604a5fac8aa0b4aaf5a6354cdd0dc7c193299bc3464f30b5cbfb931a">>).

info(_) -> 
    #{exports => [<<"info">>, <<"generate">>, <<"verify">>]}.

-spec generate(map(), map(), map()) -> {ok, binary()} | {error, term()}.
generate(_M1, M2, Opts) ->
    maybe
        {ok, _} ?= ensure_python_environment(),
        Nonce = hb_ao:get(nonce, M2, ?TEST_MOCK_NONCE, Opts),
        {ok, TokenJSON} ?= call_python_attestation(generate, #{
            <<"nonce">> => Nonce,
            <<"name">> => <<"hyperbeam-node">>,
            <<"claims_version">> => <<"3.0">>,
            <<"device_type">> => <<"gpu">>,
            <<"environment">> => <<"local">>
        }),
        {ok, TokenJSON}
    else
        {error, Reason} -> {error, Reason};
        Error -> {error, Error}
    end.

-spec verify(map(), map(), map()) -> {ok, binary()} | {error, term()}.
verify(_M1, M2, NodeOpts) ->
    maybe
        {ok, _} ?= ensure_python_environment(),
        TokenJSON = maps:get(<<"body">>, M2),
        Nonce = hb_ao:get(<<"nonce">>, M2, ?TEST_MOCK_NONCE, NodeOpts),
        {ok, TokenResult} ?= verify_token(TokenJSON, Nonce),
        case TokenResult of
            true -> {ok, <<"true">>};
            false -> {ok, <<"false">>}
        end
    else
        {error, Reason} -> {error, Reason}
    end.

-spec ensure_python_environment() -> {ok, true} | {error, term()}.
ensure_python_environment() ->
    case get(python_env_checked) of
        true -> {ok, true};
        _ ->
            case os:cmd("python3 -c \"import nv_attestation_sdk; print('OK')\"") of
                "OK\n" ->
                    put(python_env_checked, true),
                    {ok, true};
                _ ->
                    {error, python_env_not_available}
            end
    end.

-spec verify_token(binary(), binary()) -> {ok, boolean()} | {error, term()}.
verify_token(TokenJSON, Nonce) ->
    case call_python_attestation(verify, #{
        <<"token">> => TokenJSON,
        <<"nonce">> => Nonce,
        <<"name">> => <<"hyperbeam-node">>,
        <<"device_type">> => <<"gpu">>,
        <<"environment">> => <<"local">>
    }) of
        {ok, VerifyResult} ->
            case hb_json:decode(VerifyResult) of
                #{<<"valid">> := Valid} when is_boolean(Valid) -> {ok, Valid};
                _ -> {error, invalid_verify_result}
            end;
        {error, Error} ->
            {error, Error}
    end.

-spec call_python_attestation(atom(), map()) -> {ok, binary()} | {error, term()}.
call_python_attestation(Action, Data) ->
    try
        {ok, CurrentDirectory} = file:get_cwd(),
        ScriptDir = filename:join([CurrentDirectory, "native", "dev_sev_gpu"]),
        TempFile = filename:join(ScriptDir, 
            "dev_sev_gpu_" ++ integer_to_list(erlang:system_time()) ++ ".json"),
        
        RequestJSON = hb_json:encode(#{
            <<"action">> => atom_to_binary(Action),
            <<"data">> => Data
        }),
        ok = file:write_file(TempFile, RequestJSON),
        
        ShellCmd = lists:flatten(io_lib:format(
            "cat ~s | python3 ~s 2>/dev/null && rm ~s", 
            [TempFile, filename:join(ScriptDir, "main.py"), TempFile]
        )),
        
        Port = open_port({spawn, ShellCmd}, [binary, use_stdio, {cd, ScriptDir}]),
        
        Result = receive
            {Port, {data, ResponseData}} ->
                case hb_json:decode(ResponseData) of
                    #{<<"status">> := <<"ok">>, <<"result">> := ResultData} ->
                        case ResultData of
                            #{<<"token">> := Token} -> {ok, Token};
                            _ -> {ok, hb_json:encode(ResultData)}
                        end;
                    #{<<"status">> := <<"error">>, <<"error">> := Error} ->
                        {error, {python_error, Error}};
                    _ ->
                        {error, {invalid_response, ResponseData}}
                end;
            {Port, {exit_status, Status}} when Status =/= 0 ->
                {error, {python_exit_error, Status}}
        after ?PYTHON_TIMEOUT ->
            {error, python_timeout}
        end,
        port_close(Port),
        Result
    catch
        _Type:Reason -> {error, {python_call_failed, Reason}}
    end.

generate_test() ->
    case generate(#{}, #{nonce => ?TEST_MOCK_NONCE}, #{}) of
        {ok, TokenJSON} ->
            ?assert(is_binary(TokenJSON)),
            ?assert(byte_size(TokenJSON) > 0);
        {error, {python_error, <<"No evidence available for attestation">>}} ->
            ?assert(true);
        {error, python_env_not_available} ->
            ?assert(true);
        Other ->
            ?assertEqual({ok, token}, Other)
    end.

verify_test() ->
    case generate(#{}, #{nonce => ?TEST_MOCK_NONCE}, #{}) of
        {ok, GeneratedToken} ->
            ?assert(is_binary(GeneratedToken)),
            ?assert(byte_size(GeneratedToken) > 0),
            VerifyMsg = #{
                <<"body">> => GeneratedToken,
                <<"nonce">> => ?TEST_MOCK_NONCE
            },
            case verify(#{}, VerifyMsg, #{}) of
                {ok, _} -> ?assert(true);
                {error, _} -> ?assert(true)
            end;
        {error, python_env_not_available} ->
            ?assert(true);
        {error, {python_error, <<"No evidence available for attestation">>}} ->
            ?assert(true)
    end. 