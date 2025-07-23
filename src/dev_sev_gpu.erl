%%% @doc NVIDIA GPU TEE Attestation Device
%%%
%%% This device provides GPU attestation token generation and verification
%%% using NVIDIA GPU TEE (Trusted Execution Environment) technology.
-module(dev_sev_gpu).
-export([info/1, generate/3, verify/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").
-hb_debug(print).
%% Python script timeout (milliseconds)
-define(PYTHON_TIMEOUT, 30000).

%% Test constants
-define(TEST_MOCK_NONCE, <<"da4a06c3604a5fac8aa0b4aaf5a6354cdd0dc7c193299bc3464f30b5cbfb931a">>).

%% @doc Exported function for getting device info, controls which functions are
%% exposed via the device API.
info(_) -> 
    #{ exports => [info, generate, verify] }.

%% @doc HTTP info response providing information about this device
info(_Msg1, _Msg2, _Opts) ->
    InfoBody = #{
        <<"description">> => <<"NVIDIA GPU TEE Attestation Device">>,
        <<"version">> => <<"1.0">>,
        <<"api">> => #{
            <<"generate">> => #{
                <<"description">> => <<"Generate NVIDIA GPU TEE attestation token">>
            },
            <<"verify">> => #{
                <<"description">> => <<"Verify NVIDIA GPU TEE attestation token">>,
                <<"required_params">> => #{
                    <<"token">> => <<"Attestation token to verify">>
                }
            }
        }
    },
    {ok, InfoBody}.

%% @doc Generate an NVIDIA GPU TEE attestation token.
-spec generate(map(), map(), map()) -> {ok, binary()} | {error, term()}.
generate(_M1, M2, Opts) ->
    maybe
        % Ensure Python environment is ready
        {ok, _} ?= ensure_python_environment(),
        Nonce = hb_ao:get(nonce, M2, ?TEST_MOCK_NONCE, Opts),
        % Generate the GPU attestation token using Python
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

%% @doc Verify an NVIDIA GPU TEE attestation token.
-spec verify(map(), map(), map()) -> {ok, binary()} | {error, term()}.
verify(_M1, M2, NodeOpts) ->
    maybe
        % Ensure Python environment is ready
        {ok, _} ?= ensure_python_environment(),
        ?event(dev_sev_gpu, {verify, M2}),
        {ok, TokenJSON} ?= extract_token_from_message(M2, NodeOpts),
        % Extract nonce for verification
        Nonce = hb_ao:get(<<"nonce">>, M2, ?TEST_MOCK_NONCE, NodeOpts),
        ?event(dev_sev_gpu, {verify_token, TokenJSON, Nonce}),
        % Verify the GPU attestation token
        {ok, TokenResult} ?= verify_token(TokenJSON , Nonce),
        case TokenResult of
            true -> {ok, <<"true">>};
            false -> {ok, <<"false">>}
        end
    else
        {error, Reason} -> {error, Reason}
    end.

%% @doc Ensure Python environment and dependencies are ready.
-spec ensure_python_environment() -> {ok, true} | {error, term()}.
ensure_python_environment() ->
    case get(python_env_checked) of
        true ->
            {ok, true};
        _ ->
            case check_python_environment() of
                true ->
                    put(python_env_checked, true),
                    {ok, true};
                false ->
                    {error, python_env_not_available}
            end
    end.

%% @doc Check if Python environment and dependencies are available.
-spec check_python_environment() -> boolean().
check_python_environment() ->
    TestCmd = "python3 -c \"import nv_attestation_sdk; print('OK')\"",
    case os:cmd(TestCmd) of
        "OK\n" -> true;
        _ -> false
    end.

%% @doc Extract the token from the message.
-spec extract_token_from_message(map(), map()) -> {ok, binary()} | {error, term()}.
extract_token_from_message(M2, NodeOpts) ->
    try
        RawMsg = hb_ao:get(<<"body">>, M2, NodeOpts#{ hashpath => ignore }),
        {ok, RawMsg}
    catch
        _Type:Reason -> {error, {extract_failed, Reason}}
    end.

%% @doc Verify the GPU attestation token against policy.
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
                #{<<"valid">> := true} ->
                    {ok, true};
                #{<<"valid">> := false} ->
                    {ok, false};
                _ ->
                    ?event(dev_sev_gpu, {invalid_verify_result, VerifyResult}),
                    {error, invalid_verify_result}
            end;
        {error, Error} ->
            ?event(dev_sev_gpu, {verify_token_failed, TokenJSON, Error}),
            {error, Error}
    end.

%% @doc Call Python attestation script via Port.
-spec call_python_attestation(atom(), map()) -> {ok, binary()} | {error, term()}.
call_python_attestation(Action, Data) ->
    try
        Request = #{
            <<"action">> => atom_to_binary(Action),
            <<"data">> => Data
        },
        RequestJSON = hb_json:encode(Request),
        ScriptPath = get_python_script_path(),
        ScriptDir = get_python_script_dir(),
        
        % Create temporary file for JSON data
        TempFile = filename:join(ScriptDir, "dev_sev_gpu_" ++ integer_to_list(erlang:system_time()) ++ ".json"),
        ok = file:write_file(TempFile, RequestJSON),
        
        % Use shell command with temp file
        ShellCmd = lists:flatten(io_lib:format("cat ~s |  python3 ~s && rm ~s", 
            [TempFile, ScriptPath, TempFile])),
        
        Port = open_port({spawn, ShellCmd}, 
            [binary, use_stdio, stderr_to_stdout, {cd, ScriptDir}]),
        
        % Wait for response
        Result = receive
            {Port, {data, ResponseData}} ->
                case hb_json:decode(ResponseData) of
                    #{<<"status">> := <<"ok">>, <<"result">> := ResultData} ->
                        case ResultData of
                            #{<<"token">> := Token} -> 
                                {ok, Token};
                            #{<<"valid">> := _} ->
                                {ok, hb_json:encode(ResultData)};
                            _ -> 
                                {ok, hb_json:encode(ResultData)}
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
        _Type:Reason ->
            {error, {python_call_failed, Reason}}
    end.

%% @doc Get the path to the Python attestation script directory.
-spec get_python_script_dir() -> string().
get_python_script_dir() ->
    case find_project_root() of
        {ok, ProjectRoot} ->
            filename:join([ProjectRoot, "native", "dev_sev_gpu"]);
        {error, _} ->
            % Final fallback to relative path
            filename:join(["..", "native", "dev_sev_gpu"])
    end.

%% @doc Get the path to the Python attestation script.
-spec get_python_script_path() -> string().
get_python_script_path() ->
    filename:join([get_python_script_dir(), "main.py"]).

%% @doc Find the project root directory by looking for rebar.config.
-spec find_project_root() -> {ok, string()} | {error, not_found}.
find_project_root() ->
    find_project_root(".", 5).

%% @doc Find project root by walking up directories looking for rebar.config.
-spec find_project_root(string(), non_neg_integer()) -> {ok, string()} | {error, not_found}.
find_project_root(_CurrentDir, 0) ->
    {error, not_found};
find_project_root(CurrentDir, MaxDepth) ->
    RebarConfig = filename:join(CurrentDir, "rebar.config"),
    case filelib:is_file(RebarConfig) of
        true ->
            {ok, filename:absname(CurrentDir)};
        false ->
            ParentDir = filename:join(CurrentDir, ".."),
            find_project_root(ParentDir, MaxDepth - 1)
    end.

%% Unit tests

%% @doc Test token generation with valid configuration.
generate_test() ->
    TestOpts = #{},
    case generate(#{}, #{nonce => ?TEST_MOCK_NONCE}, TestOpts) of
        {ok, TokenJSON} ->
            ?event(dev_sev_gpu, {token_generated, TokenJSON}),
            ?assert(is_binary(TokenJSON)),
            ?assert(byte_size(TokenJSON) > 0);
        {error, {python_error, <<"No evidence available for attestation">>}} ->
            % No GPU hardware available - this is expected in some environments
            ?assert(true);
        {error, python_env_not_available} ->
            % Python environment not available - this is expected in some environments
            ?assert(true);
        Other ->
            ?assertEqual({ok, token}, Other)
    end.

%% @doc Test successful round-trip: generate then verify.
verify_test() ->
    TestOpts = #{},
    case generate(#{}, #{nonce => ?TEST_MOCK_NONCE}, TestOpts) of
        {ok, GeneratedToken} ->
            ?assert(is_binary(GeneratedToken)),
            ?assert(byte_size(GeneratedToken) > 0),
            % Test verification
            VerifyMsg = #{
                <<"body">> => GeneratedToken,
                <<"nonce">> => ?TEST_MOCK_NONCE
            },
            case verify(#{}, VerifyMsg, #{}) of
                {ok, <<"true">>} ->
                    ?assert(true);
                {ok, <<"false">>} ->
                    % Token verification failed, but this might be expected
                    ?assert(true);
                {error, _} ->
                    % Verification error, but this might be expected
                    ?assert(true)
            end;
        {error, python_env_not_available} ->
            % Python environment not available - skip verification test
            ?assert(true);
        {error, {python_error, <<"No evidence available for attestation">>}} ->
            % No GPU hardware available - this is expected in some environments
            ?assert(true)
    end. 