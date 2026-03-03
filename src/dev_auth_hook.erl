%%% @doc A device offering an on-request hook that signs incoming messages with
%%% node-hosted wallets, in accordance with the node operator's configuration.
%%% It is intended for deployment in environments where a node's users have
%%% intrinsic reasons for trusting the node outside of the scope of this device.
%%% For example, if executed on a node running in a Trusted Execution Environment
%%% with `~snp@1.0', or a node they operate or is operated by a trusted
%%% third-party.
%%% 
%%% This device utilizes the `generator' interface type which other devices may
%%% implement. The generator is used to find/create a secret based on a user's
%%% request, which is then passed to the `~proxy-wallet@1.0' device and matched
%%% with a wallet which is used to sign the request. The `generator' interface
%%% may implement the following keys:
%%% 
%%% <pre>
%%%     `generate' (optional): A key that generates a secret based on a
%%%                            user's request. May return either the secret
%%%                            directly, or a message with a `secret' key. If 
%%%                            a message is returned, it is assumed to be a
%%%                            modified version of the user's request and is
%%%                            used for further processing.
%%%     `finalize' (optional): A key that takes the message sequence after this
%%%                            device has processed it and returns it in a
%%%                            modified form.
%%% </pre>
%%% 
%%% At present, the `~cookie-secret@1.0' and `~http-auth@1.0' devices implement
%%% the `generator' interface. For example, the following hook definition will
%%% use the `~cookie-secret@1.0' device to generate and manage wallets for
%%% users, with authentication details stored in cookies:
%%% 
%%% <pre>
%%%   "on": {
%%%     "request": {
%%%       "device": "auth-hook@1.0",
%%%       "secret-provider": {
%%%         "device": "cookie-secret@1.0"
%%%       }
%%%     }
%%%   }
%%% </pre>
%%% 
%%% `~auth-hook@1.0' expects to receive a `secret-provider' key in the hook
%%% base message. It may optionally also take a `generate-path' and
%%% `finalize-path', which are used to generate the secret and post-process the
%%% response. If either `X-path' keys are not present, the `generate' and
%%% `finalize' paths are used upon the `secret-provider' message. If the secret
%%% provider's device does not implement these keys, the operations are skipped.
%%% 
%%% Node operators may also specify a `when' message inside their hook definition
%%% which is used to determine when messages should be signed. The supported keys
%%% are:
%%% 
%%% <pre>
%%%     `committers': always | uncommitted | [committer1, or committer2, or ...]
%%%     `keys': always | [key1, or key2, or ...]
%%% </pre>
%%% 
%%% Both keys are optional and can be combined to form 'and' conditions. For
%%% example, the following hook definition will sign all uncommitted requests
%%% that have the `Authorization' header:
%%% 
%%% <pre>
%%%   "on": {
%%%     "request": {
%%%       "device": "auth-hook@1.0",
%%%       "when": {
%%%             "keys": ["authorization"],
%%%             "committers": "uncommitted"
%%%         }
%%%       }
%%%     }
%%% </pre>
%%% 
-module(dev_auth_hook).
-export([request/3]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% Default key used to indicate that an individual message in the path should
%%% be signed.
-define(DEFAULT_COMMIT_KEY, <<"!">>).

%%% Default keys to ignore when signing
-define(DEFAULT_IGNORED_KEYS,
    [
        <<"secret">>,
        <<"cookie">>,
        <<"set-cookie">>,
        <<"path">>,
        <<"method">>,
        <<"authorization">>,
        ?DEFAULT_COMMIT_KEY
    ]
).

%% @doc Process an incoming request through a key provider. The key provider
%% should be a message optionally implementing the following keys:
%% <pre>
%%     `generate-path': The path to call the `generate' function.
%%     `finalize-path': The path to call the `finalize' function.
%%     `skip-commit': Whether to skip committing the request.
%%     `ignored-keys': A list of keys to ignore when signing (can be overridden
%%     by the user request).
%% </pre>
%% 
request(Base, HookReq, Opts) ->
    ?event({auth_hook_request, {base, Base}, {hook_req, HookReq}}),
    maybe
        % Get the key provider from options and short-circuit if none is
        % provided.
        {ok, Provider} ?= find_provider(Base, Opts),
        % Check if the request already has signatures, or the hook base enforces
        % that we should always attempt to sign the request.
        {ok, Request} ?= hb_maps:find(<<"request">>, HookReq, Opts),
        {ok, OrigMessages} ?= hb_maps:find(<<"body">>, HookReq, Opts),
        true ?= is_relevant(Base, Request, OrigMessages, Opts),
        ?event(auth_hook_is_relevant),
        % Call the key provider to normalize authentication (generate if needed)
        {ok, IntermediateProvider, NormReq} ?=
            generate_secret(Provider, Request, Opts),
        % Call `~secret@1.0' to generate a wallet if needed. Returns refreshed
        % options.
        {ok, NormProvider, NewOpts} ?=
            generate_wallet(IntermediateProvider, NormReq, Opts),
        ?event(
            {auth_hook_normalized,
                {intermediate_provider, IntermediateProvider},
                {norm_provider, NormProvider},
                {norm_req, NormReq}
            }
        ),
        % Sign the full request  
        {ok, SignedReq} ?= sign_request(NormProvider, NormReq, NewOpts),
        ?event(auth_hook_signed),
        % Process individual messages if needed
        {ok, MessageSequence} ?=
            maybe_sign_messages(
                NormProvider,
                SignedReq,
                NewOpts
            ),
        ?event(auth_hook_processed_messages),
        % Call the key provider to finalize the response
        {ok, FinalSequence} ?=
            finalize(
                NormProvider,
                SignedReq,
                MessageSequence,
                NewOpts
            ),
        ?event({auth_hook_returning, FinalSequence}),
        {ok, #{ <<"body">> => FinalSequence, <<"request">> => SignedReq }}
    else
        {error, AuthError} ->
            ?event({auth_hook_auth_error, AuthError}),
            {error, AuthError};
        {skip, {committers, Committers}, {keys, Keys}} ->
            ?event({auth_hook_skipping, {committers, Committers}, {keys, Keys}}),
            {ok, HookReq};
        error ->
            ?event({auth_hook_error, no_request}),
            {ok, HookReq};
        Other ->
            ?event({auth_hook_unexpected_result, Other}),
            Other
    end.

%% @doc Check if the request is relevant to the hook base. Node operators may
%% specify criteria for activation of the hook based on the committers of the
%% request (`always', `uncommitted', or a list of committers), or the presence
%% of certain keys (`always', or a list of keys) on any of the messages in the
%% sequence.
is_relevant(Base, Request, MessageSequence, Opts) ->
    Committers = is_relevant_from_committers(Base, Request, Opts),
    Keys =
        lists:any(
            fun(Msg) -> is_relevant_from_keys(Base, Msg, Opts) end,
            [Request | MessageSequence]
        ),
    ?event({auth_hook_is_relevant, {committers, Committers}, {keys, Keys}}),
    if Committers andalso Keys -> true;
        true -> {skip, {committers, Committers}, {keys, Keys}}
    end.

%% @doc Check if the request is relevant to the hook base based on the committers
%% of the request.
is_relevant_from_committers(Base, Request, Opts) ->
    Config =
        hb_util:deep_get(
            [<<"when">>, <<"committers">>],
            Base,
            <<"uncommitted">>,
            Opts
        ),
    ?event({auth_hook_is_relevant_from_committers, {config, Config}, {base, Base}}),
    case Config of
        <<"always">> -> true;
        <<"uncommitted">> -> hb_message:signers(Request, Opts) == [];
        RelevantCommitters ->
            lists:any(
                fun(Signer) ->
                    lists:member(Signer, RelevantCommitters)
                end,
                hb_message:signers(Request, Opts)
            )
    end.

%% @doc Check if the request is relevant to the hook base based on the presence
%% of keys specified in the hook base.
is_relevant_from_keys(_Base, ID, _Opts) when is_binary(ID) ->
    false;
is_relevant_from_keys(Base, {as, _, Msg}, Opts) ->
    is_relevant_from_keys(Base, Msg, Opts);
is_relevant_from_keys(Base, {resolve, Msg}, Opts) ->
    is_relevant_from_keys(Base, Msg, Opts);
is_relevant_from_keys(Base, Request, Opts) ->
    Config = hb_util:deep_get([<<"when">>, <<"keys">>], Base, <<"always">>, Opts),
    ?event(
        {
            auth_hook_is_relevant_from_keys,
            {config, Config},
            {base, Base},
            {request, Request}
        }
    ),
    case Config of
        <<"always">> -> true;
        RelevantKeys ->
            lists:any(
                fun(Key) ->
                    case hb_maps:find(Key, Request, Opts) of
                        {ok, _} -> true;
                        error -> false
                    end
                end,
                RelevantKeys
            )
    end.

%% @doc Normalize authentication credentials, generating new ones if needed.
generate_secret(Provider, Request, Opts) ->
    case call_provider(<<"generate">>, Provider, Request, Opts) of
        {error, not_found} ->
            ?event({no_generate_handler, Provider}),
            {ok, Provider, strip_sensitive(Request, Opts)};
        {error, Err} ->
            % Forward the error. The main handler will fail to match this and
            % return the error to the user.
            ?event({generate_error, Err}),
            {error, Err};
        {ok, Secret} when is_binary(Secret) ->
            % The provider returned a direct key, calculate the committer and
            % generate a wallet for it, if needed.
            ?event({secret_from_provider, Secret}),
            {ok, Provider#{ <<"secret">> => Secret }, strip_sensitive(Request, Opts)};
        {ok, NormalizedReq} when is_map(NormalizedReq) ->
            % If there is a `wallet' field in the request, we move it to the
            % provider, else continue with the existing provider.
            ?event({normalized_req, NormalizedReq}),
            case hb_maps:find(<<"secret">>, NormalizedReq, Opts) of
                {ok, Key} ->
                    ?event({key_found_in_normalized_req, Key}),
                    {
                        ok,
                        Provider#{ <<"secret">> => Key },
                        strip_sensitive(NormalizedReq, Opts)
                    };
                error ->
                    ?event({no_key_in_normalized_req, NormalizedReq}),
                    {ok, Provider, strip_sensitive(NormalizedReq, Opts)}
            end
    end.

%% @doc Strip the `secret' field from a request.
strip_sensitive(Request, Opts) ->
    hb_maps:without([<<"secret">>], Request, Opts).

%% @doc Generate a wallet with the key if the `wallet' field is not present in
%% the provider after normalization.
generate_wallet(Provider, Request, Opts) ->
    {ok, #{ <<"body">> := WalletID }} =
        dev_secret:generate(Provider, Request, Opts),
    ?event({generated_wallet, WalletID}),
    {ok, Provider, refresh_opts(Opts)}.

%% @doc Sign a request using the configured key provider
sign_request(Provider, Msg, Opts) ->
    case hb_maps:get(<<"skip-commit">>, Provider, true, Opts) of
        false ->
            % Skip signing and return the normalized message.
            ?event({provider_requested_signing_skip, Provider}),
            {ok, Msg};
        true ->
            % Wallet signs without ignored keys
            IgnoredKeys = ignored_keys(Msg, Opts),
            WithoutIgnored = hb_maps:without(IgnoredKeys, Msg, Opts),
            % Call the wallet to sign the request.
            case dev_secret:commit(WithoutIgnored, Provider, Opts) of
                {ok, Signed} ->
                    ?event({auth_hook_signed, Signed}),
                    SignedWithIgnored = 
                        hb_maps:merge(
                            Signed,
                            hb_maps:with(IgnoredKeys, Msg, Opts),
                            Opts
                        ),
                    {ok, SignedWithIgnored};
                {error, Err} ->
                    ?event({auth_hook_sign_error, Err}),
                    {error, Err}
            end
    end.

%% @doc Process a sequence of messages, signing those marked for signing
maybe_sign_messages(Provider, SignedReq, Opts) ->
    Parsed = hb_singleton:from(SignedReq, Opts),
    ?event({auth_hook_parsed_messages, {sequence_length, length(Parsed)}}),
    SignKey = hb_opts:get(auth_hook_commit_key, ?DEFAULT_COMMIT_KEY, Opts),
    Processed = maybe_sign_messages(Provider, SignKey, Parsed, Opts),
    {ok, Processed}.
maybe_sign_messages(_Provider, _Key, [], _Opts) -> [];
maybe_sign_messages(Provider, Key, [Msg | Rest], Opts) when is_map(Msg) ->
    case hb_util:atom(hb_maps:get(Key, Msg, false, Opts)) of
        true ->
            Uncommitted = hb_message:uncommitted(Msg, Opts),
            ?event({auth_hook_signing_message, {uncommitted, Msg}}),
            case sign_request(Provider, Uncommitted, Opts) of
                {ok, Signed} ->
                    [
                        Signed
                    |
                        maybe_sign_messages(Provider, Key, Rest, Opts)
                    ];
                {error, Err} ->
                    ?event({auth_hook_sign_error, Err}),
                    [{error, Err}]
            end;
        _ ->
            [Msg | maybe_sign_messages(Provider, Key, Rest, Opts)]
    end;
maybe_sign_messages(Provider, Key, [Msg | Rest], Opts) ->
    [Msg | maybe_sign_messages(Provider, Key, Rest, Opts)].

%% @doc Finalize the response by adding authentication state
finalize(KeyProvider, SignedReq, MessageSequence, Opts) ->
    % Add the signed request and message sequence to the response, mirroring the
    % structure of a normal `~hook@1.0' on-request hook.
    Req =
        #{
            <<"request">> => SignedReq,
            <<"body">> => MessageSequence
        },
    case call_provider(<<"finalize">>, KeyProvider, Req, Opts) of
        {ok, Finalized} ->
            ?event({auth_hook_finalized, Finalized}),
            {ok, Finalized};
        {error, not_found} ->
            ?event(auth_hook_no_finalize_handler),
            {ok, MessageSequence}
    end.

%%% Utility functions

%% @doc Refresh the options and log an event if they have changed.
refresh_opts(Opts) ->
    NewOpts = hb_http_server:get_opts(Opts),
    case NewOpts of
        Opts -> ?event(auth_hook_no_opts_change);
        _ ->
            ?event(
                {auth_hook_opts_changed,
                    {size_diff,
                        erlang:external_size(NewOpts) -
                            erlang:external_size(Opts)
                    }
                }
            )
    end,
    NewOpts.

%% @doc Get the key provider from the base message or the defaults.
find_provider(Base, Opts) ->
    case hb_maps:get(<<"secret-provider">>, Base, no_key_provider, Opts) of
        no_key_provider ->
            case hb_opts:get(hook_secret_provider, no_key_provider, Opts) of
                no_key_provider -> {error, no_key_provider};
                SecretProvider -> SecretProvider
            end;
        SecretProvider when is_binary(SecretProvider) ->
            {ok, #{ <<"device">> => SecretProvider }};
        SecretProvider when is_map(SecretProvider) ->
            {ok, SecretProvider};
        _ ->
            {error, invalid_auth_provider}
    end.

%% @doc Find the appropriate handler for a key in the key provider.
call_provider(Key, Provider, Request, Opts) ->
    ?event({call_provider, {key, Key}, {provider, Provider}, {req, Request}}),
    ExecKey = hb_maps:get(<< Key/binary, "-path">>, Provider, Key, Opts),
    ?event({call_provider, {exec_key, ExecKey}}),
    case hb_ao:resolve(Provider, Request#{ <<"path">> => ExecKey }, Opts) of
        {ok, Msg} when is_map(Msg) ->
            % The result is a message. We revert the path to its original value.
            case hb_maps:find(<<"path">>, Request, Opts) of
                {ok, Path} -> {ok, Msg#{ <<"path">> => Path }};
                _ -> {ok, Msg}
            end;
        {ok, _} = Res ->
            % The result is a non-message. We return it as-is.
            Res;
        {error, Err} ->
            ?event({call_provider_error, Err}),
            {error, Err}
    end.

%% @doc Default keys to ignore when signing
ignored_keys(Msg, Opts) ->
    hb_maps:get(
        <<"ignored-keys">>,
        Msg,
        hb_opts:get(
            hook_auth_ignored_keys,
            ?DEFAULT_IGNORED_KEYS,
            Opts
        )
    ).

%%% Tests

cookie_test() ->
    % Start a node with a secret-provider that uses the cookie device.
    Node =
        hb_http_server:start_node(
            #{
                priv_wallet => ServerWallet = ar_wallet:new(),
                on => #{
                    <<"request">> => #{
                        <<"device">> => <<"auth-hook@1.0">>,
                        <<"path">> => <<"request">>,
                        <<"secret-provider">> =>
                            #{
                                <<"device">> => <<"cookie@1.0">>
                            }
                    }
                }
            }
        ),
    % Run a request and check that the response is signed. The cookie device
    % will generate a new cookie for the client.
    {ok, Response} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"commitments">>,
                <<"body">> => <<"Test data">>
            },
            #{}
        ),
    % Filter the response to only include signed commitments.
    Signers = signers_from_commitments_response(Response, ServerWallet),
    ?event(
        {response, {found_signers, Signers}}
    ),    
    ?assertEqual(1, length(Signers)),
    % Generate a further request and check that the same address is used. Extract
    % the cookie given in the first request and use it to sign the second.
    [CookieAddress] = Signers,
    #{ <<"priv">> := CookiePriv } = Response,
    ?event(
        {cookie_from_response,
            {cookie_priv, CookiePriv},
            {cookie_address, CookieAddress}
        }
    ),
    {ok, Response2} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/commitments">>,
                <<"body">> => <<"Test data2">>,
                <<"priv">> => CookiePriv
            },
            #{}
        ),
    % Check that the second request is signed with the same address as the first.
    ?assertEqual(
        [CookieAddress],
        signers_from_commitments_response(Response2, ServerWallet)
    ).

http_auth_test() ->
    % Start a node with the `~http-auth@1.0' device as the secret-provider.
    Node =
        hb_http_server:start_node(
            #{
                priv_wallet => ServerWallet = ar_wallet:new(),
                on => #{
                    <<"request">> => #{
                        <<"device">> => <<"auth-hook@1.0">>,
                        <<"path">> => <<"request">>,
                        <<"secret-provider">> =>
                            #{
                                <<"device">> => <<"http-auth@1.0">>,
                                <<"access-control">> =>
                                    #{ <<"device">> => <<"http-auth@1.0">> }
                            }
                    }
                }
            }
        ),
    % Run a request and check that the response is a 401 with the
    % `www-authenticate' header.
    Resp1 =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"commitments">>,
                <<"body">> => <<"Test data">>
            },
            #{}
        ),
    ?assertMatch(
        {error, #{ <<"status">> := 401, <<"www-authenticate">> := _ }},
        Resp1
    ),
    % Run a request with the `Authorization' header and check that the response
    % is signed.
    AuthStr = << "Basic ", (base64:encode(<<"user:pass">>))/binary >>,
    Resp2 =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"commitments">>,
                <<"body">> => <<"Test data">>,
                <<"authorization">> => AuthStr
            },
            #{}
        ),
    ?assertMatch(
        {ok, #{ <<"status">> := 200 }},
        Resp2
    ),
    % Filter the response to only include signed commitments.
    Signers = signers_from_commitments_response(hb_util:ok(Resp2), ServerWallet),
    ?event(
        {response, {found_signers, Signers}}
    ),
    ?assertEqual(1, length(Signers)),
    % Generate a further request and check that the same address is used.
    [Signer] = Signers,
    {ok, Resp3} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"commitments">>,
                <<"body">> => <<"Test data2">>,
                <<"authorization">> => AuthStr
            },
            #{}
        ),
    ?assertEqual(
        [Signer],
        signers_from_commitments_response(Resp3, ServerWallet)
    ).

chained_preprocess_test() ->
    % Start a node with the `~http-auth@1.0' device as the secret-provider, with
    % a router chained afterwards in the request hook.
    RelayWallet = ar_wallet:new(),
    RelayAddress = hb_util:human_id(RelayWallet),
    RelayURL = hb_http_server:start_node(#{ priv_wallet => RelayWallet }),
    Node =
        hb_http_server:start_node(
            #{
                priv_wallet => ar_wallet:new(),
                relay_allow_commit_request => true,
                on => #{
                    <<"request">> =>
                        [
                            #{
                                <<"device">> => <<"auth-hook@1.0">>,
                                <<"path">> => <<"request">>,
                                <<"secret-provider">> =>
                                    #{
                                        <<"device">> => <<"http-auth@1.0">>,
                                        <<"access-control">> =>
                                            #{
                                                <<"device">> => <<"http-auth@1.0">>
                                            }
                                    }
                            },
                            #{
                                <<"device">> => <<"router@1.0">>,
                                <<"path">> => <<"preprocess">>,
                                <<"commit-request">> => true
                            }
                        ]
                },
                routes => [
                    #{
                        <<"template">> => <<"/~meta@1.0/info/address">>,
                        <<"node">> => #{ <<"prefix">> => RelayURL }
                    }
                ]
            }
        ),
    % Run a request with the `Authorization' header and check that the response
    % is signed.
    AuthStr = << "Basic ", (base64:encode(<<"user:pass">>))/binary >>,
    Resp1 =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"/~meta@1.0/info/address">>,
                <<"authorization">> => AuthStr
            },
            #{}
        ),
    ?assertMatch({ok, RelayAddress}, Resp1).

when_test() ->
    % Start a node with the `~http-auth@1.0' device as the secret-provider. Only
    % request commitment with the hook if the `Authorization' header is present.
    Node =
        hb_http_server:start_node(
            #{
                priv_wallet => ServerWallet = ar_wallet:new(),
                on => #{
                    <<"request">> => #{
                        <<"device">> => <<"auth-hook@1.0">>,
                        <<"path">> => <<"request">>,
                        <<"when">> => #{
                            <<"keys">> => [<<"authorization">>]
                        },
                        <<"secret-provider">> =>
                            #{
                                <<"device">> => <<"http-auth@1.0">>,
                                <<"access-control">> =>
                                    #{ <<"device">> => <<"http-auth@1.0">> }
                            }
                    }
                }
            }
        ),
    % Run a request and check that the response is not signed, but is `status: 200'.
    {ok, Resp1} =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"~meta@1.0/info">>,
                <<"body">> => <<"Test data">>
            },
            #{}
        ),
    ?assertEqual(200, hb_maps:get(<<"status">>, Resp1, 0)),
    % Run a request with the `Authorization' header and check that the response
    % is signed.
    AuthStr = << "Basic ", (base64:encode(<<"user:pass">>))/binary >>,
    Resp2 =
        hb_http:get(
            Node,
            #{
                <<"path">> => <<"commitments">>,
                <<"body">> => <<"Test data">>,
                <<"authorization">> => AuthStr
            },
            #{}
        ),
    ?assertMatch(
        {ok, #{ <<"status">> := 200 }},
        Resp2
    ),
    ?assertMatch(
        [_],
        signers_from_commitments_response(
            hb_util:ok(Resp2),
            ServerWallet
        )
    ).

%% @doc The cookie hook test(s) call `GET /commitments', which returns the 
%% commitments found on the client request during execution on the server.
%% This function filters the response to return only the signers of that message,
%% excluding the server's own signature.
signers_from_commitments_response(Response, ServerWallet) ->
    ServerAddress = ar_wallet:to_address(ServerWallet),
    hb_maps:values(hb_maps:filtermap(
        fun(Key, Value) when ?IS_ID(Key) ->
            Type = hb_maps:get(<<"type">>, Value, not_found, #{}),
            Committer = hb_maps:get(<<"committer">>, Value, not_found, #{}),
            case {Type, Committer} of
                {<<"rsa-pss-sha512">>, ServerAddress} -> false;
                {<<"rsa-pss-sha512">>, _} -> {true, Committer};
                _ -> false
            end;
           (_Key, _Value) ->
            false
        end,
        Response,
        #{}
    )).