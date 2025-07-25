%%% @moduledoc Implements the authentication mechanisms of the cookie codec.
%%% See the [cookie codec](dev_codec_cookie.html) documentation for more details.
-module(dev_codec_cookie_auth).
-include_lib("eunit/include/eunit.hrl").
-include("include/hb.hrl").
-export([commit/3, verify/3]).

%% @doc Generate a new secret, nonce, and name (secret reference). See the
%% module documentation of `dev_codec_cookie' for more details on its scheme.
commit(Base, Request, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    % Generate a random nonce.
    Secret = crypto:strong_rand_bytes(32),
    Name =
        case hb_ao:get(<<"name">>, Request, Opts) of
            not_found ->
                % If no name is provided, use the hash of the secret such that
                % providing the secret could be used to verify the caller and
                % re-calculate the name if necessary.
                hb_util:human_id(crypto:hash(sha256, Secret));
            N ->
                % If the name is provided use it as a salt for the secret.
                N
        end,
    Commitment = calculate_commitment(Secret, Name, Opts),
    % Create the cookie parameters, omitting the keys specified in the `omit'
    % parameter if any.
    CookieParams =
        hb_maps:without(
            case hb_ao:get(<<"omit">>, Request, Opts) of
                not_found -> [];
                KeyList when is_list(KeyList) ->
                    lists:map(fun hb_util:bin/1, KeyList);
                Key ->
                    [hb_util:bin(Key)]
            end,
            #{
                <<"name">> => Name,
                <<"secret">> => hb_util:human_id(Secret),
                <<"commitment">> => hb_util:human_id(Commitment)
            },
            Opts
        ),
    % Set the cookie on the base message and return the name.
    dev_codec_cookie:set(Base, CookieParams, Opts).

%% @doc Verify the cookie in the request message against the cookie in the base
%% message. If a name key is provided in the request message, it is used instead
%% of the name key in the request's cookie. This allows a caller to take a
%% user-given message containing a cookie and verify it was created using a
%% specific name during the `generate' call. The request message is returned
%% with the cookie removed.
verify(RawBase, Request, RawOpts) ->
    Opts = dev_codec_cookie:opts(RawOpts),
    % If the `cookie' key is not set in the base, but the `set-cookie' key is,
    % we swap it.
    Base =
        case hb_maps:get(<<"cookie">>, RawBase, not_found, Opts) of
            not_found ->
                RawBase#{
                    <<"cookie">> =>
                        hb_maps:get(<<"set-cookie">>, RawBase, <<>>, Opts)
                };
            _ ->
                RawBase
        end,
    ?event({verify, {base, Base}, {request, Request}}),
    maybe
        % Parse the commitment from the base message.
        {ok, BaseCookieMsg} = dev_codec_cookie:from(Base, Request, RawOpts),
        {ok, ExpectedCommitment} ?=
            hb_maps:find(
                <<"commitment">>,
                BaseCookieMsg,
                Opts
            ),
        % Parse the secret and name from the request message, favoring the name
        % provided in the request.
        {ok, RequestCookieMsg} = dev_codec_cookie:from(Request, Request, RawOpts),
        {ok, EncSecret} ?= hb_maps:find(<<"secret">>, RequestCookieMsg, Opts),
        Secret = hb_util:decode(EncSecret),
        {ok, Name} ?=
            case hb_maps:find(<<"name">>, Request, Opts) of
                error -> hb_maps:find(<<"name">>, RequestCookieMsg, Opts);
                N -> N
            end,
        ExpectedCommitment ?= calculate_commitment(Secret, Name, Opts),
        dev_codec_cookie:without(Request, Opts)
    else
        _ ->
            % If any of the above patterns fail, return a failure.
            {error, <<"false">>}
    end.

%% @doc Calculate the commitment for a given secret and name.
calculate_commitment(Secret, Name, _Opts) ->
    hb_util:human_id(crypto:hash(sha256, <<Secret/binary, Name/binary>>)).


%%% Tests

%% @doc Set keys in a cookie and verify that they can be parsed into a message.
set_cookie_test() ->
    Node = hb_http_server:start_node(#{}),
    {ok, SetRes} =
        hb_http:get(
            Node,
            <<"/~cookie@1.0&normal=keyval/set?k1=v1&k2=v2">>,
            #{}
        ),
    ?assertMatch(#{ <<"set-cookie">> := _, <<"normal">> := <<"keyval">> }, SetRes),
    Req = apply_cookie(#{ <<"path">> => <<"/~cookie@1.0/from">> }, SetRes, #{}),
    {ok, Res} = hb_http:get(Node, Req, #{}),
    ?assertMatch(#{ <<"k1">> := <<"v1">>, <<"k2">> := <<"v2">> }, Res),
    ok.

%% @doc Generate a secret cookie with a random name and verify it. Ensure that
%% the returned message has the cookie removed.
generate_verify_test() ->
    Node = hb_http_server:start_node(#{}),
    % Generate a secret cookie with a random name.
    {ok, Base} = hb_http:get(Node, <<"/~cookie@1.0/commit">>, #{}),
    VerifyReq =
        apply_cookie(
            #{ <<"path">> => <<"/~cookie@1.0/verify">> },
            Base,
            #{}
        ),
    % Verify the secret cookie. `verify' will return the request message with the
    % cookie removed if the secret verifies.
    {ok, Res} = hb_http:get(Node, VerifyReq, #{}),
    assert_valid_verification(Res, #{}),
    ok.

%% @doc Generate a secret cookie with a specific name and verify it. We also
%% verify that if the wrong name is provided, the verification fails.
generate_verify_with_name_test() ->
    Node = hb_http_server:start_node(#{}),
    % Generate a secret cookie with a specific name and the base verification
    % request.
    {ok, Base} = hb_http:get(Node, <<"/~cookie@1.0/commit?name=correct">>, #{}),
    VerifyReq =
        apply_cookie(
            #{ <<"path">> => <<"/~cookie@1.0/verify">> },
            Base,
            #{}
        ),
    % Verify that providing no name (letting the cookie name be used) succeeds.
    {ok, Res} = hb_http:get(Node, VerifyReq, #{}),
    assert_valid_verification(Res, #{}),
    % Verify that providing the correct name succeeds.
    {ok, Res2} = hb_http:get(Node, VerifyReq#{ <<"name">> => <<"correct">> }, #{}),
    assert_valid_verification(Res2, #{}),
    % Verify that the wrong name fails.
    {Status, _} = hb_http:get(Node, VerifyReq#{ <<"name">> => <<"wrong">> }, #{}),
    ?assertNotEqual(ok, Status),
    ok.

%% @doc Generate a secret cookie with a specified name, which must be ommitted
%% from the cookie granted to the caller. Another device may use a cookie 
%% generated in this way to be able to verify the caller's identity, without the
%% caller being able to know their own name.
generate_verify_hidden_name_test() ->
    Node = hb_http_server:start_node(#{}),
    % Generate a secret cookie with a specific name, which should not be included
    % in the cookie granted to the caller.
    {ok, Base} =
        hb_http:get(
            Node,
            <<"/~cookie@1.0/commit?name=invisible&omit=name">>,
            #{}
        ),
    VerifyReq =
        apply_cookie(
            #{ <<"path">> => <<"/~cookie@1.0/verify">> },
            Base,
            #{}
        ),
    {ok, ParsedReq} = dev_codec_cookie:from(VerifyReq, #{}, #{}),
    % Ensure that the given cookie contains a secret, but no name.
    ?assertMatch(#{ <<"secret">> := _ }, ParsedReq),
    ?assertNotMatch(#{ <<"name">> := _ }, ParsedReq),
    % Verify that the cookie cannot be used without the name being added.
    {Status, _} = hb_http:get(Node, VerifyReq, #{}),
    ?assertNotEqual(ok, Status),
    % Verify that providing the correct name succeeds.
    {ok, Res2} = hb_http:get(Node, VerifyReq#{ <<"name">> => <<"invisible">> }, #{}),
    assert_valid_verification(Res2, #{}),
    ok.

%%% Test Helpers

%% @doc Takes the cookies from the `GenerateResponse' and applies them to the
%% `Target' message.
apply_cookie(Target, GenerateResponse, Opts) ->
    ?event({apply_cookie, {target, Target}, {generate, GenerateResponse}}),
    {ok, Cookie} = hb_maps:find(<<"set-cookie">>, GenerateResponse, Opts),
    hb_ao:set(
        Target,
        #{ <<"cookie">> => Cookie, <<"set-cookie">> => unset },
        dev_codec_cookie:opts(Opts)
    ).

%% @doc Assert that the response is a valid verification.
assert_valid_verification(Res, Opts) ->
    ?assertNotEqual(<<"false">>, Res),
    ?assertNot(lists:member(<<"set-cookie">>, hb_maps:keys(Res, Opts))),
    ?assertNot(lists:member(<<"cookie">>, hb_maps:keys(Res, Opts))),
    ok.