%%% @doc Authorization for typed operations on a node. Only keys committed by
%%% the requested role are returned to the caller.
-module(lib_meta).
-export([is_authorized/4, authorized/3, role/2, without_credentials/2]).
-export([ratcheted_apply/5]).
-include("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").

%% @doc Verify the role's commitments and require their signed operation type.
%% The admin defaults to the operator, which defaults to the node wallet.
is_authorized(Type, Role, Msg, Opts) when Role == operator; Role == admin ->
    maybe
        {ok, Authorized} ?= authorized(role(Role, Opts), Msg, Opts),
        true ?=
            Type == hb_maps:get(<<"type">>, Authorized, not_found, Opts) orelse
                {error, not_authorized},
        {ok, Authorized}
    end.

%% @doc Return the keys of a message that the given committers signed, after
%% verifying their commitments. For an HTTP request, we read the original
%% request from `priv/http-request', before its path was split into messages.
authorized(Committers, Msg, Opts) ->
    try
        maybe
            Loaded = hb_cache:ensure_loaded(Msg, Opts),
            Identities = committers(Committers, Opts),
            % Retain signed inputs so their computations cannot be cached.
            [_ | _] ?= hb_message:signers(
                hb_message:with_only_committers(Loaded, Identities, Opts), Opts
            ),
            Original = hb_private:get(<<"http-request">>, Loaded, Loaded, Opts),
            false ?= lib_process:is_pushed(Original, Opts),
            Signed =
                hb_message:with_only_committers(Original, Identities, Opts),
            [_ | _] ?= hb_message:signers(Signed, Opts),
            Keys = signed_keys(Signed, Opts),
            % A signature that covers a `hashpath' attests a result.
            false ?= hb_maps:is_key(<<"hashpath">>, Keys, Opts),
            % A signature on a `Relay-Request' attests a relayed request.
            false ?=
                hb_maps:get(<<"type">>, Keys, undefined, Opts)
                    == <<"Relay-Request">>,
            {ok, Keys}
        else
            _ -> {error, not_authorized}
        end
    catch
        Class:Reason:Stack ->
            ?event(meta_authorization,
                {authorization_error, Class, Reason, {trace, Stack}}
            ),
            {error, not_authorized}
    end.

%% @doc Apply a signed control once. `Control' has only the keys that its
%% signer signed. If it gives an integer `created' time, `Fun' runs only if
%% that time is newer than the time kept at `Path/<signer>' in the node's local
%% store, and the time is kept only if `Fun' gives `{ok, _}', so a control that
%% fails can be sent again. Controls for one signer and path run one at a time,
%% and a node that restarts reads the kept time with the next control. A
%% control without a `created' time runs as it is.
ratcheted_apply(Path, Signer, Control, Fun, Opts) ->
    Key = <<Path/binary, "/", Signer/binary>>,
    case hb_util:safe_int(hb_maps:get(<<"created">>, Control, none, Opts)) of
        {ok, Time} ->
            hb_name:atomic(
                {?MODULE, Key},
                fun() -> ratchet(Key, Time, Fun, Opts) end
            );
        {error, _} -> Fun()
    end.

%% @doc Call `Fun' if `Time' is newer than the time kept under `Key', and keep
%% `Time' if it succeeds.
ratchet(Key, Time, Fun, Opts) ->
    Store = hb_store:scope(hb_opts:get(store, no_viable_store, Opts), local),
    Last =
        case hb_store:read(Store, Key, Opts) of
            {ok, Kept} -> hb_util:int(Kept);
            {error, not_found} -> none
        end,
    maybe
        true ?=
            Last == none orelse Time > Last orelse
                {error, #{
                    <<"status">> => 403,
                    <<"body">> =>
                        <<"The control is not newer than the last one.">>
                }},
        {ok, _} = Result ?= Fun(),
        ok = hb_store:write(Store, #{ Key => integer_to_binary(Time) }, Opts),
        Result
    end.

%% @doc Return the configured identities for a role. Unclaimed roles deny access.
role(admin, Opts) ->
    committers(hb_opts:get(admin, role(operator, Opts), Opts), Opts);
role(operator, Opts) ->
    committers(hb_opts:get(operator, hb_opts:committer(Opts), Opts), Opts).

%% @doc Accept a role's committer or list of committers, in any form that
%% `hb_committer:uri/2' reads.
committers(Committers, Opts) when is_list(Committers) ->
    [hb_committer:uri(C, Opts) || C <- Committers];
committers(undefined, _Opts) -> [];
committers(Committer, Opts) -> [hb_committer:uri(Committer, Opts)].

%% @doc Follow committed children without admitting their uncommitted fields.
signed_keys(Msg, Opts) when is_map(Msg) ->
    true = hb_message:verify(Msg, #{ <<"ids">> => <<"all">> }, Opts),
    {ok, Committed} = hb_message:with_only_committed(Msg, Opts),
    Authorized =
        hb_maps:map(
            fun(<<"commitments">>, Commitments) -> Commitments;
               (_, Value) -> signed_keys(Value, Opts)
            end,
            Committed,
            Opts
        ),
    % Loading and projecting a child must preserve its parent's signature.
    true = hb_message:verify(Authorized, #{ <<"ids">> => <<"all">> }, Opts),
    Authorized;
signed_keys(Link, Opts) when ?IS_LINK(Link) ->
    signed_keys(hb_cache:ensure_loaded(Link, Opts), Opts);
signed_keys(List, Opts) when is_list(List) ->
    [signed_keys(Value, Opts) || Value <- List];
signed_keys(Value, _Opts) -> Value.

%% @doc Remove the credentials that the node's options give in their values,
%% for a view of the options that any caller reads: an `authorization' header,
%% and the user information of a URL.
without_credentials(Msg, Opts) when is_map(Msg) ->
    hb_maps:map(
        fun(_, Value) -> without_credentials(Value, Opts) end,
        hb_maps:without([<<"authorization">>], Msg, Opts),
        Opts
    );
without_credentials(List, Opts) when is_list(List) ->
    [without_credentials(Item, Opts) || Item <- List];
without_credentials(Value, _Opts) when is_binary(Value) ->
    case uri_string:parse(Value) of
        #{ userinfo := _ } = URI -> uri_string:recompose(maps:remove(userinfo, URI));
        _ -> Value
    end;
without_credentials(Value, _Opts) ->
    Value.

%%% Tests

ratcheted_apply_test() ->
    Opts = #{ <<"store">> => [hb_test_utils:test_store()] },
    Apply =
        fun(Control, Fun) ->
            ratcheted_apply(
                <<"test/control">>, <<"signer">>, Control, Fun, Opts
            )
        end,
    Ok = fun() -> {ok, applied} end,
    ?assertEqual({ok, applied}, Apply(#{}, Ok)),
    ?assertEqual(
        {error, failed},
        Apply(#{ <<"created">> => 1 }, fun() -> {error, failed} end)
    ),
    ?assertEqual({ok, applied}, Apply(#{ <<"created">> => 1 }, Ok)),
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        Apply(#{ <<"created">> => 1 }, Ok)
    ),
    ?assertMatch(
        {error, #{ <<"status">> := 403 }},
        Apply(#{ <<"created">> => 0 }, Ok)
    ),
    ?assertEqual({ok, applied}, Apply(#{ <<"created">> => 2 }, Ok)).
