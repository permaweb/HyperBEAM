%%% @doc Authorization for typed operations on a node. Only keys committed by
%%% the requested role are returned to the caller.
-module(lib_meta).
-export([is_authorized/4, authorized/3, role/2]).
-include("include/hb.hrl").

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
            {ok, signed_keys(Signed, Opts)}
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
