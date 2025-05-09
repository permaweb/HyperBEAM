%%% @doc A module that implements a 'friends and family' pricing policy.
%%% It will allow users to process requests only if their addresses are
%%% in the allow-list for the node.
%%% 
%%% Fundamentally against the spirit of permissionlessness, but it is useful if
%%% you are running a node for your own purposes and would not like to allow 
%%% others to make use of it -- even for a fee. It also serves as a useful
%%% example of how to implement a custom pricing policy, as it implements stubs
%%% for both the pricing and ledger P4 APIs.
-module(dev_faff).
%%% Pricing API
%%% We only implement `estimate/3' as we do not want to charge for requests, so
%%% we are fine with the estimate being the same as the price.
-export([estimate/3]).
%%% Ledger API
%%% We need to implement `debit/3' as it is required by the ledger API, but we
%%% do not want to charge for requests, so we return `ok' and do not actually
%%% debit the user's account. Similarly, we are not interested in taking payments
%%% from users, so we do not implement `credit/3'.
-export([debit/3]).
-include("include/hb.hrl").

%% @doc Decide whether or not to service a request from a given address.
estimate(_, Msg, NodeMsg) ->
    ?event(payment, {estimate, {msg, Msg}}),
    % Check if the address is in the allow-list.
    case is_admissible(Msg, NodeMsg) of
        true -> {ok, 0};
        false -> {ok, <<"infinity">>}
    end.

%% @doc Check whether all of the signers of the request are in the allow-list.
is_admissible(Msg, NodeMsg) ->
    AllowList = hb_opts:get(faff_allow_list, [], NodeMsg),
    Req = hb_ao:get(<<"request">>, Msg, NodeMsg),
    Signers = hb_message:signers(Req),
    ?event(payment, {is_admissible, {signers, Signers}, {allow_list, AllowList}}),
    lists:all(
        fun(Signer) -> lists:member(Signer, AllowList) end,
        Signers
    ).

%% @doc Debit the user's account if the request is allowed.
debit(_, Req, _NodeMsg) ->
    ?event(payment, {debit, Req}),
    {ok, true}.
