%%% @doc A mint device for constructing and executing index processes which track
%%% and mirror the delegation choices of other users in other `~pot@1.0' token
%%% mints.
%%% 
%%% It allows the configurer to specify a `parent' mint process to monitor, and
%%% a list of addresses to track delegations to. Additionally, the configurer can
%%% specify a mechanism for determining whether new processes (mints) should be
%%% added to the admissible list to track.
%%% 
%%% The index itself may mint its own tokens, according to a given `mint-device'.
%%% This allows rights in the index itself to be tradable, and even mintable
%%% via the same delegation mechanics as the parent.
%%% 
%%% A practical example of this mechanism, although by no means the only viable
%%% strategy, is the Permaweb Index: A neutral pool that replicates the 
%%% delegation choices of users in the AO token mint to fair launch projects in
%%% the ecosystem.
-module(dev_mint_index).
-export([delegate/3, undelegate/3]).
-include_lib("include/hb.hrl").

%% @doc Interpret delegation notifications from the parent mint as instructions
%% to replicate the new delegation choice to the index itself.
delegate(Base, Req, Opts) ->
    maybe
        {ok, Parent} ?=
            hb_maps:find(
                <<"parent">>,
                Base,
                <<"No parent mint token provided.">>,
                Opts
            ),
        {ok, From} ?=
            hb_maps:find(
                <<"from">>,
                Req,
                <<"No security-device enforced `from' address in request.">>,
                Opts
            ),
        true ?= (Parent == From)
            orelse {error, <<"Delegation notification invalid.">>},
        {ok, Base}
    end,
    {ok, Base}.

%% @doc Inverts the `delegate' action, such that the index itself lowers its
%% delegation to the given address.
undelegate(Base, Req, Opts) ->
    {ok, Base}.