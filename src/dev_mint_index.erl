%%% @doc A mint device for constructing and managing token indexes which track
%%% and mirror the delegation choices of users in other `~pot@1.0' token mints.
%%% 
%%% `~mint-index@1.0' allows the configurer to specify a `parent' mint process to
%%% monitor, and a list of addresses to track delegations to. Additionally, the 
%%% configurer can specify a mechanism for determining whether new processes
%%% (mints) should be added to the admissible list to track.
%%% 
%%% The index itself may mint its own tokens, according to a given `mint-device'.
%%% This allows rights in the index itself to be tradable, and even mintable
%%% via the same delegation mechanics as the parent.
%%% 
%%% A practical example of this mechanism, although by no means the only viable
%%% strategy, is the Permaweb Index: A neutral pool that replicates the 
%%% delegation choices of users in the AO token mint to fair launch projects in
%%% the Arweave and AO ecosystem.
-module(dev_mint_index).
-export([info/2, mint/3, notify/3]).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/hb.hrl").

%% @doc Forward all key resolution requests aside `notify` and `mint' to be
%% resolved by the `provider-mint-device`.
info(Base, Opts) ->
    #{ default => provider_mint_device(Base, Opts) }.

%% @doc Return the device name that should be used to handle the minting of new
%% tokens in this process (ownership representations in the index itself).
provider_mint_device(Base, Opts) ->
    hb_maps:get(
        <<"provider-mint-device">>,
        Base,
        <<"pot@1.0">>,
        Opts
    ).

%% @doc Relay the `mint' call to the `partner-mint-device' after ensuring that
%% the index itself has been initialized.
mint(RawBase, Assignment, Opts) ->
    Base = ensure_initialized(RawBase, Opts),
    % TODO: Should this be an `as'? `as' is the logical choice but causes an
    % infinite loop.
    hb_ao:resolve(
        Base#{ <<"device">> => provider_mint_device(Base, Opts) },
        Assignment,
        Opts
    ).

%% @doc Initialize the index process based on its base state, if not already
%% complete.
ensure_initialized(Base, Opts) ->
    case hb_maps:get(<<"index-ininitialized">>, Base, false, Opts) of
        true -> 
            ?event(already_initialized),
            Base;
        false ->
            After = hb_ao:set(
                send_registrations(Base, Opts),
                <<"index-initialized">>,
                true,
                Opts
            ),
            ?event(after_set),
            After
    end.

%% @doc Register with the `parent' token to receive notifications about
%% delegations to all of the `active-mints'.
send_registrations(Base, Opts) ->
    AllActiveMints = hb_maps:get(<<"indexed-mints">>, Base, [], Opts),
    KnownDeposits =  hb_ao:get(<<"indexed-deposits/keys">>, Base, [], Opts),
    RegistrationNeeded = hb_util:list_without(KnownDeposits, AllActiveMints),
    send_registrations(Base, RegistrationNeeded, Opts).
send_registrations(Base, ToRegister, Opts) ->
    Parent = hb_ao:get(<<"parent">>, Base, Opts),
    lists:foldr(
        fun(Addr, PreRegBase) ->
            ?event(
                index_short,
                {registering,
                    {address, Addr},
                    {parent, Parent}
                }
            ),
            WithDepositReg = 
                dev_process_outbox:send_subscription_request(
                    Parent,
                    <<"deposit">>,
                    Addr,
                    PreRegBase,
                    Opts
                ),
            dev_process_outbox:send_subscription_request(
                Parent,
                <<"withdraw">>,
                Addr,
                WithDepositReg,
                Opts
            )
        end,
        Base,
        ToRegister
    ).

%% @doc Interpret delegation notifications from the parent mint based on their
%% original `target' address:
%% 1. If the `x-target' of the notification matches the local process's ID, we
%%   forward the notification to the `notify' key on the `default-device'.
%% 2. Otherwise, we interpret it as a signal that we should use in order to modify
%%   our own delegation choices.
notify(RawBase, Assignment, Opts) ->
    ?event(debug_index, {received_notify, Assignment}, Opts),
    Base = ensure_initialized(RawBase, Opts),
    ProcessID = dev_process_lib:process_id(Base, Opts),
    case hb_ao:get(<<"body/x-target">>, Assignment, Opts) of
        ProcessID ->
            % We are the target, so resolve with the provider mint device.
            hb_ao:resolve(
                {as, provider_mint_device(Base, Opts), Base},
                Assignment,
                Opts
            );
        _Other ->
            % We are being notified of a signal that may lead us to change our
            % delegation choices. Extract the relevant information from the 
            % notification, change our model to reflect the new information,
            % then send our updated preferences to the `parent' mint if 
            % appropriate.
            maybe
                {ok, Address, Quantity} ?=
                    parse_signal(
                        Base,
                        Assignment,
                        Opts
                    ),
                {ok, BaseAfter} ?=
                    update_model(
                        Address,
                        Quantity,
                        Base,
                        Opts
                    ),
                case must_redelegate(BaseAfter, Address, Quantity, Opts) of
                    false -> {ok, BaseAfter};
                    true -> redelegate(BaseAfter, Assignment, Opts)
                end
            end
    end.

%% @doc Validate the notification message and extract the key parameters.
parse_signal(Base, Assignment, Opts) ->
    maybe
        {ok, Parent} ?=
            hb_maps:find(
                <<"parent">>,
                Base,
                <<"No `parent' initialized on this index.">>,
                Opts
            ),
        {ok, Msg} ?=
            hb_maps:find(
                <<"body">>,
                Assignment,
                <<
                    "`~mint-index@1.0/notify' must be called with a ",
                    "valid assignment."
                >>,
                Opts
            ),
        {ok, From} ?=
            hb_maps:find(
                <<"from">>,
                Msg,
                <<
                    "`~mint-index@1.0/notify' must be called with a ",
                    "valid assignment."
                >>,
                Opts
            ),
        {ok, Address} ?=
            hb_maps:find(
                <<"x-target">>,
                Msg,
                <<"No delegation target provided.">>,
                Opts
            ),
        {ok, RawQuantity} ?=
            hb_maps:find(
                <<"x-quantity">>,
                Msg,
                <<"No `quantity' provided.">>,
                Opts
            ),
        {ok, Quantity} ?=
            case hb_maps:find(<<"x-action">>, Msg, Opts) of
                {ok, <<"deposit">>} -> {ok, RawQuantity};
                {ok, <<"withdraw">>} -> {ok, -RawQuantity};
                {ok, _Unsupported} ->
                    {error, <<"Notification `action' unsupported.">>};
                error ->
                    {error, <<"No `action' provided.">>}
            end,
        true ?= (Parent == From) orelse
            {error, <<"Invalid notification source.">>},
        {ok, Address, Quantity}
    end.

%% @doc Modify our internal representation of delegations from other users, then
%% potentially send re-delegation requests to the parent process to reflect said
%% changes.
update_model(Address, Quantity, Base, Opts) ->
    ?event(
        index_short,
        {signal,
            {quantity, Quantity},
            {address, Address}
        },
        Opts
    ),
    OldBalance = hb_ao:get(<<"indexed-deposits">>, Base, 0, Opts),
    ChangesSinceUpdate = hb_ao:get(<<"changes-since-update">>, Base, 0, Opts),
    hb_ao:resolve(
        Base,
        #{
            <<"path">> => <<"set">>,
            <<"indexed-deposits/", Address/binary>> => Quantity + OldBalance,
            <<"changes-since-update">> => ChangesSinceUpdate + 1
        }
    ).

%% @doc Determine if the index's delegations must be updated in the parent mint.
must_redelegate(Base, _Quantity, _Address, Opts) ->
    UpdateEvery = hb_ao:get(<<"update-every">>, Base, 0, Opts),
    ChangesSinceUpdate = hb_ao:get(<<"changes-since-update">>, Base, 0, Opts),
    UpdateEvery >= ChangesSinceUpdate.

%% @doc Send messages reflecting updated delegation preferences to the parent
%% mint.
redelegate(Base, Assignment, Opts) ->
    Self = dev_process_lib:process_id(Base, Opts),
    Parent = hb_ao:get(<<"parent">>, Base, Opts),
    TriggerSlot = hb_ao:get(<<"slot">>, Assignment, Opts),
    ?event(
        index_short,
        {sending_updated_delegations,
            {self, Self},
            {parent_mint, Parent},
            {trigger_slot, TriggerSlot}
        },
        Opts
    ),
    hb_ao:resolve(
        Base,
        #{
            <<"path">> => <<"set">>,
            <<"changes-since-update">> => 0
        },
        Opts
    ).