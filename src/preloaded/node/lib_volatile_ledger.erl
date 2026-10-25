%%% @doc A ledger of account balances that one process per node keeps in
%%% memory. The process applies the debits and reads that it receives one at a
%%% time, so concurrent debits of an account never overwrite each other. The
%%% balances last as long as the process: when the VM restarts, a ledger starts
%%% again from its starting balances.
%%%
%%% A ledger is described by a message, which its process reads when it starts:
%%%
%%% ```
%%%     name:     The name of the ledger. A node has one process per name.
%%%     balances: A message of the starting balance of each account it lists.
%%%               An account whose balance is `infinity' is never debited.
%%%               Default: #{}.
%%%     default:  The starting balance of every other account.
%%%               Default: 0.
%%%     recharge: The amount that a balance recharges per millisecond after
%%%               a debit, up to `max'.
%%%               Default: 0.
%%%     max:      The highest balance that recharging reaches.
%%%               Default: infinity.
%%%     min:      The lowest balance that a debit leaves.
%%%               Default: none.
%%% ```
%%%
%%% An account is a key of `balances', so the ledger holds and reads each
%%% account lowercase, as keys are in `structured@1.0' form.
-module(lib_volatile_ledger).
-export([debit/4, debit/5, balance/3]).
-include("include/hb.hrl").

%% @doc Debit `Amount' from `Account' and return its new balance. A negative
%% amount credits the account.
debit(Ledger, Account, Amount, Opts) when is_number(Amount) ->
    call(Ledger, {debit, hb_util:lowercase_key(Account), Amount}, Opts).

%% @doc Debit `Amount' from `Account' once for each `Nonce', and return its new
%% balance. A nonce that the ledger has seen gives `{error, already_applied}'.
debit(Ledger, Account, Amount, Nonce, Opts) when is_number(Amount) ->
    call(Ledger, {debit, hb_util:lowercase_key(Account), Amount, Nonce}, Opts).

%% @doc Return the balance of `Account'.
balance(Ledger, Account, Opts) ->
    call(Ledger, {balance, hb_util:lowercase_key(Account)}, Opts).

%% @doc Send a request to the ledger's process and wait for its reply, for as
%% long as the process is alive.
call(Ledger, Request, Opts) ->
    PID = ensure_started(Ledger, Opts),
    Ref = erlang:monitor(process, PID),
    PID ! {Request, self(), Ref},
    receive
        {Ref, Balance} ->
            erlang:demonitor(Ref, [flush]),
            Balance;
        {'DOWN', Ref, process, PID, Reason} ->
            error({'ledger-down', Reason})
    end.

%% @doc Return the PID of the ledger's process on this node, starting it if it
%% is not running. The process is registered under the ledger's `name' and
%% the node's wallet address.
ensure_started(Ledger, Opts) ->
    ServerID =
        {
            hb_maps:get(<<"name">>, Ledger, undefined, Opts),
            hb_opts:address(Opts)
        },
    hb_name:singleton(
        ServerID,
        fun() -> start_server(ServerID, Ledger, Opts) end
    ).

%% @doc Read the ledger's message into the state of its process, and run it.
start_server(ServerID, Ledger, Opts) ->
    ?event(volatile_ledger, {started_ledger, {server_id, ServerID}}),
    server_loop(
        #{
            balances =>
                maps:fold(
                    fun(Account, Balance, Balances) ->
                        Balances#{ hb_util:lowercase_key(Account) => Balance }
                    end,
                    #{},
                    hb_maps:get(<<"balances">>, Ledger, #{}, Opts)
                ),
            default => hb_maps:get(<<"default">>, Ledger, 0, Opts),
            recharge => hb_maps:get(<<"recharge">>, Ledger, 0, Opts),
            max => hb_maps:get(<<"max">>, Ledger, infinity, Opts),
            min => hb_maps:get(<<"min">>, Ledger, undefined, Opts),
            accounts => #{},
            nonces => #{},
            opts => Opts
        }
    ).

%% @doc The main loop of the ledger's process. Only responds to three messages:
%% - `{{debit, Account, Amount}, PID, Ref}': Debit the account by the amount
%%   and reply with its new balance.
%% - `{{debit, Account, Amount, Nonce}, PID, Ref}': The same, once for each
%%   nonce. A nonce that the ledger has seen gets `{error, already_applied}'.
%% - `{{balance, Account}, PID, Ref}': Reply with the account's balance.
server_loop(State = #{ nonces := Nonces }) ->
    receive
        {{debit, Account, Amount}, PID, Ref} ->
            server_loop(debit_and_reply(Account, Amount, PID, Ref, State));
        {{debit, _Account, _Amount, Nonce}, PID, Ref}
                when is_map_key(Nonce, Nonces) ->
            PID ! {Ref, {error, already_applied}},
            server_loop(State);
        {{debit, Account, Amount, Nonce}, PID, Ref} ->
            NewState = debit_and_reply(Account, Amount, PID, Ref, State),
            server_loop(NewState#{ nonces => Nonces#{ Nonce => true } });
        {{balance, Account}, PID, Ref} ->
            PID ! {Ref, account_balance(Account, State)},
            server_loop(State)
    end.

%% @doc Debit the account, reply to the caller with its new balance, and
%% return the new state.
debit_and_reply(Account, Amount, PID, Ref, State) ->
    Now = erlang:system_time(millisecond),
    NewState = debit_account(Account, Amount, State, Now),
    PID ! {Ref, account_balance(Account, NewState, Now)},
    NewState.

%% @doc Debit the account by the given amount, down to the ledger's `min'.
debit_account(
        Account,
        Amount,
        State = #{ accounts := Accounts, min := Min },
        Now
    ) ->
    case account_balance(Account, State, Now) of
        infinity -> State;
        Balance ->
            NewBalance =
                case Min of
                    undefined -> Balance - Amount;
                    _ -> max(Min, Balance - Amount)
                end,
            State#{
                accounts =>
                    Accounts#{
                        Account => #{ balance => NewBalance, last => Now }
                    }
            }
    end.

%% @doc Calculate the current balance of an account, including what it has
%% recharged since its last debit.
account_balance(Account, State) ->
    account_balance(Account, State, erlang:system_time(millisecond)).
account_balance(
        Account,
        #{
            balances := Balances,
            default := Default,
            recharge := Recharge,
            max := Max,
            accounts := Accounts,
            opts := Opts
        },
        Time
    ) ->
    case maps:get(Account, Accounts, not_found) of
        not_found -> hb_maps:get(Account, Balances, Default, Opts);
        #{ balance := Balance, last := LastDebit } ->
            RechargedSinceLast = (Time - LastDebit) * Recharge,
            min(Max, Balance + RechargedSinceLast)
    end.
