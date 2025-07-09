%%% A wrapper module for generating and executing EUnit tests for all Lua modules.
%%% When executed with `rebar3 lua-test`, this module will be invoked and scan the
%%% `scripts' directory for all Lua files, and generate an EUnit test suite for
%%% each one. By default, an individual test is generated for each function in
%%% the global `_G' table that ends in `_test'.
%%% 
%%% In order to specify other tests to run instead, the user may employ the 
%%% `LUA_TESTS' and `LUA_SCRIPTS' environment variables. The syntax for these
%%% variables is described in the function documentation for `parse_spec'.
%%% 
-module(dev_livenet_test).
-include_lib("include/hb.hrl").
-include_lib("eunit/include/eunit.hrl").


%% @doc Create a node message for the test that avoids looking up unknown 
%% recipients via remote stores. This improves test performance.
test_opts() ->
    hb:init(),
    #{}.

%% @doc Generate a Lua `script' key from a file or list of files.
lua_script(Files) when is_list(Files) ->
    [
        #{
            <<"content-type">> => <<"application/lua">>,
            <<"module">> => File,
            <<"body">> =>
                hb_util:ok(
                    file:read_file(
                        if is_binary(File) -> binary_to_list(File);
                           true -> File
                        end
                    )
                )
        }
    ||
        File <- Files
    ];
lua_script(File) when is_binary(File) ->
    hd(lua_script([File])).


%% @doc Generate a Lua process definition message.
token_process(Script, Opts) ->
    create_token_process(Script, #{}, Opts).
create_token_process(Script, Extra, Opts) ->
    % If the `balance' key is set in the `Extra' map, ensure that any wallets
    % given as keys in the message are converted to human-readable addresses.
    ModExtra =
        case maps:get(<<"balance">>, Extra, undefined) of
            undefined -> Extra;
            RawBalance ->
                Extra#{
                    <<"balance">> =>
                        maps:from_list(
                            lists:map(
                                fun({ID, Amount}) when ?IS_ID(ID) ->
                                    {hb_util:human_id(ID), Amount};
                                ({Wallet, Amount}) when is_tuple(Wallet) ->
                                    {
                                        hb_util:human_id(
                                            ar_wallet:to_address(Wallet)
                                        ),
                                        Amount
                                    }
                                end,
                                maps:to_list(RawBalance)
                            )
                        )
                }
        end,
    create_process(Script, ModExtra, Opts).

create_process(Script, Opts) ->
    create_process(Script, #{}, Opts).
create_process(Script, Extra, Opts) ->
    HostWallet = hb_opts:get(priv_wallet, hb:wallet(), Opts),
    Proc =
        hb_message:commit(
            maps:merge(
                #{
                    <<"device">> => <<"process@1.0">>,
                    <<"type">> => <<"Process">>,
                    <<"scheduler-device">> => <<"scheduler@1.0">>,
                    <<"scheduler">> => hb_util:human_id(HostWallet),
                    <<"execution-device">> => <<"lua@5.3a">>,
                    <<"authority">> => hb_util:human_id(HostWallet),
                    <<"module">> => lua_script(Script)
                },
                Extra
            ),
            Opts#{ priv_wallet => HostWallet }
        ),
    hb_cache:write(Proc, Opts),
    Proc.


%% @doc Generate a test transfer message.
transfer(ProcMsg, Sender, RecipientID, Quantity, Opts) ->
    Xfer =
        hb_message:commit(#{
            <<"path">> => <<"push">>,
            <<"body">> =>
                hb_message:commit(#{
                        <<"action">> => <<"Transfer">>,
                        <<"target">> => hb_message:id(ProcMsg, all),
                        <<"recipient">> => RecipientID,
                        <<"quantity">> => Quantity
                    },
                    Sender
                )
            },
            Sender
        ),
    hb_ao:resolve(
        ProcMsg,
        Xfer,
        Opts#{ priv_wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts) }
    ).

%% @doc Retrieve a single balance from the ledger.
balance(ProcMsg, User, Opts) when not ?IS_ID(User) ->
    balance(ProcMsg, hb_util:human_id(ar_wallet:to_address(User)), Opts);
balance(ProcMsg, ID, Opts) ->
    hb_ao:get(<<"now/balance/", ID/binary>>, ProcMsg, 0, Opts).

%% @doc Retrieve a single stake info from the livenet.
stake_info(ProcMsg, User, Opts) when not ?IS_ID(User) ->
    stake_info(ProcMsg, hb_util:human_id(ar_wallet:to_address(User)), Opts);
stake_info(ProcMsg, ID, Opts) ->
    hb_ao:get(<<"now/stakes/", ID/binary>>, ProcMsg, 0, Opts).


%% @doc unstake tokens from the livenet process.
unstake(ProcMsg, User, Opts) when not ?IS_ID(User) ->
    unstake(ProcMsg, hb_util:human_id(ar_wallet:to_address(User)), Opts);
unstake(ProcMsg, Sender, Quantity, Opts) ->
    Xfer =
        hb_message:commit(#{
            <<"path">> => <<"push">>,
            <<"body">> =>
                hb_message:commit(#{
                        <<"action">> => <<"Unstake">>,
                        <<"target">> => hb_message:id(ProcMsg, all),
                        <<"quantity">> => Quantity
                    },
                    Sender
                )
            },
            Sender
        ),
    hb_ao:resolve(
        ProcMsg,
        Xfer,
        Opts#{ priv_wallet => hb_opts:get(priv_wallet, hb:wallet(), Opts) }
    ).


%% @doc Retrieve a single unstake info from the livenet.
unstake_info(ProcMsg, User, Opts) when not ?IS_ID(User) ->
    unstake_info(ProcMsg, hb_util:human_id(ar_wallet:to_address(User)), Opts);
unstake_info(ProcMsg, ID, Opts) ->
    hb_ao:get(<<"now/unstakes/", ID/binary>>, ProcMsg, 0, Opts).




stake_unstake_test_() -> {timeout, 30, fun stake_unstake/0}.
stake_unstake() ->
    Opts = test_opts(),
    NodeUser = ar_wallet:new(),
    Alice = ar_wallet:new(),

    TokenProcess  = create_token_process(<<"scripts/hyper-token.lua">>, #{ <<"balance">> => #{ NodeUser => 100 } }, Opts),
    TokenProcessId = hb_message:id(TokenProcess, all),
    ?event(debug, {token_process_id, TokenProcessId}),
    ?assertEqual(100, balance(TokenProcess, NodeUser, Opts)),

    % TODO: add the TokenProcessId to the stake process
    StakeProcess = create_process(<<"scripts/livenet-it1.lua">>, #{ <<"token_process_id">> => TokenProcessId }, Opts),
    StakeProcessId = hb_message:id(StakeProcess, all),
    ?event(debug, {stake_process_id, StakeProcessId}),

    % transfer 10 tokens to Alice to verify transfer of tokens works
    transfer(TokenProcess, NodeUser, hb_util:human_id(Alice), 10, Opts),
    % check the balance of the alice
    ?assertEqual(10, balance(TokenProcess, Alice, Opts)),
    % check the balance of the node user
    ?assertEqual(90, balance(TokenProcess, NodeUser, Opts)),

    % transfer 10 tokens to the staking process
    transfer(TokenProcess, NodeUser, StakeProcessId, 10, Opts),
    % check the balance of the staking process
    ?assertEqual(10, balance(TokenProcess, StakeProcessId, Opts)),
    % check the balance of the node user
    ?assertEqual(80, balance(TokenProcess, NodeUser, Opts)),

    % check the stake info of the staking process
    StakeInfo = stake_info(StakeProcess, NodeUser, Opts),
    ?event(debug, {stake_info, StakeInfo}),
    % TODO: assert check the stake info

    % Initiate unstake
    unstake(StakeProcess, NodeUser, 5, Opts),

    % check the unstake info of the staking process
    UnstakeInfo = unstake_info(StakeProcess, NodeUser, Opts),
    ?event(debug, {unstake_info, UnstakeInfo}).
    % TODO: assert check the unstake info
    
    % TODO: check cooldown and then withdraw
        
    % check the balance of the staking process
    ?assertEqual(5, balance(StakeProcess, NodeUser, Opts)),
    % check the balance of the token process
    ?assertEqual(85, balance(TokenProcess, NodeUser, Opts)),
    



    