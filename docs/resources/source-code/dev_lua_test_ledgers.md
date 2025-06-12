# [Module dev_lua_test_ledgers.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_lua_test_ledgers.erl)




<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#apply_names-3">apply_names/3*</a></td><td>Apply a map of environment names to elements in either a map or list.</td></tr><tr><td valign="top"><a href="#balance-3">balance/3*</a></td><td>Retreive a single balance from the ledger.</td></tr><tr><td valign="top"><a href="#balance_total-3">balance_total/3*</a></td><td>Get the total balance for an ID across all ledgers in a set.</td></tr><tr><td valign="top"><a href="#balances-2">balances/2*</a></td><td>Get the balances of a ledger.</td></tr><tr><td valign="top"><a href="#balances-3">balances/3*</a></td><td></td></tr><tr><td valign="top"><a href="#comma_separated_scheduler_list_test-0">comma_separated_scheduler_list_test/0*</a></td><td>Ensure that the <code>hyper-token.lua</code> script can parse comma-separated
IDs in the <code>scheduler</code> field of a message.</td></tr><tr><td valign="top"><a href="#do_apply_names-3">do_apply_names/3*</a></td><td></td></tr><tr><td valign="top"><a href="#ledger-2">ledger/2*</a></td><td>Generate a Lua process definition message.</td></tr><tr><td valign="top"><a href="#ledger-3">ledger/3*</a></td><td></td></tr><tr><td valign="top"><a href="#ledgers-2">ledgers/2*</a></td><td>Get the local expectation of a ledger's balances with peer ledgers.</td></tr><tr><td valign="top"><a href="#lua_script-1">lua_script/1*</a></td><td>Generate a Lua <code>script</code> key from a file or list of files.</td></tr><tr><td valign="top"><a href="#map-2">map/2*</a></td><td>Generate a complete overview of the test environment's balances and
ledgers.</td></tr><tr><td valign="top"><a href="#map-3">map/3*</a></td><td></td></tr><tr><td valign="top"><a href="#multischeduler-0">multischeduler/0*</a></td><td></td></tr><tr><td valign="top"><a href="#multischeduler_test_disabled-0">multischeduler_test_disabled/0*</a></td><td>Verify that sub-ledgers can request and enforce multiple scheduler
commitments.</td></tr><tr><td valign="top"><a href="#normalize_env-1">normalize_env/1*</a></td><td>Normalize a set of processes, representing ledgers in a test environment,
to a canonical form: A map of <code>ID => Proc</code>.</td></tr><tr><td valign="top"><a href="#normalize_without_root-2">normalize_without_root/2*</a></td><td>Return the normalized environment without the root ledger.</td></tr><tr><td valign="top"><a href="#register-3">register/3*</a></td><td>Request that a peer register with a without sub-ledger.</td></tr><tr><td valign="top"><a href="#subledger-2">subledger/2*</a></td><td>Generate a test sub-ledger process definition message.</td></tr><tr><td valign="top"><a href="#subledger-3">subledger/3*</a></td><td></td></tr><tr><td valign="top"><a href="#subledger_deposit-0">subledger_deposit/0*</a></td><td></td></tr><tr><td valign="top"><a href="#subledger_deposit_test_-0">subledger_deposit_test_/0*</a></td><td>Verify that a user can deposit tokens into a sub-ledger.</td></tr><tr><td valign="top"><a href="#subledger_registration_test_disabled-0">subledger_registration_test_disabled/0*</a></td><td>Verify that peer ledgers on the same token are able to register mutually
to establish a peer-to-peer connection.</td></tr><tr><td valign="top"><a href="#subledger_supply-3">subledger_supply/3*</a></td><td>Calculate the supply of tokens in all sub-ledgers, from the balances of
the root ledger.</td></tr><tr><td valign="top"><a href="#subledger_to_subledger-0">subledger_to_subledger/0*</a></td><td></td></tr><tr><td valign="top"><a href="#subledger_to_subledger_test_-0">subledger_to_subledger_test_/0*</a></td><td>Verify that registered sub-ledgers are able to send tokens to each other
without the need for messages on the root ledger.</td></tr><tr><td valign="top"><a href="#subledger_transfer-0">subledger_transfer/0*</a></td><td></td></tr><tr><td valign="top"><a href="#subledger_transfer_test_-0">subledger_transfer_test_/0*</a></td><td>Simulate inter-ledger payments between users on a single sub-ledger:
1.</td></tr><tr><td valign="top"><a href="#supply-2">supply/2*</a></td><td>Get the supply of a ledger, either <code>now</code> or <code>initial</code>.</td></tr><tr><td valign="top"><a href="#supply-3">supply/3*</a></td><td></td></tr><tr><td valign="top"><a href="#test_opts-0">test_opts/0*</a></td><td>Create a node message for the test that avoids looking up unknown
recipients via remote stores.</td></tr><tr><td valign="top"><a href="#transfer-0">transfer/0*</a></td><td></td></tr><tr><td valign="top"><a href="#transfer-5">transfer/5*</a></td><td>Generate a test transfer message.</td></tr><tr><td valign="top"><a href="#transfer-6">transfer/6*</a></td><td></td></tr><tr><td valign="top"><a href="#transfer_test_-0">transfer_test_/0*</a></td><td>Test the <code>transfer</code> function.</td></tr><tr><td valign="top"><a href="#transfer_unauthorized-0">transfer_unauthorized/0*</a></td><td></td></tr><tr><td valign="top"><a href="#transfer_unauthorized_test_-0">transfer_unauthorized_test_/0*</a></td><td>User's must not be able to send tokens they do not own.</td></tr><tr><td valign="top"><a href="#unregistered_peer_transfer-0">unregistered_peer_transfer/0*</a></td><td></td></tr><tr><td valign="top"><a href="#unregistered_peer_transfer_test_-0">unregistered_peer_transfer_test_/0*</a></td><td>Verify that a ledger can send tokens to a peer ledger that is not
registered with it yet.</td></tr><tr><td valign="top"><a href="#user_supply-3">user_supply/3*</a></td><td>Calculate the supply of tokens held by users on a ledger, excluding
those held in sub-ledgers.</td></tr><tr><td valign="top"><a href="#verify_net-3">verify_net/3*</a></td><td>Execute all invariant checks for a pair of root ledger and sub-ledgers.</td></tr><tr><td valign="top"><a href="#verify_net_peer_balances-2">verify_net_peer_balances/2*</a></td><td>Verify the consistency of all expected ledger balances with their peer
ledgers and the actual balances held.</td></tr><tr><td valign="top"><a href="#verify_net_supply-3">verify_net_supply/3*</a></td><td>Verify that the sum of all spendable balances held by ledgers in a
test network is equal to the initial supply of tokens.</td></tr><tr><td valign="top"><a href="#verify_peer_balances-3">verify_peer_balances/3*</a></td><td>Verify that a ledger's expectation of its balances with peer ledgers
is consistent with the actual balances held.</td></tr><tr><td valign="top"><a href="#verify_root_supply-2">verify_root_supply/2*</a></td><td>Verify that the initial supply of tokens on the root ledger is the same
as the current supply.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="apply_names-3"></a>

### apply_names/3 * ###

`apply_names(Map, EnvNames, Opts) -> any()`

Apply a map of environment names to elements in either a map or list.
Expects a map of `ID or ProcMsg or Wallet => Name` as the `EnvNames` argument,
and a potentially deep map or list of elements to apply the names to.

<a name="balance-3"></a>

### balance/3 * ###

`balance(ProcMsg, User, Opts) -> any()`

Retreive a single balance from the ledger.

<a name="balance_total-3"></a>

### balance_total/3 * ###

`balance_total(Procs, ID, Opts) -> any()`

Get the total balance for an ID across all ledgers in a set.

<a name="balances-2"></a>

### balances/2 * ###

`balances(ProcMsg, Opts) -> any()`

Get the balances of a ledger.

<a name="balances-3"></a>

### balances/3 * ###

`balances(Mode, ProcMsg, Opts) -> any()`

<a name="comma_separated_scheduler_list_test-0"></a>

### comma_separated_scheduler_list_test/0 * ###

`comma_separated_scheduler_list_test() -> any()`

Ensure that the `hyper-token.lua` script can parse comma-separated
IDs in the `scheduler` field of a message.

<a name="do_apply_names-3"></a>

### do_apply_names/3 * ###

`do_apply_names(Map, EnvNames, Opts) -> any()`

<a name="ledger-2"></a>

### ledger/2 * ###

`ledger(Script, Opts) -> any()`

Generate a Lua process definition message.

<a name="ledger-3"></a>

### ledger/3 * ###

`ledger(Script, Extra, Opts) -> any()`

<a name="ledgers-2"></a>

### ledgers/2 * ###

`ledgers(ProcMsg, Opts) -> any()`

Get the local expectation of a ledger's balances with peer ledgers.

<a name="lua_script-1"></a>

### lua_script/1 * ###

`lua_script(Files) -> any()`

Generate a Lua `script` key from a file or list of files.

<a name="map-2"></a>

### map/2 * ###

`map(Procs, Opts) -> any()`

Generate a complete overview of the test environment's balances and
ledgers. Optionally, a map of environment names can be provided to make the
output more readable.

<a name="map-3"></a>

### map/3 * ###

`map(Procs, EnvNames, Opts) -> any()`

<a name="multischeduler-0"></a>

### multischeduler/0 * ###

`multischeduler() -> any()`

<a name="multischeduler_test_disabled-0"></a>

### multischeduler_test_disabled/0 * ###

`multischeduler_test_disabled() -> any()`

Verify that sub-ledgers can request and enforce multiple scheduler
commitments. `hyper-token` always validates that peer `base` processes
(the uncommitted process ID without its `scheduler` and `authority` fields)
match. It allows us to specify additional constraints on the `scheduler` and
`authority` fields while matching against the local ledger's base process
message. This test validates the correctness of these constraints.

The grammar supported by `hyper-token.lua` allows for the following, where
`X = scheduler | authority`:
- `X`: A list of `X`s that must (by default) be present in the
peer ledger's `X` field.
- `X-match`: A count of the number of `X`s that must be present in the
peer ledger's `X` field.
- `X-required`: A list of `X`s that always must be present in the
peer ledger's `X` field.

<a name="normalize_env-1"></a>

### normalize_env/1 * ###

`normalize_env(Procs) -> any()`

Normalize a set of processes, representing ledgers in a test environment,
to a canonical form: A map of `ID => Proc`.

<a name="normalize_without_root-2"></a>

### normalize_without_root/2 * ###

`normalize_without_root(RootProc, Procs) -> any()`

Return the normalized environment without the root ledger.

<a name="register-3"></a>

### register/3 * ###

`register(ProcMsg, Peer, Opts) -> any()`

Request that a peer register with a without sub-ledger.

<a name="subledger-2"></a>

### subledger/2 * ###

`subledger(Root, Opts) -> any()`

Generate a test sub-ledger process definition message.

<a name="subledger-3"></a>

### subledger/3 * ###

`subledger(Root, Extra, Opts) -> any()`

<a name="subledger_deposit-0"></a>

### subledger_deposit/0 * ###

`subledger_deposit() -> any()`

<a name="subledger_deposit_test_-0"></a>

### subledger_deposit_test_/0 * ###

`subledger_deposit_test_() -> any()`

Verify that a user can deposit tokens into a sub-ledger.

<a name="subledger_registration_test_disabled-0"></a>

### subledger_registration_test_disabled/0 * ###

`subledger_registration_test_disabled() -> any()`

Verify that peer ledgers on the same token are able to register mutually
to establish a peer-to-peer connection.

Disabled as explicit peer registration is not required for `hyper-token.lua`
to function.

<a name="subledger_supply-3"></a>

### subledger_supply/3 * ###

`subledger_supply(RootProc, AllProcs, Opts) -> any()`

Calculate the supply of tokens in all sub-ledgers, from the balances of
the root ledger.

<a name="subledger_to_subledger-0"></a>

### subledger_to_subledger/0 * ###

`subledger_to_subledger() -> any()`

<a name="subledger_to_subledger_test_-0"></a>

### subledger_to_subledger_test_/0 * ###

`subledger_to_subledger_test_() -> any()`

Verify that registered sub-ledgers are able to send tokens to each other
without the need for messages on the root ledger.

<a name="subledger_transfer-0"></a>

### subledger_transfer/0 * ###

`subledger_transfer() -> any()`

<a name="subledger_transfer_test_-0"></a>

### subledger_transfer_test_/0 * ###

`subledger_transfer_test_() -> any()`

Simulate inter-ledger payments between users on a single sub-ledger:
1. Alice has tokens on the root ledger.
2. Alice sends tokens to the sub-ledger from the root ledger.
3. Alice sends tokens to Bob on the sub-ledger.
4. Bob sends tokens to Alice on the root ledger.

<a name="supply-2"></a>

### supply/2 * ###

`supply(ProcMsg, Opts) -> any()`

Get the supply of a ledger, either `now` or `initial`.

<a name="supply-3"></a>

### supply/3 * ###

`supply(Mode, ProcMsg, Opts) -> any()`

<a name="test_opts-0"></a>

### test_opts/0 * ###

`test_opts() -> any()`

Create a node message for the test that avoids looking up unknown
recipients via remote stores. This improves test performance.

<a name="transfer-0"></a>

### transfer/0 * ###

`transfer() -> any()`

<a name="transfer-5"></a>

### transfer/5 * ###

`transfer(ProcMsg, Sender, Recipient, Quantity, Opts) -> any()`

Generate a test transfer message.

<a name="transfer-6"></a>

### transfer/6 * ###

`transfer(ProcMsg, Sender, Recipient, Quantity, Route, Opts) -> any()`

<a name="transfer_test_-0"></a>

### transfer_test_/0 * ###

`transfer_test_() -> any()`

Test the `transfer` function.
1. Alice has 100 tokens on a root ledger.
2. Alice sends 1 token to Bob.
3. Alice has 99 tokens, and Bob has 1 token.

<a name="transfer_unauthorized-0"></a>

### transfer_unauthorized/0 * ###

`transfer_unauthorized() -> any()`

<a name="transfer_unauthorized_test_-0"></a>

### transfer_unauthorized_test_/0 * ###

`transfer_unauthorized_test_() -> any()`

User's must not be able to send tokens they do not own. We test three
cases:
1. Transferring a token when the sender has no tokens.
2. Transferring a token when the sender has less tokens than the amount
being transferred.
3. Transferring a binary-encoded amount of tokens that exceed the quantity
of tokens the sender has available.

<a name="unregistered_peer_transfer-0"></a>

### unregistered_peer_transfer/0 * ###

`unregistered_peer_transfer() -> any()`

<a name="unregistered_peer_transfer_test_-0"></a>

### unregistered_peer_transfer_test_/0 * ###

`unregistered_peer_transfer_test_() -> any()`

Verify that a ledger can send tokens to a peer ledger that is not
registered with it yet. Each peer ledger must have precisely the same process
base message, granting transitive security properties: If a peer trusts its
own compute and assignment mechanism, then it can trust messages from exact
duplicates of itself. In order for this to be safe, the peer ledger network's
base process message must implement sufficicient rollback protections and
compute correctness guarantees.

<a name="user_supply-3"></a>

### user_supply/3 * ###

`user_supply(Proc, AllProcs, Opts) -> any()`

Calculate the supply of tokens held by users on a ledger, excluding
those held in sub-ledgers.

<a name="verify_net-3"></a>

### verify_net/3 * ###

`verify_net(RootProc, AllProcs, Opts) -> any()`

Execute all invariant checks for a pair of root ledger and sub-ledgers.

<a name="verify_net_peer_balances-2"></a>

### verify_net_peer_balances/2 * ###

`verify_net_peer_balances(AllProcs, Opts) -> any()`

Verify the consistency of all expected ledger balances with their peer
ledgers and the actual balances held.

<a name="verify_net_supply-3"></a>

### verify_net_supply/3 * ###

`verify_net_supply(RootProc, AllProcs, Opts) -> any()`

Verify that the sum of all spendable balances held by ledgers in a
test network is equal to the initial supply of tokens.

<a name="verify_peer_balances-3"></a>

### verify_peer_balances/3 * ###

`verify_peer_balances(ValidateProc, AllProcs, Opts) -> any()`

Verify that a ledger's expectation of its balances with peer ledgers
is consistent with the actual balances held.

<a name="verify_root_supply-2"></a>

### verify_root_supply/2 * ###

`verify_root_supply(RootProc, Opts) -> any()`

Verify that the initial supply of tokens on the root ledger is the same
as the current supply. This invariant will not hold for sub-ledgers, as they
'mint' tokens in their local supply when they receive them from other ledgers.

