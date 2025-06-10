# [Module dev_poda.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_poda.erl)




A simple exemplar decentralized proof of authority consensus algorithm
for AO processes.

<a name="description"></a>

## Description ##

This device is split into two flows, spanning three
actions.

Execution flow:
1. Initialization.
2. Validation of incoming messages before execution.
Commitment flow:
1. Adding commitments to results, either on a CU or MU.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_commitments-2">add_commitments/2*</a></td><td></td></tr><tr><td valign="top"><a href="#commit_to_results-2">commit_to_results/2*</a></td><td></td></tr><tr><td valign="top"><a href="#execute-3">execute/3</a></td><td></td></tr><tr><td valign="top"><a href="#extract_opts-1">extract_opts/1*</a></td><td></td></tr><tr><td valign="top"><a href="#find_process-2">find_process/2*</a></td><td>Find the process that this message is targeting, in order to
determine which commitments to add.</td></tr><tr><td valign="top"><a href="#init-2">init/2</a></td><td></td></tr><tr><td valign="top"><a href="#is_user_signed-1">is_user_signed/1</a></td><td>Determines if a user committed.</td></tr><tr><td valign="top"><a href="#pfiltermap-2">pfiltermap/2*</a></td><td>Helper function for parallel execution of commitment
gathering.</td></tr><tr><td valign="top"><a href="#push-2">push/2</a></td><td>Hook used by the MU pathway (currently) to add commitments to an
outbound message if the computation requests it.</td></tr><tr><td valign="top"><a href="#return_error-2">return_error/2*</a></td><td></td></tr><tr><td valign="top"><a href="#validate-2">validate/2*</a></td><td></td></tr><tr><td valign="top"><a href="#validate_commitment-3">validate_commitment/3*</a></td><td></td></tr><tr><td valign="top"><a href="#validate_stage-3">validate_stage/3*</a></td><td></td></tr><tr><td valign="top"><a href="#validate_stage-4">validate_stage/4*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_commitments-2"></a>

### add_commitments/2 * ###

`add_commitments(NewMsg, S) -> any()`

<a name="commit_to_results-2"></a>

### commit_to_results/2 * ###

`commit_to_results(Msg, S) -> any()`

<a name="execute-3"></a>

### execute/3 ###

`execute(Outer, S, Opts) -> any()`

<a name="extract_opts-1"></a>

### extract_opts/1 * ###

`extract_opts(Params) -> any()`

<a name="find_process-2"></a>

### find_process/2 * ###

`find_process(Item, X2) -> any()`

Find the process that this message is targeting, in order to
determine which commitments to add.

<a name="init-2"></a>

### init/2 ###

`init(S, Params) -> any()`

<a name="is_user_signed-1"></a>

### is_user_signed/1 ###

`is_user_signed(Tx) -> any()`

Determines if a user committed

<a name="pfiltermap-2"></a>

### pfiltermap/2 * ###

`pfiltermap(Pred, List) -> any()`

Helper function for parallel execution of commitment
gathering.

<a name="push-2"></a>

### push/2 ###

`push(Item, S) -> any()`

Hook used by the MU pathway (currently) to add commitments to an
outbound message if the computation requests it.

<a name="return_error-2"></a>

### return_error/2 * ###

`return_error(S, Reason) -> any()`

<a name="validate-2"></a>

### validate/2 * ###

`validate(Msg, Opts) -> any()`

<a name="validate_commitment-3"></a>

### validate_commitment/3 * ###

`validate_commitment(Msg, Comm, Opts) -> any()`

<a name="validate_stage-3"></a>

### validate_stage/3 * ###

`validate_stage(X1, Msg, Opts) -> any()`

<a name="validate_stage-4"></a>

### validate_stage/4 * ###

`validate_stage(X1, Tx, Content, Opts) -> any()`

