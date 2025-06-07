# [Module dev_push.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_push.erl)




`push@1.0` takes a message or slot number, evaluates it, and recursively
pushes the resulting messages to other processes.

<a name="description"></a>

## Description ##
The `push`ing mechanism
continues until the there are no remaining messages to push.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#apply_security-4">apply_security/4*</a></td><td>Apply the recipient's security policy to the message.</td></tr><tr><td valign="top"><a href="#apply_security-5">apply_security/5*</a></td><td></td></tr><tr><td valign="top"><a href="#augment_message-3">augment_message/3*</a></td><td>Set the necessary keys in order for the recipient to know where the
message came from.</td></tr><tr><td valign="top"><a href="#calculate_base_id-2">calculate_base_id/2*</a></td><td>Calculate the base ID for a process.</td></tr><tr><td valign="top"><a href="#commit_result-4">commit_result/4*</a></td><td>Attempt to sign a result message with the given committers.</td></tr><tr><td valign="top"><a href="#do_push-3">do_push/3*</a></td><td>Push a message or slot number, including its downstream results.</td></tr><tr><td valign="top"><a href="#extract-2">extract/2*</a></td><td>Return either the <code>target</code> or the <code>hint</code>.</td></tr><tr><td valign="top"><a href="#find_type-2">find_type/2*</a></td><td></td></tr><tr><td valign="top"><a href="#full_push_test_-0">full_push_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#is_async-3">is_async/3*</a></td><td>Determine if the push is asynchronous.</td></tr><tr><td valign="top"><a href="#message_to_legacynet_scheduler_script-0">message_to_legacynet_scheduler_script/0*</a></td><td></td></tr><tr><td valign="top"><a href="#multi_process_push_test_-0">multi_process_push_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_message-2">normalize_message/2*</a></td><td>Augment the message with from-* keys, if it doesn't already have them.</td></tr><tr><td valign="top"><a href="#parse_redirect-2">parse_redirect/2*</a></td><td></td></tr><tr><td valign="top"><a href="#ping_pong_script-1">ping_pong_script/1*</a></td><td>Test that a message that generates another message which resides on an
ANS-104 scheduler leads to <code>~push@1.0</code> re-signing the message correctly.</td></tr><tr><td valign="top"><a href="#push-3">push/3</a></td><td>Push either a message or an assigned slot number.</td></tr><tr><td valign="top"><a href="#push_as_identity_test_-0">push_as_identity_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#push_prompts_encoding_change-0">push_prompts_encoding_change/0*</a></td><td></td></tr><tr><td valign="top"><a href="#push_prompts_encoding_change_test_-0">push_prompts_encoding_change_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#push_result_message-4">push_result_message/4*</a></td><td>Push a downstream message result.</td></tr><tr><td valign="top"><a href="#push_with_mode-3">push_with_mode/3*</a></td><td></td></tr><tr><td valign="top"><a href="#push_with_redirect_hint_test_disabled-0">push_with_redirect_hint_test_disabled/0*</a></td><td></td></tr><tr><td valign="top"><a href="#remote_schedule_result-3">remote_schedule_result/3*</a></td><td></td></tr><tr><td valign="top"><a href="#reply_script-0">reply_script/0*</a></td><td></td></tr><tr><td valign="top"><a href="#schedule_initial_message-3">schedule_initial_message/3*</a></td><td>Push a message or a process, prior to pushing the resulting slot number.</td></tr><tr><td valign="top"><a href="#schedule_result-4">schedule_result/4*</a></td><td>Add the necessary keys to the message to be scheduled, then schedule it.</td></tr><tr><td valign="top"><a href="#schedule_result-5">schedule_result/5*</a></td><td></td></tr><tr><td valign="top"><a href="#split_target-1">split_target/1*</a></td><td>Split the target into the process ID and the optional query string.</td></tr><tr><td valign="top"><a href="#target_process-2">target_process/2*</a></td><td>Find the target process ID for a message to push.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="apply_security-4"></a>

### apply_security/4 * ###

`apply_security(Msg, TargetProcess, Codec, Opts) -> any()`

Apply the recipient's security policy to the message. Observes the
following parameters in order to calculate the appropriate security policy:
- `policy`: A message that generates a security policy message.
- `authority`: A single committer, or list of comma separated committers.
- (Default: Signs with default wallet)

<a name="apply_security-5"></a>

### apply_security/5 * ###

`apply_security(X1, Msg, TargetProcess, Codec, Opts) -> any()`

<a name="augment_message-3"></a>

### augment_message/3 * ###

`augment_message(Origin, ToSched, Opts) -> any()`

Set the necessary keys in order for the recipient to know where the
message came from.

<a name="calculate_base_id-2"></a>

### calculate_base_id/2 * ###

`calculate_base_id(GivenProcess, Opts) -> any()`

Calculate the base ID for a process. The base ID is not just the
uncommitted process ID. It also excludes the `authority` and `scheduler`
keys.

<a name="commit_result-4"></a>

### commit_result/4 * ###

`commit_result(Msg, Committers, Codec, Opts) -> any()`

Attempt to sign a result message with the given committers.

<a name="do_push-3"></a>

### do_push/3 * ###

`do_push(PrimaryProcess, Assignment, Opts) -> any()`

Push a message or slot number, including its downstream results.

<a name="extract-2"></a>

### extract/2 * ###

`extract(X1, Raw) -> any()`

Return either the `target` or the `hint`.

<a name="find_type-2"></a>

### find_type/2 * ###

`find_type(Req, Opts) -> any()`

<a name="full_push_test_-0"></a>

### full_push_test_/0 * ###

`full_push_test_() -> any()`

<a name="is_async-3"></a>

### is_async/3 * ###

`is_async(Process, Req, Opts) -> any()`

Determine if the push is asynchronous.

<a name="message_to_legacynet_scheduler_script-0"></a>

### message_to_legacynet_scheduler_script/0 * ###

`message_to_legacynet_scheduler_script() -> any()`

<a name="multi_process_push_test_-0"></a>

### multi_process_push_test_/0 * ###

`multi_process_push_test_() -> any()`

<a name="normalize_message-2"></a>

### normalize_message/2 * ###

`normalize_message(MsgToPush, Opts) -> any()`

Augment the message with from-* keys, if it doesn't already have them.

<a name="parse_redirect-2"></a>

### parse_redirect/2 * ###

`parse_redirect(Location, Opts) -> any()`

<a name="ping_pong_script-1"></a>

### ping_pong_script/1 * ###

`ping_pong_script(Limit) -> any()`

Test that a message that generates another message which resides on an
ANS-104 scheduler leads to `~push@1.0` re-signing the message correctly.
Requires `ENABLE_GENESIS_WASM` to be enabled.

<a name="push-3"></a>

### push/3 ###

`push(Base, Req, Opts) -> any()`

Push either a message or an assigned slot number. If a `Process` is
provided in the `body` of the request, it will be scheduled (initializing
it if it does not exist). Otherwise, the message specified by the given
`slot` key will be pushed.

Optional parameters:

`/result-depth`: The depth to which the full contents of the result
will be included in the response. Default: 1, returning
the full result of the first message, but only the 'tree'
of downstream messages.

`/push-mode`: Whether or not the push should be done asynchronously.
Default: `sync`, pushing synchronously.

<a name="push_as_identity_test_-0"></a>

### push_as_identity_test_/0 * ###

`push_as_identity_test_() -> any()`

<a name="push_prompts_encoding_change-0"></a>

### push_prompts_encoding_change/0 * ###

`push_prompts_encoding_change() -> any()`

<a name="push_prompts_encoding_change_test_-0"></a>

### push_prompts_encoding_change_test_/0 * ###

`push_prompts_encoding_change_test_() -> any()`

<a name="push_result_message-4"></a>

### push_result_message/4 * ###

`push_result_message(TargetProcess, MsgToPush, Origin, Opts) -> any()`

Push a downstream message result. The `Origin` map contains information
about the origin of the message: The process that originated the message,
the slot number from which it was sent, and the outbox key of the message,
and the depth to which downstream results should be included in the message.

<a name="push_with_mode-3"></a>

### push_with_mode/3 * ###

`push_with_mode(Process, Req, Opts) -> any()`

<a name="push_with_redirect_hint_test_disabled-0"></a>

### push_with_redirect_hint_test_disabled/0 * ###

`push_with_redirect_hint_test_disabled() -> any()`

<a name="remote_schedule_result-3"></a>

### remote_schedule_result/3 * ###

`remote_schedule_result(Location, SignedReq, Opts) -> any()`

<a name="reply_script-0"></a>

### reply_script/0 * ###

`reply_script() -> any()`

<a name="schedule_initial_message-3"></a>

### schedule_initial_message/3 * ###

`schedule_initial_message(Base, Req, Opts) -> any()`

Push a message or a process, prior to pushing the resulting slot number.

<a name="schedule_result-4"></a>

### schedule_result/4 * ###

`schedule_result(TargetProcess, MsgToPush, Origin, Opts) -> any()`

Add the necessary keys to the message to be scheduled, then schedule it.
If the remote scheduler does not support the given codec, it will be
downgraded and re-signed.

<a name="schedule_result-5"></a>

### schedule_result/5 * ###

`schedule_result(TargetProcess, MsgToPush, Codec, Origin, Opts) -> any()`

<a name="split_target-1"></a>

### split_target/1 * ###

`split_target(RawTarget) -> any()`

Split the target into the process ID and the optional query string.

<a name="target_process-2"></a>

### target_process/2 * ###

`target_process(MsgToPush, Opts) -> any()`

Find the target process ID for a message to push.

