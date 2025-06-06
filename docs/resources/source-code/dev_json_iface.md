# [Module dev_json_iface.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/dev_json_iface.erl)




A device that provides a way for WASM execution to interact with
the HyperBEAM (and AO) systems, using JSON as a shared data representation.

<a name="description"></a>

## Description ##

The interface is easy to use. It works as follows:

1. The device is given a message that contains a process definition, WASM
environment, and a message that contains the data to be processed,
including the image to be used in part of `execute{pass=1}`.
2. The device is called with `execute{pass=2}`, which reads the result of
the process execution from the WASM environment and adds it to the
message.

The device has the following requirements and interface:

```

       M1/Computed when /Pass == 1 ->
           Assumes:
               M1/priv/wasm/instance
               M1/Process
               M2/Message
               M2/Assignment/Block-Height
           Generates:
               /wasm/handler
               /wasm/params
           Side-effects:
               Writes the process and message as JSON representations into the
               WASM environment.
       M1/Computed when M2/Pass == 2 ->
           Assumes:
               M1/priv/wasm/instance
               M2/Results
               M2/Process
           Generates:
               /Results/Outbox
               /Results/Data
```
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#aos_stack_benchmark_test_-0">aos_stack_benchmark_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#basic_aos_call_test_-0">basic_aos_call_test_/0*</a></td><td></td></tr><tr><td valign="top"><a href="#compute-3">compute/3</a></td><td>On first pass prepare the call, on second pass get the results.</td></tr><tr><td valign="top"><a href="#denormalize_message-1">denormalize_message/1*</a></td><td>Normalize a message for AOS-compatibility.</td></tr><tr><td valign="top"><a href="#env_read-3">env_read/3*</a></td><td>Read the results out of the execution environment.</td></tr><tr><td valign="top"><a href="#env_write-5">env_write/5*</a></td><td>Write the message and process into the execution environment.</td></tr><tr><td valign="top"><a href="#generate_aos_msg-2">generate_aos_msg/2</a></td><td></td></tr><tr><td valign="top"><a href="#generate_stack-1">generate_stack/1</a></td><td></td></tr><tr><td valign="top"><a href="#generate_stack-2">generate_stack/2</a></td><td></td></tr><tr><td valign="top"><a href="#header_case_string-1">header_case_string/1*</a></td><td></td></tr><tr><td valign="top"><a href="#init-3">init/3</a></td><td>Initialize the device.</td></tr><tr><td valign="top"><a href="#json_to_message-2">json_to_message/2</a></td><td>Translates a compute result -- either from a WASM execution using the
JSON-Iface, or from a <code>Legacy</code> CU -- and transforms it into a result message.</td></tr><tr><td valign="top"><a href="#maybe_list_to_binary-1">maybe_list_to_binary/1*</a></td><td></td></tr><tr><td valign="top"><a href="#message_to_json_struct-1">message_to_json_struct/1</a></td><td></td></tr><tr><td valign="top"><a href="#message_to_json_struct-2">message_to_json_struct/2*</a></td><td></td></tr><tr><td valign="top"><a href="#normalize_results-1">normalize_results/1*</a></td><td>Normalize the results of an evaluation.</td></tr><tr><td valign="top"><a href="#postprocess_outbox-3">postprocess_outbox/3*</a></td><td>Post-process messages in the outbox to add the correct <code>from-process</code>
and <code>from-image</code> tags.</td></tr><tr><td valign="top"><a href="#prep_call-3">prep_call/3*</a></td><td>Prepare the WASM environment for execution by writing the process string and
the message as JSON representations into the WASM environment.</td></tr><tr><td valign="top"><a href="#prepare_header_case_tags-1">prepare_header_case_tags/1*</a></td><td>Convert a message without an <code>original-tags</code> field into a list of
key-value pairs, with the keys in HTTP header-case.</td></tr><tr><td valign="top"><a href="#prepare_tags-1">prepare_tags/1*</a></td><td>Prepare the tags of a message as a key-value list, for use in the
construction of the JSON-Struct message.</td></tr><tr><td valign="top"><a href="#preprocess_results-2">preprocess_results/2*</a></td><td>After the process returns messages from an evaluation, the
signing node needs to add some tags to each message and spawn such that
the target process knows these messages are created by a process.</td></tr><tr><td valign="top"><a href="#results-3">results/3*</a></td><td>Read the computed results out of the WASM environment, assuming that
the environment has been set up by <code>prep_call/3</code> and that the WASM executor
has been called with <code>computed{pass=1}</code>.</td></tr><tr><td valign="top"><a href="#safe_to_id-1">safe_to_id/1*</a></td><td></td></tr><tr><td valign="top"><a href="#tags_to_map-1">tags_to_map/1*</a></td><td>Convert a message with tags into a map of their key-value pairs.</td></tr><tr><td valign="top"><a href="#test_init-0">test_init/0*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="aos_stack_benchmark_test_-0"></a>

### aos_stack_benchmark_test_/0 * ###

`aos_stack_benchmark_test_() -> any()`

<a name="basic_aos_call_test_-0"></a>

### basic_aos_call_test_/0 * ###

`basic_aos_call_test_() -> any()`

<a name="compute-3"></a>

### compute/3 ###

`compute(M1, M2, Opts) -> any()`

On first pass prepare the call, on second pass get the results.

<a name="denormalize_message-1"></a>

### denormalize_message/1 * ###

`denormalize_message(Message) -> any()`

Normalize a message for AOS-compatibility.

<a name="env_read-3"></a>

### env_read/3 * ###

`env_read(M1, M2, Opts) -> any()`

Read the results out of the execution environment.

<a name="env_write-5"></a>

### env_write/5 * ###

`env_write(ProcessStr, MsgStr, Base, Req, Opts) -> any()`

Write the message and process into the execution environment.

<a name="generate_aos_msg-2"></a>

### generate_aos_msg/2 ###

`generate_aos_msg(ProcID, Code) -> any()`

<a name="generate_stack-1"></a>

### generate_stack/1 ###

`generate_stack(File) -> any()`

<a name="generate_stack-2"></a>

### generate_stack/2 ###

`generate_stack(File, Mode) -> any()`

<a name="header_case_string-1"></a>

### header_case_string/1 * ###

`header_case_string(Key) -> any()`

<a name="init-3"></a>

### init/3 ###

`init(M1, M2, Opts) -> any()`

Initialize the device.

<a name="json_to_message-2"></a>

### json_to_message/2 ###

`json_to_message(JSON, Opts) -> any()`

Translates a compute result -- either from a WASM execution using the
JSON-Iface, or from a `Legacy` CU -- and transforms it into a result message.

<a name="maybe_list_to_binary-1"></a>

### maybe_list_to_binary/1 * ###

`maybe_list_to_binary(List) -> any()`

<a name="message_to_json_struct-1"></a>

### message_to_json_struct/1 ###

`message_to_json_struct(RawMsg) -> any()`

<a name="message_to_json_struct-2"></a>

### message_to_json_struct/2 * ###

`message_to_json_struct(RawMsg, Features) -> any()`

<a name="normalize_results-1"></a>

### normalize_results/1 * ###

`normalize_results(Msg) -> any()`

Normalize the results of an evaluation.

<a name="postprocess_outbox-3"></a>

### postprocess_outbox/3 * ###

`postprocess_outbox(Msg, Proc, Opts) -> any()`

Post-process messages in the outbox to add the correct `from-process`
and `from-image` tags.

<a name="prep_call-3"></a>

### prep_call/3 * ###

`prep_call(M1, M2, Opts) -> any()`

Prepare the WASM environment for execution by writing the process string and
the message as JSON representations into the WASM environment.

<a name="prepare_header_case_tags-1"></a>

### prepare_header_case_tags/1 * ###

`prepare_header_case_tags(TABM) -> any()`

Convert a message without an `original-tags` field into a list of
key-value pairs, with the keys in HTTP header-case.

<a name="prepare_tags-1"></a>

### prepare_tags/1 * ###

`prepare_tags(Msg) -> any()`

Prepare the tags of a message as a key-value list, for use in the
construction of the JSON-Struct message.

<a name="preprocess_results-2"></a>

### preprocess_results/2 * ###

`preprocess_results(Msg, Opts) -> any()`

After the process returns messages from an evaluation, the
signing node needs to add some tags to each message and spawn such that
the target process knows these messages are created by a process.

<a name="results-3"></a>

### results/3 * ###

`results(M1, M2, Opts) -> any()`

Read the computed results out of the WASM environment, assuming that
the environment has been set up by `prep_call/3` and that the WASM executor
has been called with `computed{pass=1}`.

<a name="safe_to_id-1"></a>

### safe_to_id/1 * ###

`safe_to_id(ID) -> any()`

<a name="tags_to_map-1"></a>

### tags_to_map/1 * ###

`tags_to_map(Msg) -> any()`

Convert a message with tags into a map of their key-value pairs.

<a name="test_init-0"></a>

### test_init/0 * ###

`test_init() -> any()`

