# [Module hb_name.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_name.erl)




An abstraction for name registration/deregistration in Hyperbeam.

<a name="description"></a>

## Description ##
Its motivation is to provide a way to register names that are not necessarily
atoms, but can be any term (for example: hashpaths or `process@1.0` IDs).
An important characteristic of these functions is that they are atomic:
There can only ever be one registrant for a given name at a time.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#all-0">all/0</a></td><td>List the names in the registry.</td></tr><tr><td valign="top"><a href="#all_test-0">all_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#atom_test-0">atom_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#basic_test-1">basic_test/1*</a></td><td></td></tr><tr><td valign="top"><a href="#cleanup_test-0">cleanup_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#concurrency_test-0">concurrency_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#dead_process_test-0">dead_process_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#ets_lookup-1">ets_lookup/1*</a></td><td></td></tr><tr><td valign="top"><a href="#lookup-1">lookup/1</a></td><td>Lookup a name -> PID.</td></tr><tr><td valign="top"><a href="#register-1">register/1</a></td><td>Register a name.</td></tr><tr><td valign="top"><a href="#register-2">register/2</a></td><td></td></tr><tr><td valign="top"><a href="#spawn_test_workers-1">spawn_test_workers/1*</a></td><td></td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td></td></tr><tr><td valign="top"><a href="#start_ets-0">start_ets/0*</a></td><td></td></tr><tr><td valign="top"><a href="#term_test-0">term_test/0*</a></td><td></td></tr><tr><td valign="top"><a href="#unregister-1">unregister/1</a></td><td>Unregister a name.</td></tr><tr><td valign="top"><a href="#wait_for_cleanup-2">wait_for_cleanup/2*</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="all-0"></a>

### all/0 ###

`all() -> any()`

List the names in the registry.

<a name="all_test-0"></a>

### all_test/0 * ###

`all_test() -> any()`

<a name="atom_test-0"></a>

### atom_test/0 * ###

`atom_test() -> any()`

<a name="basic_test-1"></a>

### basic_test/1 * ###

`basic_test(Term) -> any()`

<a name="cleanup_test-0"></a>

### cleanup_test/0 * ###

`cleanup_test() -> any()`

<a name="concurrency_test-0"></a>

### concurrency_test/0 * ###

`concurrency_test() -> any()`

<a name="dead_process_test-0"></a>

### dead_process_test/0 * ###

`dead_process_test() -> any()`

<a name="ets_lookup-1"></a>

### ets_lookup/1 * ###

`ets_lookup(Name) -> any()`

<a name="lookup-1"></a>

### lookup/1 ###

`lookup(Name) -> any()`

Lookup a name -> PID.

<a name="register-1"></a>

### register/1 ###

`register(Name) -> any()`

Register a name. If the name is already registered, the registration
will fail. The name can be any Erlang term.

<a name="register-2"></a>

### register/2 ###

`register(Name, Pid) -> any()`

<a name="spawn_test_workers-1"></a>

### spawn_test_workers/1 * ###

`spawn_test_workers(Name) -> any()`

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

<a name="start_ets-0"></a>

### start_ets/0 * ###

`start_ets() -> any()`

<a name="term_test-0"></a>

### term_test/0 * ###

`term_test() -> any()`

<a name="unregister-1"></a>

### unregister/1 ###

`unregister(Name) -> any()`

Unregister a name.

<a name="wait_for_cleanup-2"></a>

### wait_for_cleanup/2 * ###

`wait_for_cleanup(Name, Retries) -> any()`

