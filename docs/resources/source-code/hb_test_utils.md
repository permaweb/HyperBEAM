# [Module hb_test_utils.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_test_utils.erl)




Simple utilities for testing HyperBEAM.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#run-4">run/4</a></td><td></td></tr><tr><td valign="top"><a href="#satisfies_requirements-1">satisfies_requirements/1*</a></td><td>Determine if the environment satisfies the given test requirements.</td></tr><tr><td valign="top"><a href="#suite_with_opts-2">suite_with_opts/2</a></td><td>Run each test in a suite with each set of options.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="run-4"></a>

### run/4 ###

`run(Name, OptsName, Suite, OptsList) -> any()`

<a name="satisfies_requirements-1"></a>

### satisfies_requirements/1 * ###

`satisfies_requirements(Requirements) -> any()`

Determine if the environment satisfies the given test requirements.
Requirements is a list of atoms, each corresponding to a module that must
return true if it exposes an `enabled/0` function.

<a name="suite_with_opts-2"></a>

### suite_with_opts/2 ###

`suite_with_opts(Suite, OptsList) -> any()`

Run each test in a suite with each set of options. Start and reset
the store(s) for each test. Expects suites to be a list of tuples with
the test name, description, and test function.
The list of `Opts` should contain maps with the `name` and `opts` keys.
Each element may also contain a `skip` key with a list of test names to skip.
They can also contain a `desc` key with a description of the options.

