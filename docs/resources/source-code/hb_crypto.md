# [Module hb_crypto.erl](https://github.com/permaweb/HyperBEAM/blob/main/src/hb_crypto.erl)




Implements the cryptographic functions and wraps the primitives
used in HyperBEAM.

<a name="description"></a>

## Description ##

Abstracted such that this (extremely!) dangerous code
can be carefully managed.

HyperBEAM currently implements two hashpath algorithms:

* `sha-256-chain`: A simple chained SHA-256 hash.

* `accumulate-256`: A SHA-256 hash that chains the given IDs and accumulates
their values into a single commitment.

The accumulate algorithm is experimental and at this point only exists to
allow us to test multiple HashPath algorithms in HyperBEAM.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#accumulate-2">accumulate/2</a></td><td>Accumulate two IDs into a single commitment.</td></tr><tr><td valign="top"><a href="#count_zeroes-1">count_zeroes/1*</a></td><td>Count the number of leading zeroes in a bitstring.</td></tr><tr><td valign="top"><a href="#sha256-1">sha256/1</a></td><td>Wrap Erlang's <code>crypto:hash/2</code> to provide a standard interface.</td></tr><tr><td valign="top"><a href="#sha256_chain-2">sha256_chain/2</a></td><td>Add a new ID to the end of a SHA-256 hash chain.</td></tr><tr><td valign="top"><a href="#sha256_chain_test-0">sha256_chain_test/0*</a></td><td>Check that <code>sha-256-chain</code> correctly produces a hash matching
the machine's OpenSSL lib's output.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="accumulate-2"></a>

### accumulate/2 ###

`accumulate(ID1, ID2) -> any()`

Accumulate two IDs into a single commitment.
Experimental! This is not necessarily a cryptographically-secure operation.

<a name="count_zeroes-1"></a>

### count_zeroes/1 * ###

`count_zeroes(X1) -> any()`

Count the number of leading zeroes in a bitstring.

<a name="sha256-1"></a>

### sha256/1 ###

`sha256(Data) -> any()`

Wrap Erlang's `crypto:hash/2` to provide a standard interface.
Under-the-hood, this uses OpenSSL.

<a name="sha256_chain-2"></a>

### sha256_chain/2 ###

`sha256_chain(ID1, ID2) -> any()`

Add a new ID to the end of a SHA-256 hash chain.

<a name="sha256_chain_test-0"></a>

### sha256_chain_test/0 * ###

`sha256_chain_test() -> any()`

Check that `sha-256-chain` correctly produces a hash matching
the machine's OpenSSL lib's output. Further (in case of a bug in our
or Erlang's usage of OpenSSL), check that the output has at least has
a high level of entropy.

