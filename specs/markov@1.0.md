# `markov@1.0` — finite-order Markov model

- **Device:** `markov@1.0`
- **Status:** Draft

## State

`model` is absent or:

```text
{
  order: N,
  samples: S,
  transitions: { ContextID: { SymbolID: PositiveInteger } }
}
```

`order` is the number of preceding symbols conditioning the next symbol;
`order=2` models `P(C | AB)`. The default is `2`.

Symbols are one-byte ASCII binaries. `|start|` and `|end|` are reserved boundary
symbols and never appear in generated output. They are their own IDs; an ASCII
symbol with value `B` has ID `t` followed by its three-digit decimal value. A
context ID is `c` followed by its ordered symbol IDs, separated by `-`. These
encodings are lowercase, unambiguous, and normative.

## Input

`target` names a path in the request; default `body`. Its value MUST be a binary
(one sample) or a list of binaries (independent samples, with no transition
between them). Every byte MUST be ASCII (`0..127`).

The effective `order` is the first present value from `Request/order`,
`Base/order`, `Base/model/order`, then `2`. It MUST be a non-negative integer.
An existing model with another order returns `order-mismatch`.

Each sample is interpreted as:

```text
[|start| x order] ++ symbols ++ [|end|]
```

## `/train`

Increment every observed `context -> next-symbol` count and increment `samples`
once per sample. Create `model` when absent. Return `{ok, Base'}` with only
`model` replaced.

## `/likelihood`

For each transition:

```text
P(next | context) = count(context, next) / sum(count(context, _))
```

The sequence likelihood is the product of all transitions, including `end`.
An unseen transition has likelihood zero. For a list, sample likelihoods are
multiplied and event counts summed; a list containing no samples has likelihood
one.

`result-mode=float` (default) returns the likelihood as a float.
`result-mode=integer` returns its exact reduced representation:

```text
{ numerator: N, denominator: D, events: E }
```

`E` counts scored transitions. `result-mode` is read from the request, then the
base.

## Derived scores

The following keys use the exact likelihood internally and return floats:

```text
/surprisal       = -log2(N / D)
/mean-surprisal  = surprisal / E
/perplexity      = 2 ^ mean-surprisal
```

For zero likelihood, each derived score returns `infinity`. Mean surprisal of
empty input is `0.0`; its perplexity is `1.0`.

## `/generate`

Begin with `order` `|start|` symbols, or reconstruct the context from the
request `body`. Sample by transition count until `|end|`.

`limit=false` (default) imposes no length limit. An integer `limit` is the total
desired output length, including an existing `body`; it is not an additional
byte count.

`seed` MAY be a binary. Without it, the implementation creates and returns a
runtime seed. Seeded draws are normative. For draw counter `i` and outcome total
`T`:

```text
R(i) = unsigned(SHA-256(seed || uint64be(i)))
```

Reject `R(i) >= floor(2^256 / T) * T`; otherwise select cumulative outcome
`R(i) mod T`, ordering symbol IDs bytewise. Increment `i` after every hash.
An outgoing count total greater than `2^256` is an invalid model.

Return:

```text
{
  body: Binary,
  seed: Seed,
  continues: false | { seed: Seed, counter: NextCounter }
}
```

`continues=false` means `|end|` was selected. A continuation means `limit` was
reached first. Passing the result back to `/generate` with a greater `limit`
MUST resume from its `body` and `continues` state. Resuming from `L1` to `L2`
MUST equal one call from the same seed with `limit=L2`.

Seedless generation is nondeterministic. Consensus execution MUST supply a seed.

## Other keys and errors

Other keys use `message@1.0`. The device performs no network or global-state
access.

Errors: `target-not-found`, `invalid-input`, `invalid-order`, `order-mismatch`,
`invalid-result-mode`, `model-not-found`, `invalid-model`, `invalid-seed`,
`invalid-continuation`, `invalid-limit`, `context-not-found`.
