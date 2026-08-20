# The history entries keep the shape they already have

## The prompt as I understand it

The brief sketches a history entry as:

```erlang
#{
    <<"device">> => <<"arweave-history@2.9">>,
    <<"kind">> => <<"reward-history">>,
    <<"height">> => Height,
    <<"value">> => EncodedRewardEntry,
    <<"previous">> => PreviousHead
}
```

## The issue

The persistent linked list already exists, on the branch this one starts from,
and its entries carry the same relationships under different names: `kind` and
`previous` as sketched, but the element's fields spelled out (`address`,
`hash-rate`, `reward`, `denomination`, or `block-interval`, `vdf-interval`,
`chunk-count`) rather than packed into one `value`, and `length` -- the number of
entries the history holds ending at this one, capped at the consensus window --
rather than `height`.

## Decision

Keep the existing shape, and add the two keys the brief asks the device to
offer: `push` and `to-binary`.

The differences are the same information under better names. Spelled-out fields
mean an entry read on its own decodes on its own, and mean the wire encoding
lives in one place -- the vendored `ar_serialize`, which both `from-binary' and
`to-binary` go through -- rather than in the entries as well. `length` is what
`cap/2` bounds and what a read counts down from; `height` would have to be
converted into it at every read, and a history whose length disagreed with its
heights would be wrong in a way nothing names.

What the brief is actually specifying here is the *structure* -- one message per
element, each committing to one new item and the prior head, shared tails, a
bounded window materialised against the identifier of the entry it ends at --
and that is what the existing list is. Rewriting the field names would churn a
working, tested representation for nothing.
