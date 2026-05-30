# Device: ~recorder@1.0

The `~recorder@1.0` device records process-local HyperBEAM event telemetry for
one AO-Core resolution and renders it as a flight recorder report.

## Chained Flight

Use `take-off` before the path you want to inspect, then end the path with
`land~recorder@1.0`:

```text
GET /~recorder@1.0/take-off/~meta@1.0/info/land~recorder@1.0
```

`take-off` starts recording in the current HTTP evaluation process and passes
the base message onward. `land` returns the captured flight and clears recorder
state.

For a relayed HTTP call:

```text
GET /~recorder@1.0/take-off/call~relay@1.0&relay-method=GET&relay-path=https%3A%2F%2Ficanhazip.com/land~recorder@1.0
```

## Formats

`land` and `record` default to `format=html`.

Use `format=raw` for the AO list of event messages:

```text
GET /~recorder@1.0/take-off/~meta@1.0/info/land~recorder@1.0&format=raw
```

Other supported formats are `json` and `text`.

## Stacks

Stack capture is off by default. Add `stack=true` to `take-off` or `record`
when you need stack traces:

```text
GET /~recorder@1.0/take-off&stack=true/~meta@1.0/info/land~recorder@1.0
```

`trace=true` is accepted as an alias for `stack=true`.

## Single-call Form

`record` wraps a target request and returns the rendered report directly:

```text
GET /~recorder@1.0/record?request=/~meta@1.0/info
```

This is useful when a client wants the recorder to perform the target request
itself, rather than composing a HyperPATH with explicit `take-off` and `land`
segments.
