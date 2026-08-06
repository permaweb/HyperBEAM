# `probability-time@1.0` — probability-weighted nametime pricing

- **Device:** `probability-time@1.0`
- **Status:** Draft

## State

`model` is the fixed model used by `probability-device`, default
`markov@1.0`. `target-occupancy=t` and `price-at-target=k` are required, with
`0 < t < 1` and `k > 0`. Prices are integer winston.

`names` is a map of leases:

```text
name: {
  deadline: Block,
  grace: Block,
  pricing: { weight: Float }
}
```

The weight of a held name is retained from its lease. A free name's default
weight is:

```text
w(name) = probability-device/likelihood(name, include-end=true)
```

If `weighting-device` is present, its `/weight` result replaces the likelihood.
It receives `name` and `probability`.

## Occupancy and price

At block `h`:

```text
u(h) = sum(lease.weight where h < lease.deadline)
r(u) = k * u * (1-t) / (t * (1-u))
```

Adding weight `w` for one block costs:

```text
c(u,w) = k * (1-t) / t * (log((1-u)/(1-u-w)) - w)
```

Require `u+w < 1`.

For `[a,b)`, partition at existing lease deadlines and sum
`duration * c(occupancy, w)` over the resulting slabs.

## `/price`

`price=Blocks`, `name`, and `token=ar` return the interval price rounded up to
whole winston. The interval begins after `duration` blocks from the state's
`spectrum-height`; `duration` defaults to zero.

## `/blocks`

`blocks=Winston`, `name`, and `token=ar` return the greatest whole number of
blocks whose `/price` does not exceed the payment:

```text
{
  blocks: Blocks,
  pricing: { weight: Weight }
}
```

The `pricing` value is opaque to the caller and MUST be retained on the lease.

Other keys use `message@1.0`. Errors return `invalid`.
