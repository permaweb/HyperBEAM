# Manifest Private No-Store

## Issue

Manifest route results can be intermediate routing maps, not final content.
Under HTTP serving, node opts may force `cache-control: always`; storing those
intermediate maps causes paranoid cache writes to verify and materialize lazy
children that the current path did not consume. Invalid-path fallback results
also depend on local manifest options, so they must not be reused as generic AO
path results.

## Options

- Widen cache keys with server options. This makes cache semantics depend on
  operational configuration and spreads the fix outside the device.
- Force-load intermediate maps before returning them. This preserves caching but
  destroys lazy manifest routing.
- Mark intermediate/fallback manifest route maps with private `no-store`, and
  make private cache-control win over caller cache policy for cache writes.

## Decision

Use private `no-store` on manifest routing maps and fallback results. Private
cache-control is an internal resolver/device directive; it does not become a
public message key, and it prevents a local routing artifact from being cached
as a reusable AO result when the actual consumed child can still be loaded and
verified normally.
