# Type Schema Memoization

## Prompt As Understood

Phase 1 must vary base/request inputs from device function specs before cache
lookup and execution, while avoiding broad cache rewrites or persistent schema
side indexes.

## Issue

`hb_types` initially extracted abstract code on every resolution. The focused
AO-Core test vector suite kept behavior green, but benchmark eunit cases timed
out in repeated `code:get_object_code/1` calls.

## Options

- Keep extracting on every call. This is simplest but makes the resolver too
  slow to pass the existing suite.
- Reintroduce a global `persistent_term` or archive side index. This conflicts
  with the explicit direction and makes the loader/cache boundary heavier.
- Add process-local memoization keyed by module and BEAM identity. This keeps
  the parser pure at the protocol boundary, avoids global persistence, and is
  invalidated naturally on code version changes within a process.

## Decision

Use process-local memoization in `hb_types:extract/2`, keyed by module plus
`module_info(md5)`.

This is not a result cache and not a device archive side index. It is a local
parse memo so one resolver process does not repeatedly decode the same BEAM's
abstract code for every key resolution.
