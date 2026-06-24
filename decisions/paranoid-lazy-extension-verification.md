# Paranoid Verification Of Lazy Extended Messages

## Issue

Paranoid cache verification was force-loading every link inside a message before
checking the committed subset. With message extension and manifest routes, this
made intermediate lazy routing maps fan out into many remote loads even when the
current path only consumed one child.

## Options

- Keep force-loading the whole message. This is simple but makes lazy links
  non-lazy under paranoid cache writes.
- Disable paranoid checks for these messages. This defeats the acceptance test.
- Verify committed subsets and already-present uncommitted children, but leave
  lazy children to be verified when they are read.

## Decision

Use the third option. `hb_message:paranoid_verify/3` now decodes link keys,
recurses into already-present uncommitted children, skips unloaded lazy links,
and verifies only the materialized committed subset of a message that actually
has commitments.

This keeps paranoid verification meaningful without turning a single manifest
route lookup into a full-tree remote load.
