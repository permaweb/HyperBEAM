# `wasi@1.0` — WASI-preview-1 host-function shim and virtual filesystem

- **Device name:** `wasi@1.0`
- **Depends-on:** `message@1.0` (identity reads/writes, `keys`, `set`, private-key rules), `wasm-64@1.0` (the WASM execution device that drives this shim as its standard-library / import handler). Both specs are provided to reimplementers.
- **Status:** Draft

## 1. Overview

`wasi@1.0` provides a small, **deterministic** subset of the
[WASI preview-1](https://github.com/WebAssembly/WASI/blob/main/legacy/preview1/docs.md)
host interface to a WASM module executed by `wasm-64@1.0`. It does two things:

1. It exposes a **virtual filesystem (VFS)** as ordinary message state — a map of
   path → file-content, plus a **file-descriptor table** mapping integer fd
   numbers to `{filename, offset}` records. Standard streams (`stdin`, `stdout`,
   `stderr`) are pre-created files in this VFS at fds `0`, `1`, `2`.
2. It implements a handful of WASI-preview-1 import functions
   (`fd_write`, `fd_read`, `path_open`, `clock_time_get`) as **resolved keys**.
   When a WASM module imports a function from the WASI module name
   `wasi_snapshot_preview1`, `wasm-64@1.0` routes the import call to this device,
   which reads/writes the calling instance's linear memory and updates the VFS
   state accordingly.

The device exists so that ordinary WASI programs (e.g. a language runtime that
`printf`s to stdout) can run on `wasm-64@1.0` and have their output captured as
inspectable message state, deterministically and without granting the guest any
real host I/O, clock, or entropy. Every implemented call is **failure-closed**
with respect to non-determinism: the clock is refused rather than read from the
host, and no entropy source is exposed at all.

This spec pins exactly which WASI functions are implemented, each one's effect on
message state, the argument/return convention of the import bridge, and the
determinism guarantees. The internal data structures backing the VFS are out of
scope; only the observable state (the VFS map, the fd table, the bytes written
into guest memory, the values returned to the guest) is normative.

## 2. Concepts & terminology

- **WASM instance:** a live, in-memory WASM execution context owned by
  `wasm-64@1.0` and reachable through the message's **private** state. This shim
  reads from and writes to that instance's linear memory. The instance handle is
  opaque; this spec only requires the ability to (a) read a length-delimited byte
  range at a memory pointer, (b) read a NUL-terminated string at a memory
  pointer, and (c) write a byte string at a memory pointer — the three primitives
  `wasm-64@1.0` exposes for memory access. The instance lives under the private
  key `<output-prefix>/instance` of the execution state (see `wasm-64@1.0`);
  with the default empty output-prefix this is the private key `instance`, and
  under a `wasm` output-prefix it is the private key `wasm/instance`.
- **Virtual filesystem (VFS):** a sub-message stored at the public key `vfs` of
  the device's state. Its leaves are **file contents** (binaries) addressed by
  their **path** (a binary like `/dev/stdout`). Directories are nested
  sub-messages; files are binary leaves. The VFS is ordinary, inspectable,
  serialisable message state.
- **File-descriptor table (fd table):** a sub-message stored at the public key
  `file-descriptors` of the device's state. Its keys are the **decimal-string**
  fd numbers (`"0"`, `"1"`, `"2"`, …); each value is a **descriptor record**, a
  message with at least `filename` (the VFS path the fd refers to) and `offset`
  (a non-negative integer byte cursor), and optionally `index` (the fd's own
  number, set by `path_open`). A descriptor record MAY additionally carry a
  `data` field (see §4 `fd_write` and §8 Open questions).
- **iovec:** the WASI-preview-1 scatter/gather descriptor: a 16-byte structure in
  guest memory holding a little-endian 64-bit **buffer pointer** followed by a
  little-endian 64-bit **buffer length**. A vectored I/O call is given a pointer
  to an array of iovecs and a count.
- **errno:** the WASI-preview-1 numeric error code returned by every WASI
  function as its (single) WASM return value. `0` (`success`) means the call
  succeeded; any non-zero value is an error.
- **The import bridge:** the calling convention by which `wasm-64@1.0` turns a
  guest's WASI import call into an AO-Core key resolution on this device, and
  turns this device's result back into the guest's return values and the new
  execution state. Pinned normatively in §3 and §5.
- **Execution state (the "state"):** the full `wasm-64@1.0` execution message at
  the moment of an import call — the message that carries the VFS, the fd table,
  and (privately) the live WASM instance. The import bridge passes this message
  to the device and expects the (possibly mutated) message back.

The build-device skill defines the AO-Core substrate (messages, keys,
resolution, private keys, default vs explicit-key devices); terms it defines are
not redefined here.

## 3. Device interface

- **Dispatch shape:** **explicit-keys (named functions), no default handler.**
  The device answers exactly the keys named in §4 by their key name; it does
  **not** install a catch-all that answers arbitrary keys. (Equivalently: it
  exposes its named keys and nothing else; any unimplemented key falls through to
  the base identity device `message@1.0`, and any *WASI* function it does not
  implement is handled by `wasm-64@1.0`'s undefined-import stub, not by this
  device — see §9.) Realise this in `info/1` with **either** an `exports` list of
  the named keys (unlisted keys auto-delegate to `message@1.0`) **or** a
  `default => message@1.0` handler (the `dev_test` idiom — named functions resolve
  by arity, everything else delegates); both give the same fall-through. The
  message-manipulation keys (`keys`, `set`, `set-path`, `remove`) are inherited
  unmodified from `message@1.0` and MUST NOT be shadowed.

- **Two state-access conventions (normative — an implementer MUST reproduce
  both).** The implemented keys split into two groups by how they locate the
  execution state and the WASM instance. This split is an observable part of the
  contract because it determines what `Base` an implementation must accept:

  - **State-wrapped keys** — `fd_write`, `fd_read`, `clock_time_get`. The import
    bridge (see below) delivers these a `Base` that is **not** the execution
    state itself but a wrapper carrying the execution state under the key
    `state`. These handlers MUST read the execution state from `Base/state`, MUST
    read the live WASM instance from that state's **private** `wasm/instance`
    key, and MUST return their result as a wrapper `{ok, #{ state => NewState,
    results => [...] }}` (§5).
  - **Direct-state keys** — `path_open`. This handler treats `Base` **as** the
    execution state directly: it reads the fd table from `Base/file-descriptors`
    and the WASM instance from `Base`'s **private** `wasm/instance` key (the
    bridge's `<output-prefix>/instance` placement — §4), and returns
    `{ok, #{ state => NewState, results => [...] }}` where `NewState` is derived
    from `Base` directly (not from `Base/state`).

  This asymmetry is a property of the reference implementation that a
  bit-compatible reimplementation must preserve. See §8 Open questions for why it
  exists and the latitude a reimplementer has.

- **How the bridge reaches this device.** This device is **not** invoked
  directly by callers. `wasm-64@1.0` is configured (by this device's `init`, §4)
  so that the WASI module name `wasi_snapshot_preview1` maps to a sub-message
  whose `device` is `wasi@1.0`. When the guest calls an imported WASI function
  named `F`, `wasm-64@1.0`:
  1. builds a request message with `path = import`, `module` = the WASI module
     name (`wasi_snapshot_preview1`), `func` = `F`, `args` = the list of integer
     arguments the guest passed, and `func-sig` = the function's WASM type
     signature (a binary);
  2. its own `import` key rewrites the path to
     `<output-prefix>/stdlib/<module>/<F>`, attaches the whole execution state
     under `<output-prefix>/stdlib/<module>/state`, and resolves that path;
  3. that resolution lands on this device (because
     `<output-prefix>/stdlib/<module>` is the `wasi@1.0` sub-message), with the
     request path's final segment being `F` (so key `F` is resolved here) and the
     `Base` carrying the execution state under `state`;
  4. if this device does not implement `F`, the resolution returns
     `not_found` and `wasm-64@1.0` falls back to its undefined-import stub, which
     records the call and returns errno `0` with no state change (§9).

- **Message shape (the device's own state).** After `init`, the execution
  message carries (publicly):

  | Key | Type | Meaning |
  |---|---|---|
  | `vfs` | message | the virtual filesystem (§5); after init contains `dev/stdin`, `dev/stdout`, `dev/stderr`, each an empty binary `<<>>` |
  | `file-descriptors` | message | the fd table (§5); after init contains fds `0`,`1`,`2` |

  and (under the `wasm-64@1.0` standard-library wiring, written by `init`) the key
  `<output-prefix>/stdlib/wasi_snapshot_preview1` set to the sub-message
  `#{ device => "wasi@1.0" }`. With the canonical `wasm` output-prefix this key is
  `wasm/stdlib/wasi_snapshot_preview1`; an implementation MUST write it under the
  same output-prefix `wasm-64@1.0` uses so that the bridge path matches.

- **Request shape (the bridge request, for an implemented WASI call).** The
  request message this device's handlers read carries:

  | Key | Type | Meaning |
  |---|---|---|
  | `args` | list of integers | the guest's call arguments, in order |
  | `func-sig` | binary | the WASM type signature of the called function (informational; logged, not required for behaviour) |
  | `module`, `func` | binary | the WASI module name and function name (consumed by `wasm-64@1.0`'s `import` routing; a handler MAY ignore them) |
  | `path` | binary | the resolution path, ending in the function name |

  All keys are lowercase, hyphenated binary on the wire (`func-sig`,
  `file-descriptors`). The `args` list elements are the integer values the guest
  passed (pointers and counts are 64-bit unsigned integers in the Memory-64 ABI
  of `wasm-64@1.0`).

## 4. Resolved keys (normative)

For each key: **Reads** (inputs from `Base`/`Req`/state), **Behaviour**,
**Returns**, **Side effects**.

### `init` — set up the WASI environment

- **Reads:** the base execution message (`Base`); node options.
- **Behaviour:** Initialise the WASI environment on the message by writing three
  things (the result is the augmented message):
  1. **Standard-library wiring.** Set the key
     `<output-prefix>/stdlib/wasi_snapshot_preview1` to the sub-message
     `#{ <<"device">> => <<"wasi@1.0">> }`, so that `wasm-64@1.0`'s import bridge
     routes `wasi_snapshot_preview1` calls to this device. **"Set the key" means an
     AO-Core `set` (e.g. `hb_ao:set`) that NESTS the slashed path into a walkable
     sub-message — NOT a flat top-level binary key `<<"wasm/stdlib/...">>`, which
     the bridge cannot path-resolve into.** With the canonical output-prefix
     `wasm`, the written key is `wasm/stdlib/wasi_snapshot_preview1`. An
     implementation MUST use the same output-prefix as the surrounding
     `wasm-64@1.0` device (typically `wasm`).
  2. **The fd table.** Set the public key `file-descriptors` to the initial fd
     table:
     - `"0"` → `#{ filename => "/dev/stdin",  offset => 0 }`
     - `"1"` → `#{ filename => "/dev/stdout", offset => 0 }`
     - `"2"` → `#{ filename => "/dev/stderr", offset => 0 }`
  3. **The VFS.** Set the public key `vfs` to the initial virtual filesystem:
     `#{ dev => #{ stdin => <<>>, stdout => <<>>, stderr => <<>> } }` — i.e. the
     files `/dev/stdin`, `/dev/stdout`, `/dev/stderr`, each an empty binary.
- **Returns:** `{ok, AugmentedMessage}`.
- **Side effects:** none external. (No WASM instance is created here; the
  instance is created by `wasm-64@1.0`'s own `init`. In a stack, this device's
  `init` runs alongside `wasm-64@1.0`'s `init`; see §9.)

### `compute` — no-op pass-through

- **Reads:** the base message (`Base`).
- **Behaviour:** Return the base message unchanged. This device performs no
  computation of its own at `compute` time; the guest's WASI calls happen as
  side-effects of `wasm-64@1.0`'s `compute` invoking the WASM function, which
  drives the import bridge into this device's per-call handlers.
- **Returns:** `{ok, Base}`.
- **Side effects:** none.

### `stdout` — read the captured stdout buffer

- **Reads:** the VFS path `vfs/dev/stdout` of the message (`Base`).
- **Behaviour:** Return the current contents of the `/dev/stdout` file — the
  bytes the guest has written to fd `1` so far.
- **Returns:** the stdout binary (the accumulated bytes; `<<>>` if nothing has
  been written), or `not_found` if the VFS/stdout file is absent.
- **Side effects:** none. (Convenience accessor; equivalent to resolving
  `vfs/dev/stdout` via `message@1.0`.)

### `fd_write` — vectored write to a file descriptor (state-wrapped)

WASI-preview-1 `fd_write(fd, iovs_ptr, iovs_len, nwritten_ptr) -> errno`.

- **Reads:**
  - the execution state from `Base/state` (§3); the live WASM instance from that
    state's private `wasm/instance`;
  - from `Req`: `args` = `[fd, iovs_ptr, iovs_len, nwritten_ptr | _]` (only the
    first four are used) and `func-sig` (logged only);
  - from the state: the descriptor record at `file-descriptors/<fd>` (with `<fd>`
    rendered as a decimal string), specifically its `filename` and `offset`; and
    the current VFS file content at `vfs/<filename>` (the splice source — §4
    Behaviour);
  - from guest memory: for each iovec, the 16-byte iovec structure at the current
    pointer, then `len` bytes at the iovec's buffer pointer.
- **Behaviour:** Iterate the `iovs_len` iovecs starting at `iovs_ptr`, advancing
  the iovec-array pointer by **16 bytes** per iovec and decrementing the
  remaining count by one each step. For each iovec:
  1. Parse the iovec (read 16 bytes at the array pointer; the first little-endian
     u64 is the data pointer, the second is the length).
  2. Read `length` bytes of data from guest memory at the data pointer.
  3. Splice that data into the target file **at the descriptor's current
     offset**: the new file content is `Before ++ Data ++ After`, where the file
     is split at `offset` into `Before` (first `offset` bytes) and `After` (the
     **full** remainder from `offset`). This is an **insert** at `offset`: `Data`
     is placed at `offset` and the old remainder (`After`) follows it (shifting
     right); at `offset == byte_size(file)` it appends. The reference implements
     exactly `Before ++ Data ++ After` — there is **no** in-place overwrite of a
     spanned region (disregard any "overwrite" reading). See §8.
  4. Advance the descriptor's `offset` by the number of bytes written in this
     iovec, and write the spliced content back to the VFS file named by the
     descriptor's `filename` (key `vfs/<filename>`).
  - The "current file content" that step 3 splices into is read from the **VFS
    file** at `vfs/<filename>` — the **same** key step 4 writes back to, so the VFS
    file is the single source of truth. Do **not** read from a separate descriptor
    `data` field (that was a latent inconsistency: read-source ≠ write-target). It
    defaults to the empty binary if absent.
- After all iovecs are consumed (remaining count reaches 0): write the **total
  bytes written across all iovecs** as a little-endian unsigned 64-bit integer to
  guest memory at `nwritten_ptr`.
- **Returns:** `{ok, #{ state => NewState, results => [0] }}` — errno `0`
  (success). `NewState` is the execution state with the target VFS file updated
  and the descriptor's `offset` advanced.
- **Side effects:** writes `nwritten_ptr` in guest memory; updates the VFS file
  and the descriptor offset in the returned state. No external I/O — fd `1`
  (`/dev/stdout`) and fd `2` (`/dev/stderr`) are captured into the VFS, **not**
  forwarded to any real stream.

### `fd_read` — vectored read from a file descriptor (state-wrapped)

WASI-preview-1 `fd_read(fd, iovs_ptr, iovs_len, nread_ptr) -> errno`.

- **Reads:**
  - the execution state from `Base/state`; the live WASM instance from the
    state's private `wasm/instance`;
  - from `Req`: `args` = `[fd, iovs_ptr, iovs_len, nread_ptr | _]` and `func-sig`
    (logged only);
  - from the state: the descriptor record at `file-descriptors/<fd>` (`filename`
    and `offset`), and the VFS file content at `vfs/<filename>`;
  - from guest memory: each iovec's 16-byte structure (to obtain the destination
    buffer pointer and capacity).
- **Behaviour:** Iterate the `iovs_len` iovecs starting at `iovs_ptr`, advancing
  the array pointer by **16 bytes** per iovec and decrementing the count. For
  each iovec:
  1. Parse the iovec to get the destination pointer and the buffer length `len`.
  2. Compute the read size as `min(len, byte_size(file) - offset)` (never reads
     past end-of-file; never negative once `offset <= byte_size(file)`).
  3. Read `read_size` bytes from the VFS file starting at `offset`.
  4. Write those bytes into guest memory at the iovec's destination pointer.
  5. Advance the descriptor's `offset` by `read_size`.
- After all iovecs are consumed: write the **total bytes read** as a
  little-endian unsigned 64-bit integer to guest memory at `nread_ptr`.
- **Returns:** `{ok, #{ state => NewState, results => [0] }}` — errno `0`
  (success). `NewState` has the descriptor's `offset` advanced by the total bytes
  read.
- **Side effects:** writes the read bytes into guest memory and writes
  `nread_ptr`; advances the descriptor offset in the returned state. The VFS file
  is not modified.

### `path_open` — open a path into a file descriptor (direct-state)

WASI-preview-1
`path_open(fd, dirflags, path_ptr, path_len, oflags, rights_base, rights_inheriting, fdflags, opened_fd_ptr) -> errno`.

> Note: the reference implementation consumes only the **first three** of the
> nine standard arguments and uses an **out-of-band return shape** for the opened
> fd. The behaviour pinned here is the reference behaviour, not the full WASI
> ABI; see §8 Open questions.

- **Reads:**
  - the execution state **as `Base` directly** (§3); the live WASM instance from
    `Base`'s private `wasm/instance` key — the bridge places the instance at
    `<output-prefix>/instance` = `wasm/instance` (same as `fd_write`/`fd_read`);
    reading a bare `instance` makes `path_open` unreachable under the canonical
    `wasm` prefix;
  - from `Req`: `args` = `[fd_ptr, lookup_flag, path_ptr | _]` (the first three
    integers; the remaining six standard `path_open` arguments are ignored);
  - from guest memory: the NUL-terminated path string at `path_ptr`;
  - from the state: the fd table `file-descriptors` (to count existing fds), and
    any existing descriptor at `vfs/<path>`.
- **Behaviour:**
  1. Read the path string at `path_ptr` (a NUL-terminated UTF-8/byte string).
  2. Look up the key `vfs/<path>` in the state. If a descriptor record already
     exists there, reuse it. Otherwise create a new descriptor record
     `#{ index => N, filename => <path>, offset => 0 }`, where `N` =
     (number of keys currently in the fd table) + 1 — i.e. the next sequential fd
     number.
  3. Write that descriptor record into the state at `vfs/<path>` (the descriptor
     is stored at the VFS path, alongside/over file content — see §8).
  4. Return errno `0` and the chosen fd number `N`.
- **Returns:** `{ok, #{ state => NewState, results => [0, N] }}` — a **two-element**
  results list: errno `0` followed by the new fd index `N`. (The reference does
  **not** write `N` to `opened_fd_ptr` in guest memory; it returns it as a second
  WASM return value. See §8.)
- **Side effects:** writes a descriptor record into the state at `vfs/<path>`.
  Does not allocate or write guest memory beyond reading the path string.

### `clock_time_get` — refuse the clock (deterministic) (state-wrapped)

WASI-preview-1 `clock_time_get(clock_id, precision, time_ptr) -> errno`.

- **Reads:** the execution state from `Base/state`. Arguments are **ignored**.
- **Behaviour:** Do **not** read any host clock and do **not** write a timestamp
  to guest memory. Return a **non-zero errno** to signal that the clock is
  unavailable. This is how the device keeps execution deterministic: rather than
  return a host-dependent time, it refuses the call.
- **Returns:** `{ok, #{ state => State, results => [1] }}` — errno `1`. The state
  is returned unchanged.
- **Side effects:** none. No guest memory is written; no host clock is read.

### Inherited message-manipulation keys (`keys`, `set`, `set-path`, `remove`, `id`, …)

- These are **not** implemented by this device and MUST resolve via the base
  identity device `message@1.0` over the same message (the VFS and fd table are
  ordinary keys and are listed/mutated/serialised by `message@1.0` as for any
  message). The device MUST NOT shadow them.

## 5. Data formats & encodings

- **VFS layout.** The VFS is a message at the `vfs` key. A file is addressed by
  its absolute path with the leading `/` folded into the key path:
  `/dev/stdout` ⇄ `vfs/dev/stdout`. File content is a **binary**; directories are
  nested messages. Reading/writing a file is `get`/`set` of the key
  `vfs/<path-without-leading-slash>`: **normalise away the leading `/`** so the key
  is `vfs/dev/stdout` (single slash), matching what `init` writes. A descriptor's
  stored filename and the key its content is read/written under MUST address the
  same leaf — strip the leading `/` on both; do **not** concatenate `vfs/` ++
  `/dev/stdout` into a `vfs//dev/stdout` double-slash key (normalise first). See §8.
- **fd table layout.** `file-descriptors` is a message whose keys are the
  **decimal-string** fd numbers (`"0"`, `"1"`, …). Each value is a descriptor
  record message with:
  - `filename` — binary, the VFS path the fd refers to (e.g. `/dev/stdout`);
  - `offset` — non-negative integer, the byte cursor;
  - `index` — integer, the fd's own number (present on descriptors created by
    `path_open`; not required on the initial `0`/`1`/`2` descriptors);
  - `data` — (optional) the file content the write path splices into (§8).
  The fd number used to index this table is the integer argument from the guest,
  rendered as its base-10 decimal string (no leading zeros; `0` renders as `"0"`).
- **iovec encoding (WASI-preview-1).** A 16-byte structure: bytes 0–7 are the
  buffer pointer as a **little-endian unsigned 64-bit** integer; bytes 8–15 are
  the buffer length as a **little-endian unsigned 64-bit** integer. An array of
  iovecs is contiguous; consecutive iovecs are **16 bytes** apart.
- **Out-parameter encoding.** `nwritten` (for `fd_write`) and `nread` (for
  `fd_read`) are written to guest memory as **little-endian unsigned 64-bit**
  integers at the pointer the guest supplied.
- **Pointers and counts** in `args` are integers (the Memory-64 ABI of
  `wasm-64@1.0` uses 64-bit addresses). The device treats them as plain integers.
- **errno / results encoding.** A handler's result list is the sequence of WASM
  values handed back to the guest as the imported function's return values.
  - `fd_write`, `fd_read`: `[0]` (single errno, success).
  - `clock_time_get`: `[1]` (single errno, non-success).
  - `path_open`: `[0, N]` (errno `0`, then the fd number `N`).
  The undefined-import stub (in `wasm-64@1.0`, for any WASI function this device
  does not implement) returns `[0]`.
- **Bridge result wrapper.** Every per-call handler returns
  `{ok, #{ state => <new execution state>, results => <list of integers> }}`.
  `wasm-64@1.0`'s bridge reads `state` (the new execution state to continue with)
  and `results` (the values to return to the guest) from this map. An
  implementation MUST use exactly these two keys (`state`, `results`).
- **String reads.** A path read by `path_open` is the NUL-terminated byte string
  at the given pointer (read up to and excluding the first `0x00`).
- **No base64url/hex concern at this layer.** The device emits no identifiers,
  commitments, or content-addressed values of its own; IDs/commitments of the
  messages flowing through are governed by `message@1.0`.

## 6. Ordering, freshness & caching

- **Determinism — the central guarantee.** Every implemented call is a pure
  function of (the execution state, the guest arguments, the contents of guest
  memory). Specifically:
  - **Clock:** `clock_time_get` never reads a host clock; it returns errno `1`
    and writes no time. There is no path by which wall-clock time enters the
    computation.
  - **Entropy:** the device implements **no** randomness function (`random_get`
    is not provided). A guest call to `random_get` (or any other unimplemented
    WASI function) is handled by `wasm-64@1.0`'s undefined-import stub, which
    returns errno `0` and changes no state — so it injects no entropy either. An
    implementation MUST NOT add a host-entropy source.
  - **No other host I/O:** there is no real filesystem, network, environment, or
    args access; `fd_write`/`fd_read` operate solely on in-message VFS state.
  Consequently a `wasm-64@1.0` + `wasi@1.0` execution is reproducible: the same
  inputs yield the same VFS/fd state and the same guest return values, on any
  node, at any time.
- **Ordering.** Within a single `fd_write`/`fd_read`, iovecs are processed in
  array order (ascending pointer), and bytes are spliced/read at the descriptor's
  monotonically advancing `offset`. Across calls, the VFS reflects the cumulative
  effect of every write in call order — i.e. the guest's own ordering of WASI
  calls determines the final `stdout`/`stderr`/file contents.
- **Caching.** The device performs no result caching of its own; it operates
  purely on the supplied message and request. Whether a `compute` that drives
  these calls is cached is governed by the surrounding node/substrate
  configuration and by `wasm-64@1.0` (this device adds no freshness rules).
- **Mutation-at-constant-path.** The VFS/fd table are ordinary keys of the
  execution message; their visibility follows `message@1.0` semantics and the
  hashpath of the carrying message.

## 7. Security & authority

- **Capability isolation (failure-closed).** The guest is granted **no** real
  host capabilities. It cannot read the host clock (refused), cannot obtain
  entropy (unimplemented), cannot touch a real filesystem or network, and cannot
  read host environment/args. All "I/O" is confined to in-message VFS state. This
  is the security purpose of the shim: a WASI program runs sandboxed, with its
  side-effects materialised as inspectable, deterministic message state.
- **No authorisation of its own.** The device requires no commitment/signature to
  invoke and performs no authority checks; it is reached only as the import
  handler of an already-authorised `wasm-64@1.0` execution. Authority over the
  execution is whatever `wasm-64@1.0` and the surrounding stack enforce.
- **Private instance handle.** The live WASM instance is read from the message's
  **private** state and is never part of the public, committed, or serialised
  message surface. `message@1.0`'s private-key rules apply (the instance is never
  returned by `keys`/`get` of public keys, never committed).
- **Unknown calls are inert, not fatal.** A WASI function this device does not
  implement does not crash the guest; `wasm-64@1.0`'s stub records it and returns
  success (errno `0`) with no state change. An implementation MUST NOT turn an
  unimplemented WASI call into a hard error at this layer.

## 8. Errors

This device originates very few errors; most "error" signalling is via WASI
**errno return values**, not AO-Core error atoms.

- **errno `1` from `clock_time_get`** — always returned (the clock is
  deliberately unavailable). Not an AO-Core error; a normal `{ok, …}` result with
  `results => [1]`.
- **errno `0`** — success, returned by `fd_write`, `fd_read`, `path_open`, and
  the undefined-import stub.
- **`not_found`** — returned (via `message@1.0`) when resolving a key the device
  does not implement and that does not exist on the message (e.g. `stdout` when
  the VFS has no `dev/stdout`). The import bridge interprets a `not_found` for an
  unimplemented WASI **function** as "fall through to the undefined-import stub"
  (§3, §9).
- **No hyphenated error atoms are defined by this device.** (It performs no
  validation that produces named errors; malformed guest memory or
  out-of-range pointers manifest as failures of the underlying memory-access
  primitives of `wasm-64@1.0`, whose error surface is defined by that spec, not
  this one.) An implementation SHOULD surface such low-level failures exactly as
  `wasm-64@1.0`'s memory primitives do, and MUST NOT silently swallow them into a
  spurious `success`.

## 9. Composition

- **Driven by `wasm-64@1.0` as its standard library.** This device is designed to
  be composed with `wasm-64@1.0`, not used alone. The canonical composition is a
  **`stack@1.0` fold** of `[wasi@1.0, wasm-64@1.0]` (in that order) whose
  `output-prefix`/`input-prefix` are set to `wasm`, running the keys `init` then
  `compute`. Concretely (the reference test wiring):
  - `device-stack = [wasi@1.0, wasm-64@1.0]`,
  - `output-prefixes = [wasm, wasm]`, `input-prefixes` as needed,
  - `stack-keys = [init, compute]`.
  On `init`, `wasi@1.0`'s `init` writes the VFS, the fd table, and the
  `wasm/stdlib/wasi_snapshot_preview1` → `{device: wasi@1.0}` wiring, while
  `wasm-64@1.0`'s `init` boots the WASM instance and installs the import
  resolver. On `compute`, `wasm-64@1.0` runs the WASM function; each WASI import
  the guest performs re-enters AO-Core via the import bridge and is resolved on
  this device per §4.
- **The import-bridge round trip.** For an implemented call, the bridge path is
  `wasm/stdlib/wasi_snapshot_preview1/<func>`; the execution state is attached
  under `wasm/stdlib/wasi_snapshot_preview1/state`; the handler returns
  `{state, results}`; the bridge extracts both and returns `results` to the guest
  while continuing with `state`. For an **un**implemented call, the bridge gets
  `not_found`, records the call (under a results/undefined-calls list in the
  state), and returns errno `0`.
- **Output capture.** Because fds `1`/`2` map to VFS files `/dev/stdout` /
  `/dev/stderr`, a guest's stdout/stderr are observable after `compute` as
  `vfs/dev/stdout` / `vfs/dev/stderr` (or via the `stdout` convenience key). This
  is the primary way callers consume a WASI program's output.
- **Serialisability.** The VFS and fd table are plain message state, so the whole
  WASI-augmented execution message round-trips through the message codecs (a
  reimplementation MUST keep VFS/fd-table values in the ordinary message domain —
  binaries, integers, nested messages — so they serialise; the live instance
  stays private and is reconstructed by `wasm-64@1.0`'s snapshot/restore, not by
  this device).

## 10. Conformance (normative checklist)

An implementation MUST exhibit all of the following. Items are checkable by
composing the device with `wasm-64@1.0` (per §9) and running a WASI guest, or by
resolving the device's own keys directly.

1. **init populates VFS + fd table + wiring.** After `init`, the message has
   `vfs/dev/stdin`, `vfs/dev/stdout`, `vfs/dev/stderr` each `= <<>>`;
   `file-descriptors/0`, `/1`, `/2` with `filename` `/dev/stdin`, `/dev/stdout`,
   `/dev/stderr` and `offset = 0`; and a `…/stdlib/wasi_snapshot_preview1` key
   whose value carries `device = wasi@1.0`. The VFS and fd table survive a
   message-codec round-trip (serialisable).
2. **Only the four host functions are implemented.** The device answers exactly
   `path_open`, `fd_write`, `fd_read`, `clock_time_get` as WASI host functions
   (plus the lifecycle/inspection keys `init`, `compute`, `stdout`). Any other
   WASI function resolves to `not_found` at this device and is handled by
   `wasm-64@1.0`'s undefined-import stub (errno `0`, no state change) — verifiable
   by a guest that imports an unimplemented WASI call and observes it succeed
   inertly.
3. **fd_write captures stdout.** A guest that writes bytes `B` to fd `1` causes
   `vfs/dev/stdout` to equal `B` (for a single write from offset 0); `nwritten`
   in guest memory equals `byte_size(B)` (little-endian u64); the call returns
   errno `0`. The same for fd `2` → `vfs/dev/stderr`. No bytes are emitted to any
   real host stream.
4. **fd_write is offset-positioned and vectored.** Multiple iovecs in one
   `fd_write` are processed in order, 16 bytes apart, and their data is
   concatenated into the file in iovec order; the descriptor `offset` advances by
   the total bytes written; the returned `nwritten` is the **sum** across iovecs.
   A subsequent write continues at the advanced offset (splicing `Before ++ Data
   ++ After` at that offset).
5. **fd_read is bounded by EOF and vectored.** A `fd_read` of a fd whose file has
   `K` remaining bytes from `offset` fills each iovec with up to its capacity but
   never past EOF (`min(len, size - offset)` per iovec), writes the read bytes
   into guest memory at each iovec pointer, advances `offset` by the total read,
   writes `nread` (sum, little-endian u64) to guest memory, and returns errno `0`.
   Reading at EOF reads zero bytes and returns errno `0`.
6. **path_open creates/returns an fd.** Calling `path_open` with a path that has
   no existing descriptor creates a descriptor with the next sequential index
   `(#fds)+1`, stores it at the VFS path, and returns `results = [0, index]` (a
   two-element list: errno then the new fd number). Calling it again for the same
   path reuses the existing descriptor.
7. **clock_time_get is refused deterministically.** `clock_time_get` returns
   `results = [1]` (errno `1`), writes no timestamp to guest memory, reads no host
   clock, and leaves the state unchanged — independent of wall-clock time and
   reproducible across nodes.
8. **No entropy source.** The device exposes no `random_get` (or any other)
   entropy function; a guest requesting randomness goes through the inert stub.
   An implementation that adds a real host-entropy source is non-conformant.
9. **State-access conventions.** `fd_write`/`fd_read`/`clock_time_get` read the
   execution state from `Base/state` and (for the I/O calls) the instance from
   that state's private `wasm/instance`, and return `{ok, #{state, results}}`;
   `path_open` reads the state as `Base` directly and the instance from `Base`'s
   private `instance`, and returns `{ok, #{state, results}}` derived from `Base`.
   Both return shapes use exactly the keys `state` and `results`.
10. **Bridge result contract.** Each per-call handler returns
    `{ok, #{ state => NewState, results => Ints }}`; the values in `results` are
    the integers returned to the guest as the imported function's WASM results,
    and `NewState` is the execution state continued with. Out-parameters
    (`nwritten`, `nread`) are written into guest memory as little-endian u64; they
    are **not** carried in `results`.
11. **Determinism end-to-end.** Running the same WASI guest with the same inputs
    twice yields byte-identical `vfs/dev/stdout` (and other VFS/fd state) and
    identical guest return values.
12. **Inherited mutation/inspection.** `keys`, `set`, `set-path`, `remove`, `id`
    on a WASI-augmented message behave exactly as `message@1.0` defines (the
    device does not shadow them); the private WASM-instance handle is never
    exposed by `keys`/public `get` and is never committed.

## 11. Out of scope

- The **internal representation** of the VFS, the fd table, and the descriptor
  records (only the observable key/value content and the bytes exchanged with
  guest memory are normative).
- The **WASM instance** and the memory-access primitives (read range, read
  string, write range): defined by `wasm-64@1.0`. This spec only requires their
  existence and little-endian/NUL-terminated semantics as used above.
- The **full WASI-preview-1 ABI**: this device implements a deliberately small
  subset, and `path_open` in particular diverges from the standard ABI (§8 Open
  questions). A reimplementation targets *this device's* observable behaviour,
  not WASI conformance.
- The **import-bridge mechanics of `wasm-64@1.0`** (path rewriting,
  state-attachment, stub fallback): defined by `wasm-64@1.0`; reproduced here only
  to the extent needed to pin this device's argument/return contract.
- The **snapshot/restore** of WASM execution state across messages (a
  `wasm-64@1.0` concern); this device keeps no live state of its own beyond the
  inspectable VFS/fd table.
- Performance, storage strategy, and the exact `func-sig` string values (logged,
  not behaviourally significant).

## Open questions

These are ambiguities or apparent defects in the reference contract that a
reimplementer should be aware of. Where the reference is self-inconsistent, a
bit-compatible reimplementation must match the reference's *observable* behaviour
on the paths the reference actually exercises; the items below flag where that
behaviour is under-pinned or where the standard WASI ABI is not followed.

1. **`fd_write` `data` vs `vfs` source-of-truth.** The write path reads the
   "current file content" it splices into from the **descriptor record's `data`
   field** (`file-descriptors/<fd>/data`), but writes the spliced result to the
   **VFS file** (`vfs/<filename>`). The initial descriptors (`0`/`1`/`2`) have no
   `data` field, and nothing in the reference populates it, so the first write to
   stdout/stderr splices into an absent/empty `data` (effectively starting from
   `<<>>`). A reimplementer SHOULD treat the file's prior content as the VFS file
   content (which is what readers and the `stdout` accessor observe); the
   reference's use of `data` here appears to be a latent inconsistency. The
   observable result for the common case (sequential writes to stdout from
   offset 0) is the concatenation of the written bytes, which both readings agree
   on. Pin the **VFS file** as the authoritative captured output.

2. **VFS path key construction (double slash).** Filenames are stored **with**
   their leading `/` (e.g. `/dev/stdout`), and file content is read/written under
   the key `vfs/` ++ filename — which yields a key with a doubled slash
   (`vfs//dev/stdout`). The init VFS, however, is written as nested
   `vfs/dev/stdout` (single slash). Whether the substrate folds `vfs//dev/stdout`
   and `vfs/dev/stdout` to the same location is a substrate path-normalisation
   detail. A reimplementer MUST be internally consistent: the key a descriptor's
   filename produces for content read/write MUST address the same leaf that the
   `stdout` accessor and a `vfs/dev/stdout` resolution read. The safest reading is
   to normalise away the leading slash so `/dev/stdout` ⇄ `vfs/dev/stdout`.

3. **`path_open` non-standard ABI.** The reference consumes only the first **3**
   of the **9** standard `path_open` arguments (`fd`, `dirflags`/lookup-flag,
   `path_ptr`) and ignores `path_len`, `oflags`, the rights masks, `fdflags`, and
   `opened_fd_ptr`. It also returns the opened fd as a **second WASM return value**
   (`results = [0, N]`) instead of writing it to `opened_fd_ptr` and returning
   only errno. A guest compiled against the real WASI ABI would therefore not
   receive its fd correctly. This spec pins the reference behaviour; a
   reimplementer reproducing *this device* MUST return `[0, N]` and store the
   descriptor at `vfs/<path>`, but should be aware the reference `path_open` is
   not WASI-ABI-conformant and is effectively unused by real WASI guests (the
   exercised, load-bearing functions are `fd_write`, `fd_read`, and
   `clock_time_get`).

4. **`path_open` stores a descriptor at a VFS *file* path.** `path_open` writes
   the descriptor **record** (a map with `index`/`filename`/`offset`) to
   `vfs/<path>`, i.e. into the VFS namespace where file **content** (a binary)
   otherwise lives. A subsequent `fd_write`/`fd_read` for that fd, however,
   addresses `vfs/<filename>` for content. Whether a descriptor-map and a
   content-binary can coexist at the same VFS path is unclear; the reference does
   not appear to exercise `path_open` followed by I/O on the opened fd. A
   reimplementer SHOULD keep the descriptor in the **fd table** (keyed by the new
   fd number) for the I/O paths to work, while matching the reference's
   `results = [0, N]` return; the precise VFS-vs-fd-table placement of the new
   descriptor is under-pinned.

5. **fd numbering races.** `path_open` derives the new fd as
   `(#fds in table) + 1`. If descriptors are stored in the VFS (item 4) rather
   than the fd table, the fd-table count does not grow with each `path_open`, so
   repeated opens of distinct paths could collide on the same index. This is a
   consequence of items 3–4 and is not exercised by the reference; a reimplementer
   that stores new descriptors in the fd table avoids the collision.

6. **`func-sig` is informational.** The function type signature is passed in the
   request and logged but does not influence behaviour. A reimplementer MAY ignore
   it entirely.

7. **`clock_time_get` errno value.** The reference returns errno `1`. In WASI
   preview-1, `1` is `EPERM`-class (not the conventional `ENOSYS`/`28` for
   "unsupported"). The *important* property is that it is **non-zero** (the guest
   must treat the clock as failed and must not read an uninitialised time
   pointer); the exact non-zero value is not load-bearing for determinism, but a
   reimplementer SHOULD return exactly `1` to match the reference byte-for-byte.
