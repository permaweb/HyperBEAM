# lapee_tpm — result

This is a real Erlang NIF that FFI-links directly into `libtss2-esys` and talks
to the running `swtpm` via the ESYS API. No Python, no subprocess calls to
`tpm2_*` CLI tools from Erlang. The only subprocess we would shell out to is
`tpm2_checkquote` for the optional independent verification step — and when
that tool isn't present we do the equivalent verification in Erlang using
OpenSSL (`public_key:verify/5` with RSA-PSS) plus a TPMS_ATTEST parser that
checks the nonce and PCR-digest binding.

## What works

- `rebar3 compile` builds `priv/lapee_tpm_nif.so` from `c_src/lapee_tpm_nif.c`
  and `c_src/tpm_helpers.c`, linking against the locally-built libtss2 at
  `work/tss2-prefix/lib`.
- The NIF loads into BEAM (OTP 27 / erts 15.0.1). `otool -L` shows real
  dependencies on `@rpath/libtss2-esys.0.dylib`, `libtss2-mu.0.dylib`,
  `libtss2-tctildr.0.dylib`, `libtss2-rc.0.dylib`, and OpenSSL 3. The rpath
  baked into the .so points to `work/tss2-prefix/lib`.
- All NIFs called against real `swtpm` on TCP 2321:
  - `lapee_tpm_nif:startup/0` — `Esys_Startup(TPM2_SU_CLEAR)`, idempotent.
  - `lapee_tpm_nif:pcr_read/1` — `Esys_PCR_Read` for the SHA-256 bank.
  - `lapee_tpm_nif:pcr_extend/2` — `Esys_PCR_Extend`, verified in the test by
    recomputing `H(old_pcr || H(data))` and asserting equality with the new
    PCR value.
  - `lapee_tpm_nif:create_primary_ek/0` — `Esys_CreatePrimary` under
    `ESYS_TR_RH_ENDORSEMENT` with the standard EK template (RSA 2048,
    SHA-256, restricted decrypt, policy = TPM2_PolicySecret(EH) digest).
    Returns `{handle, esys_tr, public_pem}`; PEM is produced by
    `EVP_PKEY_fromdata` + `PEM_write_bio_PUBKEY`.
  - `lapee_tpm_nif:create_signing_key/1` — `Esys_CreatePrimary` for a
    restricted RSA-2048 PSS signing key. Returns `{handle, esys_tr,
    public_pem, tpm2b_public}` where `tpm2b_public` is the marshalled
    TPM2B_PUBLIC for potential consumers of that wire form.
  - `lapee_tpm_nif:quote/3` — `Esys_Quote` with the requested PCR list. The
    result includes the raw quoted blob (TPMS_ATTEST), the signature bytes,
    the fully-marshalled TPMT_SIGNATURE (what `tpm2_checkquote` expects), and
    a map of the current PCR values.
  - `lapee_tpm_nif:sign/2` — `Esys_Hash` then `Esys_Sign` (RSA-PSS SHA-256)
    using the hash ticket so restricted signing keys are satisfied.
  - `lapee_tpm_nif:flush_context/1` — `Esys_FlushContext`.
  - `lapee_tpm_nif:set_tcti/1` — re-initialises TCTI + ESYS with a new
    TCTI string at runtime.

- `rebar3 eunit` runs the acceptance test `test/real_quote_test.erl` which
  does all six milestones (startup, read PCR 0, extend PCR 15 with math
  check, create EK, create AK under EK, quote [0,7,11,15] with a random
  20-byte nonce, save artefacts, independently verify).

## Last 30 lines of `rebar3 eunit`

```
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling lapee_tpm
===> Performing EUnit tests...
[t] PCR0 = 0000000000000000000000000000000000000000000000000000000000000000
[t] PCR15 extended correctly (H(old||H(data))).
[t] EK handle=0x80000000 pem_bytes=451
[t] AK handle=0x80000001 pem_bytes=451 tpm2b_bytes=282
[t] Quote quoted_bytes=133 sig_bytes=256
[t] Artifacts written to /tmp/lapee-nif-quote-1-1776587308077355
[t] tpm2_checkquote not found; using OpenSSL.
[t] OpenSSL RSA-PSS verify: true
[t] extraData bytes=20 nonce bytes=20
[t] pcrDigest in quote: 78A17215754ABA45558359A7DCA128CF7EFFBF3F64318A80AE9AC137F231EE11
[t] Expected pcrDigest : 78A17215754ABA45558359A7DCA128CF7EFFBF3F64318A80AE9AC137F231EE11
[t] Acceptance test PASSED.
.
Finished in 0.262 seconds
1 tests, 0 failures
```

## Independent verification

### What we actually verified

The test does three independent checks against the NIF output:

1. **RSA-PSS signature.** `public_key:verify(Quoted, sha256, Sig, PubKey,
   [{rsa_padding, rsa_pkcs1_pss_padding}, {rsa_pss_saltlen, -1},
   {rsa_mgf1_md, sha256}])`. This exercises OpenSSL (via `crypto.so`) using
   the AK PEM that the NIF returned, and the signature bytes the NIF
   extracted from `TPMT_SIGNATURE.signature.rsapss.sig`. A pass here proves
   (a) the AK public key we hand out really is the public half of the TPM's
   AK, and (b) the signature really was produced over the quoted data.

2. **Nonce binding.** We parse the TPMS_ATTEST blob in Erlang (magic `0xFF544347`,
   type `0x8018 = TPM_ST_ATTEST_QUOTE`, then `TPM2B_NAME qualifiedSigner`,
   then `TPM2B_DATA extraData`) and assert `extraData == nonce`. This proves
   the TPM signed over the nonce we supplied, ruling out replay.

3. **PCR-digest binding.** Continuing to parse TPMS_ATTEST past `clockInfo`
   and `firmwareVersion` into the `TPMS_QUOTE_INFO`, we extract the
   `pcrDigest` and assert it equals `SHA-256(concat(pcr_values in selection
   order))` using the PCR values the NIF returned. This proves the quote's
   PCR selection/values really match what we read separately.

### tpm2_checkquote

Running `tpm2_checkquote` was not possible on this macOS host because
`tpm2-tools` (and `tpm2-tss`) are not in Homebrew. I built `tpm2-tss` from
source (see below) but `tpm2-tools` 5.7 has several macOS portability
problems: missing `uchar.h`, missing `endian.h`, and additional `-Wl,-z,relro`
linker requirements. Getting it to compile would be another hour of patching
and is out of scope for this milestone since the OpenSSL-based verification
covers the same checks. The test does call `os:find_executable/1` — if
`tpm2_checkquote` ever does land on PATH, it will be used in preference to
the OpenSSL path and the signature file written is already the
tools-compatible marshalled form (`sig_marshalled.bin`).

## Building libtss2 on macOS (not available via Homebrew)

The task brief said "tss2 is installed on the Mac via Homebrew" — that isn't
true; only `libtpms` (the server-side library used by swtpm) ships via brew.
The client-side TSS2 (ESYS/SYS/MU/TCTILDR) is not in brew for macOS. I built
it from upstream source:

- Source: `tpm2-tss 4.1.3` from
  `https://github.com/tpm2-software/tpm2-tss/releases`.
- Prefix: `work/tss2-prefix` (inside the worktree so it travels with the
  project).
- Configured with `--disable-fapi --disable-policy --disable-doxygen-doc
  --disable-tcti-libtpms --disable-tcti-cmd --disable-tcti-pcap
  --disable-tcti-spi-* --disable-tcti-i2c-* --disable-tcti-device
  --with-tcti=mssim,swtpm`. Disabled because either (a) they require Linux
  headers (`sys/prctl.h`, `linux/ioctl.h`) or (b) they need libusb/libftdi
  that aren't useful here.
- Worked around configure's `addgroup`/`useradd` requirement with dummy shell
  scripts on PATH.
- Post-install, patched dylib install-names from the absolute `/opt/lapee/tpm2-tss/lib/…`
  (configure prefix artefact) to `@rpath/…` with `install_name_tool`, so the
  NIF's rpath to `work/tss2-prefix/lib` actually works.
- Symlinked `.so.0` / `.so` names to `.0.dylib` because `Tss2_TctiLdr`
  constructs candidate file names with the Linux `.so` suffix and `dlopen`s
  them.
- Pass the absolute path to `libtss2-tcti-swtpm.0.dylib` in the TCTI string so
  `Tss2_TctiLdr_Initialize` doesn't have to search for the plugin at all.
  (See `src/lapee_tpm_nif.erl`; override with env `LAPEE_TPM_TCTI`.)

The `rebar.config` hard-codes the tss2 prefix to
`work/tss2-prefix`. For Linux/baremetal this will come from the system
library path instead; keep in mind when porting.

## Known gaps / deferred items

These were explicitly called out as OK to defer in the brief:

- **Encrypted HMAC sessions.** All calls use `ESYS_TR_PASSWORD` with an
  empty auth. Fine against swtpm in a dev loop; must move to an HMAC/salted
  session before we trust this on real hardware.
- **Real EK→AK binding via credential activation.** The brief allows this to
  be deferred. The current `create_signing_key/1` takes the EK's ESYS_TR as
  an argument but ignores it and instead creates the AK as a fresh primary
  under `ESYS_TR_RH_OWNER`. That's enough to demonstrate a signing key and
  a quote that verifies against its own PEM, but it does not prove the AK
  lives on the same TPM as the EK. Completing this requires
  `Esys_MakeCredential` / `Esys_ActivateCredential` and a HMAC session,
  which is a follow-up milestone.
- **x86_64/Linux cross-build.** Explicitly out of scope per the brief. The
  Darwin-specific dylib path and install-name dance will need to be replaced
  by a `.so`/`rpath` setup for the Buildroot target.

## Files

- `src/lapee_tpm.erl` — thin Erlang API.
- `src/lapee_tpm_nif.erl` — NIF loader + stubs. Computes TCTI at load time;
  Darwin gets the absolute dylib path.
- `c_src/lapee_tpm_nif.c` — all ESYS calls and the NIF table.
- `c_src/tpm_helpers.{h,c}` — TSS2 error decoding, TPM2B marshalling,
  TPM2B_PUBLIC → PEM via OpenSSL 3 `EVP_PKEY_fromdata`.
- `test/real_quote_test.erl` — end-to-end eunit test with independent
  OpenSSL verification (and tpm2_checkquote support if present).
- `rebar.config` — NIF build via the `pc` plugin. Uses `post` hook so the
  Erlang compile runs before the C link.

## How to re-run

```
cd lapee-tpm
rebar3 eunit     # full acceptance test
rebar3 shell     # interactive
```

`swtpm` must be running (`scripts/swtpm.sh start`). Override the TCTI with
`LAPEE_TPM_TCTI=...` if you need a different swtpm/mssim endpoint.
