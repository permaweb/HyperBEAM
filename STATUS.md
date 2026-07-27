# feat/name-token review — STATUS

Branch: `claude/name-token-review-e7ea27`, from `feat/name-token` @ 824816e7c.
Baseline at task start: 5514 inserted lines vs `edge`; the two devices were
1473 + 2028 = 3501 of them.

## Where it stands

Tests, all green, run per device with a distinct `HB_PORT`:

| device                   | tests |
|--------------------------|-------|
| `dev_arweave_swap`       | 25    |
| `dev_name_token`         | 19    |
| `dev_arweave_scheduler`  | 12    |
| `dev_copycat`            | 31    |

Four of the name-token tests replay pinned mainnet transactions, so the
protocol changes are validated against the real weave, not fixtures.

Patch size vs `edge`: **5514 -> 4315 code lines, 21.7%**. (4485 including 170
lines of `STATUS.md` + `decisions/`, which are working log and should not merge.)

## Done

- Both devices rewritten twice: deposit removed then restored for fungible
  supplies, eager reservation clock replaced by lazy expiry, `note/3`,
  `recipient`, `field/4`, `state/4` deleted; `status` kept because the ao-site
  UI codes against it.
- Three permanent-wedge bugs found and fixed, each now pinned by a test: an
  unresolvable `swap-device` threw out of `hb_ao`'s guard; `total-supply` was
  coerced with `hb_util:int/1` outside any `maybe`; and removing `set/3` sends
  `hb_ao:set` into infinite recursion (so it stays, now proven load-bearing).
- `~arweave-scheduler@1.0` pins `path: compute` on every `all`-mode assignment,
  so both devices are `-export([compute/3, init/3, snapshot/3, normalize/3])`
  and inherit `~message@1.0` for everything else.
- copycat `mode=headers` integrated, measured, and cut down: the `/block2`
  stack was a pessimisation and its 265 lines are gone. See
  `decisions/all-mode-headers-sync.md` for the numbers.
- Deep-clean pass 1 applied (16 verified cuts of 50 proposed; 34 were refuted
  by the skeptic pass and correctly not applied).

## Not done — do not assume otherwise

1. **The 1/3 reduction target is not met: 21.7%, not 33%.** Reaching 3676 needs
   ~640 more lines. The honest remaining hotspots are `dev_arweave_scheduler`
   (~1272) and the two devices' test sections, which are now the majority of
   both files. Deep-clean pass 1's skeptics refuted every further test cut
   proposed against the two devices, so the next real lever is the scheduler,
   which has had one pass, not two.
2. **Deep-clean pass 2 was not run.** Only pass 1.
3. **No end-to-end browser evidence of the buy flow.** The ao-site
   `feat/name-token-sales` UI renders and lists real name tokens
   (`pn-live-1..3`), but their prices never resolve, because that needs a node
   carrying `~name-token@1.0` and I could not keep one alive: `rebar3 device
   local` exits when its shell loses a tty, and under `nohup`/`script` the node
   printed its banner and bound :8799 but never answered a request. Two
   screenshots of the Names page exist; neither shows a completed sale.
4. **No live-network revalidation with the `gg...` wallet.** No transactions
   were posted and no AR was spent.

## Next

- Bring a node up in a real terminal (`HB_PORT=8799 rebar3 device local
  --devices dev_name_token`), serve the UI copy from the scratchpad, and point
  it at the node:
  `http://localhost:5199/#/names?hb-node=http://localhost:8799`.
- Deep-clean pass 2 against `dev_arweave_scheduler`.
- Live suite: its driver was deleted as unverifiable; recover it with
  `git show 824816e7c:src/preloaded/process/dev_name_token.erl` if fresh
  fixtures are wanted.
