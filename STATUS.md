# feat/name-token review — STATUS

Branch: claude/name-token-review-e7ea27 (from feat/name-token @ 824816e7c)
Baseline at task start: dev_arweave_swap 1473 + dev_name_token 2028 = 3501 lines.

## Acceptance (immutable)
1. Total patch size >= 1/3 smaller than the 3501-line start.
2. Fresh-node `all`-mode sync is fast, using copycat `mode=headers`.
3. Two complete /deep-clean minimization passes.
4. Revalidated against the real network (gg... wallet) and the ~/src/ao-site UI.
5. Browser screenshots of the whole flow end to end.

## Done
- [x] Slimmed both devices, 55 -> 42 tests, all green incl. 4 mainnet replays (ddf3c9fd2)
- [x] Found+fixed 2 permanent-wedge bugs (swap-device throw; total-supply int/1)

## In flight
- [ ] Scheduler: all-mode assignments pin `path: compute`
- [ ] Devices: -export([compute/3]) only; drop info/router/set/keys/state
- [ ] Deposit restored for fungible supply (<100 lines)
- [ ] copycat mode=headers integration
- [ ] deep-clean x2
- [ ] live revalidation + ao-site screenshots
