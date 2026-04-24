# LapEE guest initramfs

This directory holds the four artefacts that `build-initramfs-hb.sh`
stitches into the guest initramfs baked into the UKI:

| File              | Role                                              |
|-------------------|---------------------------------------------------|
| `init`            | PID 1 init. Mounts /proc etc, parses cmdline,     |
|                   | brings up networking, and either `exec`s HB in    |
|                   | foreground or (writeback mode) forks HB, waits    |
|                   | for `/info`, writes attestation back to ESP.      |
| `lapee-splash`    | Animated boot splash (see below).                 |
| `lapee-dhcp-hook` | udhcpc action script. First interface to lease    |
|                   | wins the default route and flips the splash into  |
|                   | QR-code mode.                                     |
| `logo.ascii`      | Centred figlet-style HyperBEAM art for the        |
|                   | static splash fallback.                           |

## The boot splash

`lapee-splash` is a retro demo-reel in busybox-awk. When init PID 1
launches, the splash starts as a long-lived daemon writing directly
to `/dev/console` (no `/dev/kmsg`; that channel is reserved for init
traces). What the operator sees:

1. A wireframe retro laptop silhouette projected in 3D space, yaw-
   rotating around its vertical axis. 6 fps, `sleep 0.16` between
   frames. CPU cost is a few percent -- awk is cheap and the
   framebuffer is 80-to-160 cells wide at most.
2. A status line on the penultimate row of the screen tells the
   operator what init is doing ("booting...", "waiting for
   network...", "waiting for HyperBEAM...", etc.).
3. Once HB is serving `/info` and a DHCP lease has landed, the
   laptop settles into a front-facing pose (yaw snaps to 0, tilt
   goes flat) and a scannable QR code appears on its projected
   screen panel, pointing at `http://<ip>:8734/`.

Visually, the "laptop" is a clamshell built from:

- An 8-vertex base cuboid (keyboard deck + underside).
- An 8-vertex lid, hinged at the back edge. It opens from
  `angle=0` (closed) to `angle=1.85 rad` (~106°) via an ease-toward-
  target animation. In QR phase the target drops to `1.70 rad`
  so the screen faces the camera more directly.
- A bezel inset rectangle inside the lid front face -- the region
  where the QR gets stamped.

The math is classic demo-scene: yaw * tilt * perspective-divide,
then Bresenham-like line rasterisation into a 2D cell grid.
Character aspect (cells are ~2:1 tall on an 8x16 framebuffer font)
is corrected with `ASPECT=2.0` multiplied into the x-axis.

### Phase state machine

State lives in `/run/lapee/splash.state`. One-line format:

    phase=<phase> [ip=<ip>] [msg=<free text>]

| Phase      | Footer message                                  |
|------------|-------------------------------------------------|
| `boot`     | booting...                                      |
| `net-none` | no network found -- plug in Ethernet            |
| `net-up`   | interfaces up; starting HyperBEAM...            |
| `hb-wait`  | waiting for HyperBEAM (attestation in flight)...|
| `qr`       | scan -> http://<ip>:8734/                       |
| `halted`   | HALTED: <msg>                                   |
| `exit`     | (daemon returns, restoring cursor)              |

Transitions are driven by:

- `init` at startup           -> `boot`
- `init` after NIC enumeration -> `net-up` or `net-none`
- `init` entering HB wait      -> `hb-wait`
- `lapee-dhcp-hook` on lease   -> `hb-wait` with `ip=<ip>`
  (also pre-renders the QR matrix to `/run/lapee/qr.txt`)
- `init` after `/info` responds -> `qr` with `ip=<ip>`
- `lapee-dhcp-hook` fork      -> non-writeback fallback:
  background-polls `/info` and flips to `qr` when HB answers

### Phase update API

    /usr/local/bin/lapee-splash set <phase> [key=val ...]

Writes a fresh state file atomically (via `mv` over a `.tmp.$$`).
If `ip=<ip>` is among the args, also writes `/run/lapee/qr.txt`
by shelling out to `qrencode -l L -m 1 -t ASCII`.

    /usr/local/bin/lapee-splash daemon

Long-lived animator. Reads state file every frame. Starts in
whatever phase the state file currently holds (default `boot`).
Exits cleanly on `phase=exit`.

    /usr/local/bin/lapee-splash "<arbitrary text>"

Back-compat with the pre-animation static-splash API. Prints a
centred logo + status line once and exits. Used by init before
the daemon is ready (e.g. TME enforcement halt path) and as the
fallback when `lapee.nosplash=1` is present on the kernel cmdline.

### Extending

**New phase.** Add a case to `compose_footer()` in `lapee-splash`
for the status-line text. Transition by writing the state via
`lapee-splash set <phase>`.

**New visual element.** Extend the vertex/edge tables in the
`BEGIN` block (`bv()` / `be()` for base, `lv()` / `le()` for lid).
Every edge is a pair of vertex indices into its table. Keep the
silhouette sparse -- at SCALE ~ 10 cells/unit the wireframe tangles
fast if you add more than ~20 edges per object.

**QR content.** `do_set()` in the shell wrapper calls `qrencode -l L
-m 1 -t ASCII` with `http://$ip:8734/`. Change the URL shape there
or adjust `-l` / `-m` to tune error-correction level / quiet-zone
size. The awk overlay reads the ASCII matrix from `/run/lapee/qr.txt`
and stamps each module at a fixed `2x1` console cell footprint.

**Disable entirely.** `lapee.nosplash=1` on the kernel cmdline
degrades to the legacy static splash + a no-repaint idle loop, so
boot logs scroll normally past the splash. Useful when debugging a
suspected splash-related hang.

### Size budget

| Component                      | initramfs delta |
|--------------------------------|-----------------|
| `lapee-splash` shell + awk     | ~12 KB          |
| `qrencode` binary              | ~40 KB          |
| `libqrencode.so.4`             | ~100 KB         |
| `libpng16.so.16` + `libz.so.1` | ~350 KB*        |

*`libz` was already in the tree for HB; only `libpng` is new bloat
per the QR path. Target was under 300 KB net; actual is ~170 KB new
(libpng + qrencode + libqrencode; libz shared with HB).
