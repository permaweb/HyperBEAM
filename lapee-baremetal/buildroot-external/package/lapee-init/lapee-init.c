/*
 * lapee-init — minimal PID 1 wrapper for a HyperBEAM LapEE appliance.
 *
 * Responsibilities:
 *   - Mount the essential virtual filesystems (/proc, /sys, /dev, /run, /tmp).
 *   - Bring up the loopback interface.
 *   - Bring up eth0 via a simple DHCP-less static-or-nothing approach; the
 *     operator supplies network config through the signed UKI cmdline.
 *   - exec(2) the HyperBEAM release binary as a regular child process.
 *   - Reap zombies, forward SIGTERM/SIGINT to the child, shut down cleanly.
 *
 * This deliberately does NOT do: logging to disk, getty, cron, user sessions,
 * service management. HyperBEAM is the only userspace program.
 *
 * Inspired by nerves-project/erlinit but pared down for LapEE.
 */

#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <linux/if.h>
#include <linux/reboot.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mount.h>
#include <sys/reboot.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>

#define HYPERBEAM_BINARY "/usr/bin/hyperbeam"

static pid_t hb_pid = 0;

static void die(const char *fmt, ...) __attribute__((noreturn));
static void die(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fputc('\n', stderr);
    /* A PID-1 exit triggers kernel panic. Sleep forever so logs are visible. */
    sync();
    while (1) pause();
}

static void info(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    fputs("[lapee-init] ", stderr);
    vfprintf(stderr, fmt, ap);
    va_end(ap);
    fputc('\n', stderr);
}

/* mount() with diagnostic; do not fail hard on EBUSY (already mounted). */
static int try_mount(const char *src, const char *dst, const char *type,
                     unsigned long flags, const char *data) {
    if (mount(src, dst, type, flags, data) == 0) return 0;
    if (errno == EBUSY) return 0;
    info("mount %s -> %s (%s) failed: %s", src, dst, type, strerror(errno));
    return -1;
}

static void mount_essentials(void) {
    mkdir("/proc", 0555);
    mkdir("/sys", 0555);
    mkdir("/dev", 0755);
    mkdir("/run", 0755);
    mkdir("/tmp", 01777);

    try_mount("proc", "/proc", "proc", MS_NOSUID | MS_NOEXEC | MS_NODEV, NULL);
    try_mount("sysfs", "/sys", "sysfs", MS_NOSUID | MS_NOEXEC | MS_NODEV, NULL);
    try_mount("devtmpfs", "/dev", "devtmpfs", MS_NOSUID, "mode=0755");
    try_mount("tmpfs", "/run", "tmpfs", MS_NOSUID | MS_NODEV, "mode=0755");
    try_mount("tmpfs", "/tmp", "tmpfs", MS_NOSUID | MS_NODEV, "mode=1777");

    mkdir("/dev/pts", 0755);
    try_mount("devpts", "/dev/pts", "devpts", MS_NOSUID | MS_NOEXEC, "gid=5,mode=620");
}

/* Bring up the loopback interface via ioctl, no external `ip` needed. */
static void bring_up_loopback(void) {
    int s = socket(AF_INET, SOCK_DGRAM, 0);
    if (s < 0) {
        info("socket() failed: %s", strerror(errno));
        return;
    }
    struct ifreq ifr = {0};
    strncpy(ifr.ifr_name, "lo", IFNAMSIZ);
    if (ioctl(s, SIOCGIFFLAGS, &ifr) == 0) {
        ifr.ifr_flags |= IFF_UP | IFF_RUNNING;
        if (ioctl(s, SIOCSIFFLAGS, &ifr) < 0)
            info("SIOCSIFFLAGS lo: %s", strerror(errno));
    }
    close(s);
}

/* Forward SIGTERM/SIGINT to HyperBEAM, let it shut down cleanly. */
static void forward_signal(int sig) {
    if (hb_pid > 0) kill(hb_pid, sig);
}

static void install_signal_handlers(void) {
    struct sigaction sa = {0};
    sa.sa_handler = forward_signal;
    sigaction(SIGTERM, &sa, NULL);
    sigaction(SIGINT, &sa, NULL);
    /* SIGCHLD handled via explicit waitpid loop below. */
}

/* Main PID-1 loop: reap children, exit when HyperBEAM exits. */
static int reap_loop(void) {
    for (;;) {
        int status;
        pid_t pid = waitpid(-1, &status, 0);
        if (pid < 0) {
            if (errno == EINTR) continue;
            if (errno == ECHILD) {
                info("all children exited; halting");
                return 0;
            }
            info("waitpid: %s", strerror(errno));
            return -1;
        }
        if (pid == hb_pid) {
            info("hyperbeam exited (status=%d); shutting down", status);
            return 0;
        }
        /* Reaped a grandchild, nothing to do. */
    }
}

int main(int argc, char *argv[]) {
    (void)argc; (void)argv;

    if (getpid() != 1) {
        info("lapee-init must run as PID 1; current pid=%d", getpid());
        return 1;
    }

    info("LapEE init starting (kernel=%s)", "unknown");

    mount_essentials();
    bring_up_loopback();
    install_signal_handlers();

    /* If HyperBEAM is present, exec it; otherwise fall back to /bin/sh so
     * the image remains bootable + diagnosable during M1 bring-up.
     * The overlay from lapee-baremetal/scripts/... grafts HyperBEAM in later. */
    const char *child_cmd = HYPERBEAM_BINARY;
    if (access(HYPERBEAM_BINARY, X_OK) != 0) {
        info("%s not present; falling back to /bin/sh", HYPERBEAM_BINARY);
        child_cmd = "/bin/sh";
    } else {
        info("launching hyperbeam");
    }

    hb_pid = fork();
    if (hb_pid < 0) die("fork failed: %s", strerror(errno));
    if (hb_pid == 0) {
        /* Child: exec the selected binary. For /bin/sh we want an interactive
         * shell wired to the serial console (stdin/stdout/stderr are already
         * inherited from PID 1 — the kernel set them up against /dev/console). */
        if (child_cmd == HYPERBEAM_BINARY) {
            execl(HYPERBEAM_BINARY, HYPERBEAM_BINARY, (char *)NULL);
        } else {
            execl("/bin/sh", "/bin/sh", "-i", (char *)NULL);
        }
        die("exec %s failed: %s", child_cmd, strerror(errno));
    }

    int rc = reap_loop();

    /* If HyperBEAM exited, power off (VM) or halt (bare metal). */
    sync();
    sleep(1);
    reboot(RB_POWER_OFF);
    return rc;
}
