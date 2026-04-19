#!/bin/sh
# HyperBEAM launcher for LapEE. Sets environment and execs the Erlang release.
set -e

HB_ROOT=/usr/lib/hyperbeam
HB_DATA=/var/hyperbeam

mkdir -p "${HB_DATA}"
cd "${HB_DATA}"

export HB_CONFIG="${HB_DATA}/lapee-config.flat"
export HB_MODE=lapee

exec "${HB_ROOT}/bin/hb" foreground
