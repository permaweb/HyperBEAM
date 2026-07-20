#!/bin/sh
set -eu

PEBBLE_IMAGE='ghcr.io/letsencrypt/pebble@sha256:ddf230642b1a584f519f32e347de1b05a6e4c1f6c35c1863b33effeab5f78199'
PEBBLE_NAME="hb-tls-pebble-$$"
TEST_DIR=$(mktemp -d "${TMPDIR:-/tmp}/hb-tls-pebble.XXXXXX")

cleanup() {
	docker rm -f "$PEBBLE_NAME" >/dev/null 2>&1 || true
	rm -rf "$TEST_DIR"
}
trap cleanup EXIT INT TERM

docker run --detach --name "$PEBBLE_NAME" \
	--add-host host.docker.internal:host-gateway \
	--publish 127.0.0.1:14000:14000 \
	--publish 127.0.0.1:15000:15000 \
	--env PEBBLE_VA_NOSLEEP=1 \
	--env PEBBLE_WFE_NONCEREJECT=0 \
	"$PEBBLE_IMAGE" >/dev/null

docker cp "$PEBBLE_NAME:/test/certs/pebble.minica.pem" \
	"$TEST_DIR/pebble.minica.pem"

attempt=0
until curl --silent --show-error --fail \
	--cacert "$TEST_DIR/pebble.minica.pem" \
	https://localhost:14000/dir >/dev/null; do
	attempt=$((attempt + 1))
	[ "$attempt" -lt 60 ] || {
		echo "Pebble did not become ready." >&2; exit 1;
	}
	sleep 0.25
done

curl --silent --show-error --fail --cacert "$TEST_DIR/pebble.minica.pem" \
	https://localhost:15000/roots/0 \
	--output "$TEST_DIR/pebble.root.pem"

HB_PEBBLE_DIRECTORY_URL=https://localhost:14000/dir \
HB_PEBBLE_CA="$TEST_DIR/pebble.minica.pem" \
HB_PEBBLE_ISSUER_CA="$TEST_DIR/pebble.root.pem" \
	HB_PORT=0 rebar3 device test \
		--devices dev_tls \
		--with-core \
		--module hb_tls_examples \
		--timeout 300
