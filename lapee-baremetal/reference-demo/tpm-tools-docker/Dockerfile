# Linux tooling image for LapEE build.
#
# Contains:
#   - tpm2-tools + libtss2 for TPM operations against swtpm
#   - sbsigntool / efitools for UEFI Secure Boot key enrollment + UKI signing
#   - openssl for key material
#   - Tools for dm-verity, squashfs, veritysetup
#   - Minimal Linux userland
#
# Used as an ephemeral container from macOS host via `docker run -v ... --network=host`.
FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -qq && apt-get install -y -qq --no-install-recommends \
        tpm2-tools \
        libtss2-dev \
        sbsigntool \
        efitools \
        mokutil \
        openssl \
        cryptsetup-bin \
        squashfs-tools \
        dosfstools \
        mtools \
        cpio \
        gzip \
        xz-utils \
        curl \
        ca-certificates \
        jq \
        python3 \
        python3-cryptography \
        xxd \
        file \
        gcc \
        libc6-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /work
CMD ["/bin/bash"]
