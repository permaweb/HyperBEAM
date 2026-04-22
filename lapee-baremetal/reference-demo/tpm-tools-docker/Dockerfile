# Linux tooling image for LapEE build.
#
# Contains:
#   - tpm2-tools + libtss2 for TPM operations against swtpm
#   - sbsigntool / efitools for UEFI Secure Boot key enrollment + UKI signing
#   - systemd-boot-efi (linuxx64.efi.stub) + systemd-ukify for UKI assembly
#   - openssl for key material
#   - Tools for dm-verity, squashfs, veritysetup
#   - gdisk / parted for GPT partitioning of USB-image output
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
        systemd-boot-efi \
        systemd-ukify \
        gdisk \
        parted \
        kmod \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /work
CMD ["/bin/bash"]
