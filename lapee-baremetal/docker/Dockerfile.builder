# LapEE Buildroot builder — Linux/amd64 under Rosetta on Apple Silicon.
#
# Runs Buildroot 2024.02 LTS with the prerequisites for a minimal
# measured-boot appliance. Used as an ephemeral container from the macOS
# host; the lapee-baremetal/ tree is bind-mounted at /lapee.
FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive

RUN apt-get update -qq && apt-get install -y -qq --no-install-recommends \
        bc \
        bison \
        build-essential \
        ca-certificates \
        cmake \
        cpio \
        curl \
        file \
        flex \
        g++ \
        gawk \
        git \
        gzip \
        libelf-dev \
        libncurses-dev \
        libssl-dev \
        libudev-dev \
        pkg-config \
        python3 \
        python3-pip \
        rsync \
        sed \
        unzip \
        wget \
        xz-utils \
        zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

# Buildroot needs a non-root user for some package builds (Erlang in particular).
RUN useradd -m -s /bin/bash builder
USER builder
WORKDIR /lapee
CMD ["/bin/bash"]
