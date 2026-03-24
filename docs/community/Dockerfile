FROM erlang:27 AS builder

RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    pkg-config \
    libssl-dev \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable
ENV PATH="/root/.cargo/bin:${PATH}"

ARG HB_BRANCH=neo/edge
ARG HB_REPO=https://github.com/permaweb/HyperBEAM.git

WORKDIR /app
RUN git clone --depth 1 --branch ${HB_BRANCH} ${HB_REPO} .

RUN touch config.flat && rebar3 release

FROM ubuntu:24.04

RUN apt-get update && apt-get install -y \
    libssl3 \
    libncurses6 \
    ca-certificates \
    curl \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /app/_build/default/rel/hb /app/hb
COPY entrypoint.sh /app/entrypoint.sh
RUN chmod +x /app/entrypoint.sh

WORKDIR /app

EXPOSE 8001

ENTRYPOINT ["/app/entrypoint.sh"]
