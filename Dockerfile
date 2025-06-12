FROM ubuntu:22.04 AS builder

RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    pkg-config \
    ncurses-dev \
    libssl-dev \
    sudo \
    curl \
    ca-certificates

RUN git clone https://github.com/erlang/otp.git && \
    cd otp && \
    git checkout maint-27 && \
    ./configure && \
    make -j16 && \
    sudo make install

RUN git clone https://github.com/erlang/rebar3.git && \
    cd rebar3 && \
    ./bootstrap && \
    sudo mv rebar3 /usr/local/bin/

# install node 22 (used by genesis_wasm profile)
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - && \
    apt-get install -y nodejs && \
    node --version

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /opt

COPY . .

# compile the project with provided profiles
RUN rebar3 clean && rebar3 get-deps && rebar3 as genesis_wasm release

FROM ubuntu:22.04 AS runner

WORKDIR /opt

# Install Node 22 dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    curl \
    gnupg

# node 22 is still needed for genesis_wasm profile
RUN curl -fsSL https://deb.nodesource.com/setup_22.x | bash - && \
    apt-get install -y nodejs && \
    node --version

# copy the build artifacts from the builder stage
COPY --from=builder /opt/_build/ /opt/_build/

# bin bash here to start the container
ENTRYPOINT ["/opt/_build/genesis_wasm/rel/hb/bin/hb"]
