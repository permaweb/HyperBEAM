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

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

WORKDIR /app

COPY . .

# compile the project
RUN rebar3 clean && rebar3 get-deps && rebar3 compile

# create the release
RUN rebar3 as prod release

CMD ["/bin/bash"]

FROM ubuntu:22.04 AS runner

RUN apt-get update && apt-get install -y \
    libssl-dev \
    ncurses-dev \
    ca-certificates && \
    apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=builder /app/_build/prod/rel/hb /app

RUN chmod +x /app/bin/hb

CMD ["/app/bin/hb", "foreground", "--eval", "hb:start_mainnet(#{})"]
