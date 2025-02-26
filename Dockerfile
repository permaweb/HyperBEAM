FROM --platform=linux/amd64 ubuntu:22.04

RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    pkg-config \
    ncurses-dev \
    libssl-dev \
    sudo \
    python3 \
    python3-pip \
    curl \ 
    ninja-build

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

RUN git clone https://github.com/rust-lang/rust.git && \
    cd rust && \
    ./configure && \
    make && \
    sudo make install

COPY . /app

RUN cd /app && \
    rebar3 compile

CMD ["/bin/bash"]
