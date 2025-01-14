# Stage 1: Build the application
FROM erlang:25-slim AS builder

# Install necessary build tools
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    git \
    make \
    gcc \
    g++ \
    libc-dev \
    wget \
    bash \
    cmake \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Set environment variables
ENV REBAR3_VERSION=3.24.0

# Download and install Rebar3 by cloning and bootstrapping from source
RUN git clone https://github.com/erlang/rebar3.git /usr/local/src/rebar3 && \
    cd /usr/local/src/rebar3 && \
    git checkout ${REBAR3_VERSION}

# Build Rebar3
RUN cd /usr/local/src/rebar3 && \
    ./bootstrap && \
    mv rebar3 /usr/local/bin/ && \
    chmod +x /usr/local/bin/rebar3

# Set environment variables for WAMR
ENV WAMR_DIR=/app/wamr

RUN mkdir -p $WAMR_DIR

# Set WAMR version
ENV WAMR_VERSION=WAMR-2.2.0

# Download and extract WAMR release
RUN wget https://github.com/bytecodealliance/wasm-micro-runtime/archive/refs/tags/${WAMR_VERSION}.tar.gz && \
    tar xzf ${WAMR_VERSION}.tar.gz && \
    mv wasm-micro-runtime-${WAMR_VERSION} $WAMR_DIR && \
    rm ${WAMR_VERSION}.tar.gz

# Set the working directory inside the container
WORKDIR /app

# Cache dependencies by copying rebar.config and rebar.lock first
COPY rebar.config rebar.lock ./

# Fetch and compile dependencies
RUN rebar3 compile

# Copy the rest of the application code
COPY . .

# Compile the application
RUN rebar3 release

# Stage 2: Create the runtime image
FROM erlang:25-slim AS runner

# ENV
ENV HB_PORT=8734

# Install necessary runtime dependencies
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    openssl \
    libncurses5 \
    bash \
    && rm -rf /var/lib/apt/lists/*

# Set environment variables
WORKDIR /opt/hb

# Copy the release from the build stage
COPY --from=builder /app/_build/default/rel/hb ./

# Expose the port your application listens on (adjust as needed)
EXPOSE ${HB_PORT}

# Set the entrypoint to run the application
ENTRYPOINT ["./bin/hb", "foreground"]
