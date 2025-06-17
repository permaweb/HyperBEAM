# **Installing Rebar3**

Rebar3 is the Erlang build tool used by HyperBEAM for compilation and dependency management.

## Building Rebar3 from Source

To install Rebar3:

```bash
git clone https://github.com/erlang/rebar3.git && cd rebar3 \
	./bootstrap && \
	sudo mv rebar3 /usr/local/bin/ && \
	cd .. && rm -rf rebar3
```

This will:

1. Clone the Rebar3 repository
2. Bootstrap Rebar3 (build it)
3. Move the executable to your system path
4. Clean up the source directory

## Verify Installation

You can verify your Rebar3 installation with:

```bash
rebar3 --version
```

This should display the version information for Rebar3.

Example output:
`rebar 3.24.0+build.5437.ref5495da14 on Erlang/OTP 27 Erts 15.2`. 