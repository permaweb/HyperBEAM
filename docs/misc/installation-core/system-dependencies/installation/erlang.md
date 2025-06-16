# **Installing Erlang/OTP**

HyperBEAM is built on Erlang/OTP, so you'll need to have Erlang installed on your system.

## Building Erlang from Source

For the best compatibility, we recommend building Erlang from source:

```bash
git clone https://github.com/erlang/otp.git && \
	cd otp && git checkout maint-27 && \
	./configure --without-wx --without-debugger --without-observer --without-et && \
	make -j$(nproc) && \
	sudo make install && \
	cd .. && rm -rf otp
```

This will:

1. Clone the Erlang/OTP repository
2. Checkout the maintenance branch for version 27
3. Configure the build to exclude GUI components (reducing dependencies)
4. Build Erlang using all available CPU cores
5. Install Erlang system-wide
6. Clean up the source directory

## Verify Installation

You can verify your Erlang installation with:

```bash
erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'
```

This should output `27`, indicating the OTP release version. 
