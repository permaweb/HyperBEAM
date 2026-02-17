######################################################################
# HyperBEAM Library GNU Makefile
#
# Usage: make [all|clean]
# Usage: make [secp256k1|secp256k1-clean]
#
# Based on Arweave's lib/Makefile
######################################################################
SECP256K1_CMAKE_OPTIONS ?= \
	-DSECP256K1_DISABLE_SHARED=ON \
	-DSECP256K1_ENABLE_MODULE_RECOVERY=ON \
	-DBUILD_SHARED_LIBS=OFF \
	-DSECP256K1_BUILD_BENCHMARK=OFF \
	-DSECP256K1_BUILD_EXHAUSTIVE_TESTS=OFF \
	-DSECP256K1_BUILD_TESTS=OFF \
	-DSECP256K1_ENABLE_MODULE_MUSIG=OFF \
	-DSECP256K1_ENABLE_MODULE_EXTRAKEYS=OFF \
	-DSECP256K1_ENABLE_MODULE_ELLSWIFT=OFF \
	-DSECP256K1_ENABLE_MODULE_SCHNORRSIG=OFF \
	-DSECP256K1_APPEND_CFLAGS=-fPIC

GIT_SUBMODULE_OPTIONS ?= --checkout --init

PHONY += all
all: secp256k1

PHONY += help
help:
	@echo "Usage: make [all|clean]"
	@echo "Usage: make [secp256k1|secp256k1-clean]"

PHONY += clean
clean: secp256k1-clean

######################################################################
# secp256k1 targets
######################################################################
PHONY += secp256k1
secp256k1: secp256k1/CMakeLists.txt secp256k1/build/lib/libsecp256k1.a

PHONY += secp256k1-clean
secp256k1-clean:
	-rm -rf secp256k1/build

secp256k1/CMakeLists.txt:
	git submodule update $(GIT_SUBMODULE_OPTIONS) secp256k1

secp256k1/build: secp256k1/CMakeLists.txt
	mkdir $@

secp256k1/build/Makefile: secp256k1/build
	cd secp256k1/build \
		&& cmake $(SECP256K1_CMAKE_OPTIONS) ..

secp256k1/build/lib/libsecp256k1.a: secp256k1/build/Makefile
	cd secp256k1/build \
		&& cmake --build .

.PHONY: $(PHONY)
