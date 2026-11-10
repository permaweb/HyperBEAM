######################################################################
# HyperBEAM Library GNU Makefile
#
# Usage: make [all|clean]
# Usage: make [secp256k1|secp256k1-clean]
# Usage: make [blst|blst-clean]
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

SECP256K1_SOURCE_FILES := $(shell \
	if [ -d secp256k1/include ] && [ -d secp256k1/src ]; then \
		find secp256k1/include secp256k1/src \
			-type f \( -name '*.c' -o -name '*.h' \) -print; \
	fi)

GIT_SUBMODULE_OPTIONS ?= --checkout --init

PHONY += all
all: secp256k1 blst

PHONY += help
help:
	@echo "Usage: make [all|clean]"
	@echo "Usage: make [secp256k1|secp256k1-clean]"
	@echo "Usage: make [blst|blst-clean]"

PHONY += clean
clean: secp256k1-clean blst-clean

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

secp256k1/build/lib/libsecp256k1.a: secp256k1/build/Makefile $(SECP256K1_SOURCE_FILES)
	cd secp256k1/build \
		&& cmake --build .

######################################################################
# blst targets
######################################################################
# `__BLST_PORTABLE__' picks the ADX code path at run time instead of from
# the build host's CPU, so a library built in one image runs on any x86_64
# node.
BLST_SOURCE_FILES := $(shell \
	if [ -d blst/src ] && [ -d blst/build ]; then \
		find blst/bindings blst/src blst/build \
			-type f \( -name '*.c' -o -name '*.h' -o -name '*.S' \) -print; \
	fi)

PHONY += blst
blst: blst/build.sh blst-build/libblst.a

PHONY += blst-clean
blst-clean:
	-rm -rf blst-build

blst/build.sh:
	git submodule update $(GIT_SUBMODULE_OPTIONS) blst

blst-build/libblst.a: blst/build.sh $(BLST_SOURCE_FILES)
	mkdir -p blst-build
	cd blst-build && sh ../blst/build.sh -D__BLST_PORTABLE__

.PHONY: $(PHONY)
