######################################################################
# HyperBEAM Library GNU Makefile
#
# Usage: make [all|clean]
# Usage: make [secp256k1|secp256k1-clean]
# Usage: make [RandomX|RandomX-clean|RandomX-512|RandomX-4096|RandomX-squared]
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

# The three Arweave RandomX variants are the same fork built with
# different Argon2 cache, dataset and scratchpad sizes. The sizes are
# baked into the archive, so each variant needs its own build directory.
RANDOMX_CMAKE_OPTIONS_512 ?= \
	-DUSE_HIDDEN_VISIBILITY=ON \
	-DRANDOMX_ARGON_MEMORY=262144 \
	-DRANDOMX_DATASET_BASE_SIZE=536870912

RANDOMX_CMAKE_OPTIONS_4096 ?= \
	-DUSE_HIDDEN_VISIBILITY=ON \
	-DRANDOMX_ARGON_MEMORY=524288 \
	-DRANDOMX_DATASET_BASE_SIZE=4294967296

RANDOMX_CMAKE_OPTIONS_squared ?= \
	-DUSE_HIDDEN_VISIBILITY=ON \
	-DRANDOMX_ARGON_MEMORY=524288 \
	-DRANDOMX_DATASET_BASE_SIZE=2147483648 \
	-DRANDOMX_SCRATCHPAD_L1=2097152 \
	-DRANDOMX_SCRATCHPAD_L2=2097152 \
	-DRANDOMX_SCRATCHPAD_L3=2097152

RANDOMX_SOURCE_FILES := $(shell \
	if [ -d RandomX/src ]; then \
		find RandomX/src \
			-type f \( -name '*.c' -o -name '*.cpp' -o -name '*.h' -o -name '*.hpp' \) -print; \
	fi)

GIT_SUBMODULE_OPTIONS ?= --checkout --init

PHONY += all
all: secp256k1 RandomX

PHONY += help
help:
	@echo "Usage: make [all|clean]"
	@echo "Usage: make [secp256k1|secp256k1-clean]"
	@echo "Usage: make [RandomX|RandomX-clean|RandomX-512|RandomX-4096|RandomX-squared]"

PHONY += clean
clean: secp256k1-clean RandomX-clean

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
# RandomX targets
######################################################################
PHONY += RandomX
RandomX: RandomX/CMakeLists.txt RandomX-512 RandomX-4096 RandomX-squared

PHONY += RandomX-clean
RandomX-clean: RandomX-512-clean RandomX-4096-clean RandomX-squared-clean

RandomX/CMakeLists.txt:
	git submodule update $(GIT_SUBMODULE_OPTIONS) RandomX

define randomx_template
PHONY += RandomX-$(1)
RandomX-$(1): RandomX/build$(1)/librandomx$(1).a

PHONY += RandomX-$(1)-clean
RandomX-$(1)-clean:
	-rm -rf RandomX/build$(1)

RandomX/build$(1): RandomX/CMakeLists.txt
	mkdir -p $$@

# Order-only on the build directory: cmake writes into it, so a normal
# prerequisite would re-run the configure step on every build.
RandomX/build$(1)/Makefile: | RandomX/build$(1)
	cd RandomX/build$(1) \
		&& cmake $$(RANDOMX_CMAKE_OPTIONS_$(1)) ..

# `--target randomx' skips the tests/benchmark/codegen executables, which
# HyperBEAM does not link against.
RandomX/build$(1)/librandomx.a: RandomX/build$(1)/Makefile $$(RANDOMX_SOURCE_FILES)
	cd RandomX/build$(1) \
		&& cmake --build . --parallel --target randomx

RandomX/build$(1)/librandomx$(1).a: RandomX/build$(1)/librandomx.a
	cd RandomX/build$(1) \
		&& cp librandomx.a librandomx$(1).a
endef

# RANDOMX 512
$(eval $(call randomx_template,512))

# RANDOMX 4096
$(eval $(call randomx_template,4096))

# RANDOMX SQUARED
$(eval $(call randomx_template,squared))

.PHONY: $(PHONY)
