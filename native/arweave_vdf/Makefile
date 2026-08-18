# Arweave VDF NIF Makefile for HyperBEAM
# Based on Arweave's c_src/Makefile

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/../..)

PROJECT ?= $(notdir $(BASEDIR))
PROJECT := $(strip $(PROJECT))

ifeq ($(MODE), debug)
	CFLAGS ?= -O0 -g
	CXXFLAGS ?= -O0 -g
else
	CFLAGS ?= -O3
	CXXFLAGS ?= -O3
endif

UNAME_SYS := $(shell uname -s)

ifeq ($(UNAME_SYS), Linux)
	# _mm_crc32_u32 support
	CFLAGS += -msse4.2
	CXXFLAGS += -msse4.2
endif

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval 'io:format("~ts/erts-~ts/include/", [code:root_dir(), erlang:system_info(version)]).' -s init stop)
ERL_INTERFACE_INCLUDE_DIR ?= $(shell erl -noshell -eval 'io:format("~ts", [code:lib_dir(erl_interface, include)]).' -s init stop)
ERL_INTERFACE_LIB_DIR ?= $(shell erl -noshell -eval 'io:format("~ts", [code:lib_dir(erl_interface, lib)]).' -s init stop)

# System type and C compiler/flags.

ifeq ($(UNAME_SYS), Darwin)
	OSX_CPU_ARCH ?= x86_64
	# nix systems may not have sysctl where uname -m will return the correct arch
	SYSCTL_EXISTS := $(shell which sysctl 2>/dev/null)
	ifneq ($(shell uname -m | egrep "arm64"),)
		OSX_CPU_ARCH = arm64
	else
		ifdef SYSCTL_EXISTS
			ifneq ($(shell sysctl -n machdep.cpu.brand_string | egrep "M(1|2)"),)
				OSX_CPU_ARCH = arm64
			endif
		endif
	endif
	CC ?= cc
	CFLAGS += -std=c99 -arch $(OSX_CPU_ARCH) -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS += -arch $(OSX_CPU_ARCH) -finline-functions -Wall
	LDFLAGS ?= -arch $(OSX_CPU_ARCH)
	LDFLAGS += -undefined suppress
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS += -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS += -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS += -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS += -finline-functions -Wall
endif

# SHA-256 comes from OpenSSL's `libcrypto', which is already a runtime
# dependency of every OTP release (the `crypto' application links it).
# Arweave statically links its own `openssl-sha-lite' fork instead; see
# VENDOR.md. The low-level `SHA256_*' entry points are deprecated in
# OpenSSL 3 but still exported, hence `-Wno-deprecated-declarations'.
OPENSSL_DIR ?= $(shell pkg-config --variable=prefix libcrypto 2>/dev/null)
ifneq ($(OPENSSL_DIR),)
	CFLAGS += -I $(OPENSSL_DIR)/include
	CXXFLAGS += -I $(OPENSSL_DIR)/include
	CRYPTO_LDLIBS = -L $(OPENSSL_DIR)/lib -lcrypto
else
	CRYPTO_LDLIBS = -lcrypto
endif
CFLAGS += -Wno-deprecated-declarations
CXXFLAGS += -Wno-deprecated-declarations

C_SRC_DIR = $(CURDIR)

CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I /usr/local/include -I $(C_SRC_DIR)
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -std=c++11
LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -L /usr/local/lib -lei

VDF_OUTPUT ?= $(BASEDIR)/priv/vdf_arweave.so

VDF_SOURCES = $(wildcard $(C_SRC_DIR)/*.c $(C_SRC_DIR)/vdf/*.c $(C_SRC_DIR)/vdf/*.cpp)
VDF_OBJECTS = $(addsuffix .o, $(basename $(VDF_SOURCES)))

# NOTE tabs here will cause build fail
ifeq ($(UNAME_SYS), Linux)
  $(C_SRC_DIR)/vdf/vdf_fused_x86.o: CXXFLAGS += -msha
endif
ifeq ($(UNAME_SYS), Darwin)
  $(C_SRC_DIR)/vdf/vdf_fused_arm.o: CXXFLAGS += -march=armv8-a+crypto
  $(C_SRC_DIR)/vdf/vdf_hiopt_arm.o: CXXFLAGS += -march=armv8-a+crypto
endif
ifeq ($(UNAME_SYS), Darwin)
	VDF_ARM_ASM_OBJ = $(C_SRC_DIR)/vdf/sha256-armv8.o
	VDF_OBJECTS += $(VDF_ARM_ASM_OBJ)
$(VDF_ARM_ASM_OBJ): $(C_SRC_DIR)/vdf/sha256-armv8.S
	@echo "Assembling ARM64 specific file: $<"
	clang -O3 -arch arm64 -c $(C_SRC_DIR)/vdf/sha256-armv8.S -o $(VDF_ARM_ASM_OBJ)
endif

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

ifeq ($(UNAME_SYS), Darwin)
	LINK_NIF = -bundle -flat_namespace -undefined suppress
else
	LINK_NIF = -shared
endif

all: $(VDF_OUTPUT)

$(VDF_OUTPUT): $(VDF_OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
	$(link_verbose) $(CXX) $(VDF_OBJECTS) $(LDFLAGS) $(LDLIBS) $(CRYPTO_LDLIBS) $(LINK_NIF) -o $(VDF_OUTPUT)

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

clean:
	@rm -f $(VDF_OUTPUT) $(VDF_OBJECTS)

.PHONY: all clean
