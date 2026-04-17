# secp256k1 NIF Makefile for HyperBEAM
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

# Set default libs path for secp256k1 implementation
SECP256K1_LDLIBS = -L /usr/lib -L /usr/local/lib

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
	# on MacOS, some libs are also present in /opt/homebrew/lib
	SECP256K1_LDLIBS += -L /opt/homebrew/lib
else ifeq ($(UNAME_SYS), FreeBSD)
	CC ?= cc
	CFLAGS += -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS += -finline-functions -Wall
else ifeq ($(UNAME_SYS), Linux)
	CC ?= gcc
	CFLAGS += -std=c99 -finline-functions -Wall -Wmissing-prototypes
	CXXFLAGS += -finline-functions -Wall
endif

C_SRC_DIR = $(CURDIR)

# Paths to libsecp256k1
LIBSECP256K1_DIR = $(BASEDIR)/native/lib/secp256k1
LIBSECP256K1_STATIC = $(LIBSECP256K1_DIR)/build/lib/libsecp256k1.a

# Source files
SECP256K1_SOURCES = $(C_SRC_DIR)/secp256k1_nif.c $(BASEDIR)/native/hb_nif/hb_nif.c
SECP256K1_OBJECTS = $(C_SRC_DIR)/secp256k1_nif.o $(BASEDIR)/native/hb_nif/hb_nif.o

# Build up SECP256K1 flags (matching Arweave's pattern)
SECP256K1_CFLAGS += $(CFLAGS)
SECP256K1_LDLIBS += $(LDFLAGS)
CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I /usr/local/include -I $(C_SRC_DIR)
CXXFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -std=c++11
LDLIBS += -L $(ERL_INTERFACE_LIB_DIR) -L /usr/local/lib -lei

# Final secp256k1-specific flags
SECP256K1_CFLAGS += -fPIC -I $(ERTS_INCLUDE_DIR) -I $(ERL_INTERFACE_INCLUDE_DIR) -I /usr/local/include -I $(LIBSECP256K1_DIR)/include -I $(BASEDIR)/native/hb_nif -I $(C_SRC_DIR)
SECP256K1_LDLIBS += -L $(ERL_INTERFACE_LIB_DIR)

# Output
SECP256K1_OUTPUT ?= $(BASEDIR)/priv/secp256k1_arweave.so

# Verbosity.

c_verbose_0 = @echo " C     " $(?F);
c_verbose = $(c_verbose_$(V))

cpp_verbose_0 = @echo " CPP   " $(?F);
cpp_verbose = $(cpp_verbose_$(V))

link_verbose_0 = @echo " LD    " $(@F);
link_verbose = $(link_verbose_$(V))

COMPILE_C = $(c_verbose) $(CC) $(CFLAGS) $(CPPFLAGS) -c
COMPILE_CPP = $(cpp_verbose) $(CXX) $(CXXFLAGS) $(CPPFLAGS) -c

$(SECP256K1_OUTPUT): $(LIBSECP256K1_STATIC) $(SECP256K1_OBJECTS)
	@mkdir -p $(BASEDIR)/priv/
ifeq ($(UNAME_SYS), Darwin)
	$(link_verbose) $(CC) $(SECP256K1_OBJECTS) $(LIBSECP256K1_STATIC) -bundle -flat_namespace -undefined suppress $(SECP256K1_LDLIBS) -o $(SECP256K1_OUTPUT)
else
	$(link_verbose) $(CC) $(SECP256K1_OBJECTS) $(LIBSECP256K1_STATIC) -shared $(SECP256K1_LDLIBS) -o $(SECP256K1_OUTPUT)
endif

%secp256k1_nif.o: %secp256k1_nif.c
	$(c_verbose) $(CC) $(SECP256K1_CFLAGS) -c $(OUTPUT_OPTION) $<

%hb_nif.o: %hb_nif.c
	$(c_verbose) $(CC) $(SECP256K1_CFLAGS) -c $(OUTPUT_OPTION) $<

%.o: %.c
	$(COMPILE_C) $(OUTPUT_OPTION) $<

%.o: %.cc
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.C
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

%.o: %.cpp
	$(COMPILE_CPP) $(OUTPUT_OPTION) $<

all: $(SECP256K1_OUTPUT)

$(LIBSECP256K1_STATIC):
	$(MAKE) -C $(BASEDIR)/native/lib secp256k1

clean:
	@rm -f $(SECP256K1_OUTPUT) $(SECP256K1_OBJECTS)

.PHONY: all clean
