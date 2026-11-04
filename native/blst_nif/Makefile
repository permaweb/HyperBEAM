# blst NIF Makefile for HyperBEAM: BLS12-381 signature verification over
# supranational/blst (native/lib/blst, built by native/lib/Makefile).

CURDIR := $(shell pwd)
BASEDIR := $(abspath $(CURDIR)/../..)

UNAME_SYS := $(shell uname -s)

ERTS_INCLUDE_DIR ?= $(shell erl -noshell -eval 'io:format("~ts/erts-~ts/include/", [code:root_dir(), erlang:system_info(version)]).' -s init stop)

CFLAGS ?= -O2
CC ?= cc
CFLAGS += -std=c99 -fPIC -Wall -Wmissing-prototypes

C_SRC_DIR = $(CURDIR)

# Paths to libblst
LIBBLST_DIR = $(BASEDIR)/native/lib/blst
LIBBLST_STATIC = $(BASEDIR)/native/lib/blst-build/libblst.a

BLST_NIF_CFLAGS = $(CFLAGS) -I $(ERTS_INCLUDE_DIR) -I $(LIBBLST_DIR)/bindings

# Output
BLST_NIF_OUTPUT ?= $(BASEDIR)/priv/blst_nif.so

all: $(BLST_NIF_OUTPUT)

$(BLST_NIF_OUTPUT): $(LIBBLST_STATIC) $(C_SRC_DIR)/blst_nif.o
	@mkdir -p $(BASEDIR)/priv/
ifeq ($(UNAME_SYS), Darwin)
	$(CC) $(C_SRC_DIR)/blst_nif.o $(LIBBLST_STATIC) -bundle -flat_namespace -undefined suppress -o $@
else
	$(CC) $(C_SRC_DIR)/blst_nif.o $(LIBBLST_STATIC) -shared -o $@
endif

$(C_SRC_DIR)/blst_nif.o: $(C_SRC_DIR)/blst_nif.c $(LIBBLST_DIR)/bindings/blst.h
	$(CC) $(BLST_NIF_CFLAGS) -c -o $@ $<

$(LIBBLST_STATIC):
	$(MAKE) -C $(BASEDIR)/native/lib blst

clean:
	@rm -f $(BLST_NIF_OUTPUT) $(C_SRC_DIR)/blst_nif.o

.PHONY: all clean
