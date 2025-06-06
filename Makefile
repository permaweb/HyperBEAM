.PHONY: all compile debug debug-clean clean setup-genesis-wasm port_libs port_libs-clean wamrc aot_files

all: compile

compile:
	rebar3 compile

GENESIS_WASM_BRANCH = tillathehun0/cu-experimental
GENESIS_WASM_REPO = https://github.com/permaweb/ao.git
GENESIS_WASM_SERVER_DIR = _build/genesis-wasm-server

PROJ_DIR = $(shell pwd)
BUILD_DIR = $(PROJ_DIR)/_build
ifeq ($(shell uname -s),Darwin)
    SHARED_LIB_EXT = dylib
else
    SHARED_LIB_EXT = so
endif

PORT_LIBS_DIR = $(BUILD_DIR)/port_libs
BEAMR_LIB = $(PORT_LIBS_DIR)/hb_beamr/libhb_beamr_lib.a
BEAMRC_LIB = $(PORT_LIBS_DIR)/hb_beamrc/libhb_beamrc_lib.$(SHARED_LIB_EXT)
WAMRC_BUILD_DIR = $(PORT_LIBS_DIR)/hb_beamrc/wamr-src/wamr-compiler/build
WAMRC = $(WAMRC_BUILD_DIR)/wamrc

debug: debug-clean
	CFLAGS="-DHB_DEBUG=1" rebar3 compile

debug-clean:
	rm -rf priv

clean:
	rebar3 clean
	rm -f $(AOT_FILES)

$(GENESIS_WASM_SERVER_DIR):
	mkdir -p $(GENESIS_WASM_SERVER_DIR)
	@echo "Cloning genesis-wasm repository..." && \
        tmp_dir=$$(mktemp -d) && \
        git clone --depth=1 -b $(GENESIS_WASM_BRANCH) $(GENESIS_WASM_REPO) $$tmp_dir && \
        mkdir -p $(GENESIS_WASM_SERVER_DIR) && \
        cp -r $$tmp_dir/servers/cu/* $(GENESIS_WASM_SERVER_DIR) && \
        rm -rf $$tmp_dir && \
        echo "Extracted servers/genesis-wasm to $(GENESIS_WASM_SERVER_DIR)"

# Set up genesis-wasm@1.0 environment
setup-genesis-wasm: $(GENESIS_WASM_SERVER_DIR)
	@cp native/genesis-wasm/launch-monitored.sh $(GENESIS_WASM_SERVER_DIR) && \
	if ! command -v node > /dev/null; then \
		echo "Error: Node.js is not installed. Please install Node.js before continuing."; \
		echo "For Ubuntu/Debian, you can install it with:"; \
		echo "  curl -fsSL https://deb.nodesource.com/setup_22.x | sudo -E bash - && \\"; \
		echo "  apt-get install -y nodejs && \\"; \
		echo "  node -v && npm -v"; \
		exit 1; \
	fi
	@cd $(GENESIS_WASM_SERVER_DIR) && npm install > /dev/null 2>&1 && \
		echo "Installed genesis-wasm@1.0 server."

# TODO: Use the same `WAMR_SRC_DIR` for both `hb_beamr_lib` and `hb_beamrc_lib`

$(BEAMR_LIB):
	make \
        -C "native/hb_beamr/lib" \
        CMAKE_BUILD_DIR="$(PORT_LIBS_DIR)/hb_beamr" \
        build

$(BEAMRC_LIB):
	make \
        -C "native/hb_beamrc/lib" \
        CMAKE_BUILD_DIR="$(PORT_LIBS_DIR)/hb_beamrc" \
        build

port_libs: \
    $(BEAMR_LIB) \
    $(BEAMRC_LIB)

port_libs-clean:
	rm -rf $(PORT_LIBS_DIR)

$(WAMRC): compile
	mkdir -p $(WAMRC_BUILD_DIR) && \
	    cd $(WAMRC_BUILD_DIR) && \
	    cmake .. && \
	    make

wamrc: $(WAMRC)

# TODO: Find a way to keep these flags in sync with `hb_beamrc_lib`?

test/%.aot: test/%.wasm
	@echo "Compiling $< to $@ (ignoring errors)"
	-$(WAMRC) \
        --enable-nan-canonicalization \
        --nan-canonicalization-sign-bit=0 \
        --bounds-checks=1 \
        -o $@ $<

WASM_FILES := $(wildcard test/*.wasm)
AOT_FILES := $(patsubst test/%.wasm,test/%.aot,$(WASM_FILES))

aot_files: $(AOT_FILES)
	@echo "+++ AOT files generated (errors ignored) +++"
