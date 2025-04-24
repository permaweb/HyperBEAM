.PHONY: all compile debug debug-clean clean wamrc aot_files setup-genesis-wasm

all: compile aot_files

compile:
	rebar3 compile

WAMRC_INSTALL_PATH = _build/hb_wasm/wamr-src/wamr-compiler/build/wamrc

GENESIS_WASM_BRANCH = tillathehun0/cu-experimental
GENESIS_WASM_REPO = https://github.com/permaweb/ao.git
GENESIS_WASM_SERVER_DIR = _build/genesis-wasm-server

debug: debug-clean
	CFLAGS="-DHB_DEBUG=1" rebar3 compile

debug-clean:
	rm -rf priv

clean:
	rebar3 clean
	rm -f $(AOT_FILES)

WASM_FILES := $(wildcard test/*.wasm)
AOT_FILES := $(patsubst test/%.wasm,test/%.aot,$(WASM_FILES))

test/%.aot: test/%.wasm compile
	@echo "Compiling $< to $@ (ignoring errors)"
	-$(WAMRC_INSTALL_PATH) -o $@ $<

aot_files: $(AOT_FILES)
	@echo "+++ AOT files generated (errors ignored) +++"

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
