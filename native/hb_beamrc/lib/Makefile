CMAKE_BUILD_DIR ?= _build

.PHONY: all configure build clean test

all: build

configure:
	@echo "+++ Configuring hb_wasm +++"
	cmake -S . -B $(CMAKE_BUILD_DIR) $(CMAKE_ARGS)

# Build depends on configure implicitly via dependency or workflow
build: configure
	@echo "+++ Building hb_wasm +++"
	cmake --build $(CMAKE_BUILD_DIR)

# Full clean: Removes the entire build directory, including dependencies
clean:
	@echo "+++ Cleaning hb_wasm (full) +++"
	rm -rf $(CMAKE_BUILD_DIR)

# Run tests
test:
	@echo "Running tests..."
	@cmake --build $(CMAKE_BUILD_DIR) --target run_tests
