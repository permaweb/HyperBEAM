# Simple Makefile to drive the CMake build for hb_beamr_wasm

CMAKE_BUILD_DIR ?= _build
CMAKE_INSTALL_PREFIX ?= $(CMAKE_BUILD_DIR)/install

.PHONY: all configure build install clean clean-local

all: build install

configure:
	@echo "+++ Configuring hb_beamr_wasm +++"
	cmake -S . -B $(CMAKE_BUILD_DIR) -DCMAKE_INSTALL_PREFIX=$(CMAKE_INSTALL_PREFIX)

# Build depends on configure implicitly via dependency or workflow
build: configure
	@echo "+++ Building hb_beamr_wasm +++"
	cmake --build $(CMAKE_BUILD_DIR)

# Install depends on build
install: build
	@echo "+++ Installing hb_beamr_wasm +++"
	cmake --install $(CMAKE_BUILD_DIR)

# Full clean: Removes the entire build directory, including dependencies
clean:
	@echo "+++ Cleaning hb_beamr_wasm (full) +++"
	rm -rf $(CMAKE_BUILD_DIR)

# Local clean: Removes only CMake generated files and local library artifacts,
# preserving downloaded/built dependencies (_deps).
clean-local:
	@echo "+++ Cleaning hb_beamr_wasm (local only) +++"
	-@rm -f $(CMAKE_BUILD_DIR)/CMakeCache.txt
	-@rm -rf $(CMAKE_BUILD_DIR)/CMakeFiles
	-@rm -f $(CMAKE_BUILD_DIR)/cmake_install.cmake
	-@rm -f $(CMAKE_BUILD_DIR)/Makefile
	-@rm -f $(CMAKE_BUILD_DIR)/lib/libhb_beamr_wasm.a
