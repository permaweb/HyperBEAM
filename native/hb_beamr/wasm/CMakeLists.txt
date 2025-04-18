cmake_minimum_required(VERSION 3.14)
project(hb_beamr_wasm C)

# Define WAMR build/install directories
set(wamr_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/_deps/wamr-src)
set(wamr_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/_deps/wamr-build)
set(WAMR_INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/_deps/wamr-install)

# Include WAMR ExternalProject definition
include(cmake/wamr.cmake)

# =============================================================================
# --- hb_beamr_wasm Target ---
# =============================================================================

add_library(hb_beamr_wasm STATIC
    src/wasm.c
    # src/helpers.c
)

# Ensure WAMR is built and installed before this library
add_dependencies(hb_beamr_wasm wamr-proj)

# Include directories for the library (WAMR includes from install dir)
target_include_directories(hb_beamr_wasm PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/include # Local headers
    ${WAMR_INSTALL_DIR}/include          # Use installed WAMR headers
)

# Link WASM lib against WAMR lib
# NOTE: Since this is STATIC, linking happens when final SO is created by rebar3
# But specifying it helps CMake understand dependencies.
# We point to the installed library location.
target_link_libraries(hb_beamr_wasm PRIVATE
    ${WAMR_INSTALL_DIR}/lib/libvmlib.a
)

# =============================================================================
# --- Installation (for Rebar3 to find) ---
# =============================================================================
# Install the static library itself so rebar3 can find it in a predictable place
# relative to the CMake build directory.
install(TARGETS hb_beamr_wasm
    ARCHIVE DESTINATION lib
)

# Install public headers needed by the Erlang driver
install(DIRECTORY include/ DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)
