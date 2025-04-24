cmake_minimum_required(VERSION 3.14)
project(hb_beamr_lib C)

# Dependencies
include(${CMAKE_CURRENT_LIST_DIR}/../../common/wamr/cmake/wamr-src.cmake)

# Fetch WAMR sources
set(WAMR_GIT_REPOSITORY "https://github.com/permaweb/wasm-micro-runtime.git" CACHE STRING "Git repository URL for WAMR")
set(WAMR_GIT_TAG "2.2.0-nan-canonicalization-fp-boundary-simd-immediate" CACHE STRING "Git tag or branch for WAMR")
set(WAMR_SRC_DIR "${CMAKE_BINARY_DIR}/wamr-src" CACHE PATH "Directory for WAMR sources")
fetch_wamr_source()

# Configure WAMR build options
set(WAMR_BUILD_PLATFORM "darwin")
set(WAMR_BUILD_TARGET "AARCH64")
set(WAMR_BUILD_INTERP 1)
set(WAMR_BUILD_AOT 1)
set(WAMR_BUILD_JIT 0)
set(WAMR_BUILD_LIBC_BUILTIN 1)
set(WAMR_BUILD_LIBC_WASI 1)
set(WAMR_BUILD_SIMD 1)

# Use WAMR runtime lib directly
set(WAMR_ROOT_DIR ${WAMR_SRC_DIR})
include(${WAMR_ROOT_DIR}/build-scripts/runtime_lib.cmake)

# Build vmlib as a static library directly
add_library(vmlib STATIC ${WAMR_RUNTIME_LIB_SOURCE})
target_include_directories(vmlib PUBLIC ${WAMR_ROOT_DIR}/core/iwasm/include)
target_link_libraries(vmlib ${WAMR_RUNTIME_LIB_DEPS})

# =============================================================================
# --- hb_beamr_lib Target ---
# =============================================================================

add_library(hb_beamr_lib SHARED stub.c)

# Include directories for the library
target_include_directories(hb_beamr_lib PUBLIC
    ${WAMR_ROOT_DIR}/core/iwasm/include  # WAMR headers
)

# Link against vmlib and export it
target_link_libraries(hb_beamr_lib PUBLIC vmlib)