cmake_minimum_required(VERSION 3.14)
project(hb_beamr_wasm C)

# Include FetchContent for downloading WAMR
include(FetchContent)

# Define WAMR source repository
set(WAMR_GIT_URL "https://github.com/permaweb/wasm-micro-runtime.git" CACHE STRING "URL of WAMR repository")
set(WAMR_GIT_TAG "2.2.0-nan-canonicalization-fp-boundary-simd-immediate" CACHE STRING "Git tag/branch/commit of WAMR to use")

# Fetch WAMR source
FetchContent_Declare(
    wamr
    GIT_REPOSITORY ${WAMR_GIT_URL}
    GIT_TAG ${WAMR_GIT_TAG}
    GIT_SHALLOW TRUE
)
FetchContent_MakeAvailable(wamr)

# WAMR build options
set(WAMR_BUILD_EXECUTABLE 0)
set(SHARED_DIR ${wamr_SOURCE_DIR}/core/shared)
set(IWASM_DIR ${wamr_SOURCE_DIR}/core/iwasm)
set(WAMR_ROOT_DIR ${wamr_SOURCE_DIR})

# Tell CMake to use custom LLVM since the one in WAMR deps might not be built yet
set(WAMR_BUILD_WITH_CUSTOM_LLVM 1)

# Find LLVM
find_package(LLVM REQUIRED CONFIG)
message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

# Include WAMR cmake directly
include(cmake/wamr.cmake)

# =============================================================================
# --- hb_beamr_wasm Target ---
# =============================================================================

add_library(hb_beamr_wasm STATIC
    src/wasm.c
    # src/helpers.c
)

# Include directories for the library
target_include_directories(hb_beamr_wasm PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}/include # Local headers
    ${SHARED_DIR}/include               # WAMR shared headers
    ${IWASM_DIR}/include                # WAMR iwasm headers
)

# Link WASM lib against WAMR libs directly
target_link_libraries(hb_beamr_wasm PRIVATE
    vmlib
    aotclib
)

# =============================================================================
# --- Installation (for Rebar3 to find) ---
# =============================================================================
# Install the static library itself so rebar3 can find it in a predictable place
# relative to the CMake build directory.
install(TARGETS hb_beamr_wasm vmlib aotclib
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
    RUNTIME DESTINATION bin
)

# Install public headers needed by the Erlang driver
install(DIRECTORY include/ DESTINATION include
    FILES_MATCHING PATTERN "*.h"
)

# Install WAMR headers
install(DIRECTORY ${SHARED_DIR}/include/ 
    DESTINATION include/wamr
    FILES_MATCHING PATTERN "*.h"
)

install(DIRECTORY ${IWASM_DIR}/include/
    DESTINATION include/wamr
    FILES_MATCHING PATTERN "*.h"
)
