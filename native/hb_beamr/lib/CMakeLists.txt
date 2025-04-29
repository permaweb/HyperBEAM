cmake_minimum_required(VERSION 3.14)
project(hb_beamr_lib C)

# Dependencies
include(${CMAKE_CURRENT_LIST_DIR}/../../common/wamr/cmake/wamr-src.cmake)

# Fetch WAMR sources
set(WAMR_GIT_REPOSITORY "https://github.com/permaweb/wasm-micro-runtime.git" CACHE STRING "Git repository URL for WAMR")
set(WAMR_GIT_TAG "2.2.0-nan-canonicalization-fp-boundary-simd-immediate" CACHE STRING "Git tag or branch for WAMR")
set(WAMR_SRC_DIR "${CMAKE_BINARY_DIR}/wamr-src" CACHE PATH "Directory for WAMR sources")
fetch_wamr_source()

# Determine WAMR Platform based on CMake system name
if(CMAKE_SYSTEM_NAME MATCHES "Darwin")
  set(WAMR_BUILD_PLATFORM "darwin")
elseif(CMAKE_SYSTEM_NAME MATCHES "Linux")
  set(WAMR_BUILD_PLATFORM "linux")
elseif(CMAKE_SYSTEM_NAME MATCHES "Windows")
  set(WAMR_BUILD_PLATFORM "windows")
else()
  message(FATAL_ERROR "Unsupported WAMR Platform: ${CMAKE_SYSTEM_NAME}")
endif()

# Determine WAMR Target Architecture based on CMake system processor
if(CMAKE_SYSTEM_PROCESSOR MATCHES "^(arm64|aarch64|ARM64)$")
  set(WAMR_BUILD_TARGET "AARCH64")
elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^(x86_64|amd64|AMD64)$")
  set(WAMR_BUILD_TARGET "X86_64")
# Add more architectures as needed based on WAMR support and your targets
# elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^(arm|ARM)$")
#  set(WAMR_BUILD_TARGET "ARM")
# elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^(i[3-6]86|x86)$")
#  set(WAMR_BUILD_TARGET "X86_32")
else()
  message(WARNING "Unsupported WAMR Target Architecture: ${CMAKE_SYSTEM_PROCESSOR}. Assuming X86_64. Please verify.")
  set(WAMR_BUILD_TARGET "X86_64") # Default or fallback
endif()

message(STATUS "Configuring WAMR for Platform: ${WAMR_BUILD_PLATFORM}, Target: ${WAMR_BUILD_TARGET}")

# -- Begin WAMR build options --

# Run mode: We are only supporting AOT compilation currently.
set(WAMR_BUILD_AOT 1)
set(WAMR_BUILD_INTERP 0)
set(WAMR_BUILD_FAST_INTERP 0)
set(WAMR_BUILD_JIT 0)
set(WAMR_BUILD_FAST_JIT 0)

# AOT options
set(WAMR_BUILD_AOT_STACK_FRAME 1) # TODO: Justification?

# Debug options
# TODO: set conditionally?
set(WAMR_BUILD_DEBUG_AOT 1)
set(WAMR_BUILD_DUMP_CALL_STACK 1)
set(WAMR_BUILD_MEMORY_PROFILING 1)

# Features
set(WAMR_BUILD_MEMORY64 1) # Memory >4GB
set(WAMR_BUILD_SIMD 1) # SIMD for performance
set(WAMR_BUILD_TAIL_CALL 1) # TODO: Justification?

# Disable hardware-enforced bounds checking
# TODO: Justification? This is for determinism reasons?
set(WAMR_DISABLE_HW_BOUND_CHECK 1)

# Exception handling is not supported in AOT mode
set(WAMR_BUILD_EXCE_HANDLING 0)

# Disable all libc libraries, as they can conflict with dev_wasi
set(WAMR_BUILD_LIBC_BUILTIN 0)
set(WAMR_BUILD_LIBC_WASI 0)
set(WAMR_BUILD_LIBC_UVWASI 0)

# Explicitly disable some features that are unused or hazardous
set(WAMR_BUILD_SHARED_MEMORY 0)
set(WAMR_BUILD_BULK_MEMORY 0)
set(WAMR_BUILD_LIB_WASI_THREADS 0)

# -- End WAMR build options --

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
# target_link_libraries(hb_beamr_lib PUBLIC vmlib) # Original line

# Link against vmlib, forcing export of its symbols into the shared library
if(APPLE)
  # On macOS, -all_load forces loading of all members of static archives
  target_link_libraries(hb_beamr_lib PUBLIC "-Wl,-all_load" vmlib)
elseif(UNIX AND NOT APPLE)
  # On other Unix-like systems (e.g., Linux), use --whole-archive
  target_link_libraries(hb_beamr_lib PUBLIC "-Wl,--whole-archive" vmlib "-Wl,--no-whole-archive")
else()
  # Fallback for other systems (e.g., Windows) or if flags cause issues
  target_link_libraries(hb_beamr_lib PUBLIC vmlib)
endif()