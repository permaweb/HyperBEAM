# cmake/wamr.cmake

include(ExternalProject)

# =============================================================================
# --- WAMR Dependency (ExternalProject) ---
# =============================================================================

# Define build directories relative to this project's build dir
# These variables need to be defined *before* including this file
# set(wamr_SOURCE_DIR ${CMAKE_CURRENT_BINARY_DIR}/_deps/wamr-src)
# set(wamr_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/_deps/wamr-build)
# set(WAMR_INSTALL_DIR ${CMAKE_CURRENT_BINARY_DIR}/_deps/wamr-install)

# Determine WAMR Platform/Arch
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set(WAMR_BUILD_PLATFORM "darwin")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(WAMR_BUILD_PLATFORM "linux")
else()
    message(FATAL_ERROR "Unsupported platform: ${CMAKE_SYSTEM_NAME}")
endif()
if(CMAKE_SYSTEM_PROCESSOR MATCHES "^(arm64|aarch64|ARM64|AARCH64)$")
    set(WAMR_BUILD_TARGET "AARCH64")
elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^(x86_64|amd64|AMD64)$")
    set(WAMR_BUILD_TARGET "X86_64")
else()
    message(FATAL_ERROR "Unsupported processor: ${CMAKE_SYSTEM_PROCESSOR}")
endif()

# Ensure CMAKE_BUILD_TYPE is set (defaults to Release)
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the build type" FORCE)
endif()

# WAMR CMake arguments (Uses WAMR_INSTALL_DIR set by caller)
set(WAMR_CMAKE_ARGS
    "-DCMAKE_INSTALL_PREFIX=${WAMR_INSTALL_DIR}" # Install WAMR libs/includes
    "-DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}"
    "-DWAMR_BUILD_TARGET=${WAMR_BUILD_TARGET}"
    "-DWAMR_BUILD_PLATFORM=${WAMR_BUILD_PLATFORM}"
    -DWAMR_BUILD_MEMORY64=1
    -DWAMR_DISABLE_HW_BOUND_CHECK=1
    -DWAMR_BUILD_EXCE_HANDLING=1
    -DWAMR_BUILD_SHARED_MEMORY=0
    -DWAMR_BUILD_AOT=1
    -DWAMR_BUILD_LIBC_WASI=0
    -DWAMR_BUILD_FAST_INTERP=0
    -DWAMR_BUILD_INTERP=1
    -DWAMR_BUILD_JIT=0
    -DWAMR_BUILD_FAST_JIT=0
    -DWAMR_BUILD_DEBUG_AOT=1
    -DWAMR_BUILD_TAIL_CALL=1
    -DWAMR_BUILD_AOT_STACK_FRAME=1
    -DWAMR_BUILD_MEMORY_PROFILING=1
    -DWAMR_BUILD_DUMP_CALL_STACK=1
)
if(CMAKE_BUILD_TYPE MATCHES "Debug")
    list(APPEND WAMR_CMAKE_ARGS "-DWAMR_ENABLE_LOG=1")
endif()

ExternalProject_Add(wamr-proj
    GIT_REPOSITORY "https://github.com/permaweb/wasm-micro-runtime.git"
    GIT_TAG "2.2.0-nan-canonicalization-fp-boundary-simd-immediate"
    GIT_SHALLOW TRUE
    SOURCE_DIR ${wamr_SOURCE_DIR}
    BINARY_DIR ${wamr_BINARY_DIR}
    CONFIGURE_COMMAND ${CMAKE_COMMAND} -S <SOURCE_DIR> -B <BINARY_DIR> ${WAMR_CMAKE_ARGS}
    BUILD_COMMAND $(MAKE) -C <BINARY_DIR> # Explicitly build in BINARY_DIR
    INSTALL_COMMAND $(MAKE) -C <BINARY_DIR> install # Install includes/libs
) 