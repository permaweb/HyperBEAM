cmake_minimum_required(VERSION 3.14)
project(hb_beamr_lib C)

# Option to enable building tests
option(BUILD_TESTING "Build the tests" OFF)

# Dependencies
include(${CMAKE_CURRENT_LIST_DIR}/../../common/wamr/cmake/wamr-src.cmake)

# Fetch WAMR sources
set(WAMR_GIT_REPOSITORY "https://github.com/permaweb/wasm-micro-runtime.git" CACHE STRING "Git repository URL for WAMR")
set(WAMR_GIT_TAG "2.2.0-nan-canonicalization-fp-boundary-simd-immediate" CACHE STRING "Git tag or branch for WAMR")
set(WAMR_SRC_DIR "${CMAKE_BINARY_DIR}/wamr-src" CACHE PATH "Directory for WAMR sources")
fetch_wamr_source()

# --- LLVM and WAMRC setup (only when testing) ---
if(BUILD_TESTING)
  # Include LLVM helper relative to this file
  include(${CMAKE_CURRENT_LIST_DIR}/../../common/wamr/cmake/wamr-llvm.cmake)

  # Ensure Python3 is found for LLVM build script
  find_package(Python3 REQUIRED COMPONENTS Interpreter)

  # Setup and build LLVM using WAMR script
  # This will place LLVM inside ${WAMR_SRC_DIR}/core/deps/llvm
  setup_llvm_for_wamr(${WAMR_SRC_DIR})
  run_build_llvm(${WAMR_SRC_DIR})

  # Define path to wamrc source and build directories within our main build dir
  set(WAMRC_SOURCE_DIR ${WAMR_SRC_DIR}/wamr-compiler)
  set(WAMRC_BUILD_DIR ${CMAKE_BINARY_DIR}/wamrc_build) # Build wamrc within our project's build dir
  set(WAMRC_EXECUTABLE ${WAMRC_BUILD_DIR}/wamrc)

  # Add a custom target to build the wamrc executable
  # This depends on LLVM being built first implicitly via run_build_llvm execution order
  add_custom_command(
    OUTPUT ${WAMRC_EXECUTABLE}
    COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "+++ Configuring wamrc +++"
    COMMAND ${CMAKE_COMMAND} -S ${WAMRC_SOURCE_DIR} -B ${WAMRC_BUILD_DIR}
            -DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}
            # Pass the LLVM location built by run_build_llvm
            -DCMAKE_PREFIX_PATH=${LLVM_SRC_ROOT}/build
    COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "+++ Building wamrc +++"
    COMMAND ${CMAKE_COMMAND} --build ${WAMRC_BUILD_DIR}
    DEPENDS ${BUILD_LLVM_SCRIPT} # Depends on the script existing
    COMMENT "Building wamrc executable..."
    VERBATIM
  )

  add_custom_target(build_wamrc_exe DEPENDS ${WAMRC_EXECUTABLE})

endif()
# --- End LLVM and WAMRC setup ---

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
set(WAMR_BUILD_INTERP 1)
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
set(WAMR_BUILD_BULK_MEMORY 1) # TODO: Check if this is okay?

# Disable hardware-enforced bounds checking
# TODO: Justification? This is due to running on the TEE?
set(WAMR_DISABLE_HW_BOUND_CHECK 1)

# Exception handling is not supported in AOT mode
set(WAMR_BUILD_EXCE_HANDLING 0)

# Disable all libc libraries, as they can conflict with dev_wasi
set(WAMR_BUILD_LIBC_BUILTIN 0)
set(WAMR_BUILD_LIBC_WASI 0)
set(WAMR_BUILD_LIBC_UVWASI 0)

# Explicitly disable some features that are unused or hazardous
set(WAMR_BUILD_SHARED_MEMORY 0)
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

add_library(hb_beamr_lib STATIC
    src/hb_beamr_lib.c
    src/hb_beamr_fsm.c
)

# -----------------------------------------------------------------------------
# Erlang (ei.h) detection – required for erlang term conversion helpers
# -----------------------------------------------------------------------------
find_path(ERLANG_EI_INCLUDE_DIR
          NAMES ei.h
          HINTS
            $ENV{ERLANG_ROOT}/usr/include
            /usr/lib/erlang/usr/include
            /usr/local/lib/erlang/usr/include
            /opt/homebrew/opt/erlang/lib/erlang/usr/include
            /usr/local/opt/erlang/lib/erlang/usr/include
            /opt/local/lib/erlang/usr/include
            /usr/include
            /usr/local/include
)

if(NOT ERLANG_EI_INCLUDE_DIR)
  message(FATAL_ERROR "Failed to locate ei.h. Please install Erlang/OTP or set ERLANG_EI_INCLUDE_DIR.")
else()
  message(STATUS "Found ei.h in: ${ERLANG_EI_INCLUDE_DIR}")
endif()

# Locate Erlang EI library (libei)
find_library(ERLANG_EI_LIBRARY
             NAMES ei
             HINTS
               $ENV{ERLANG_ROOT}/usr/lib
               /usr/lib/erlang/usr/lib
               /usr/local/lib/erlang/usr/lib
               /opt/homebrew/opt/erlang/lib/erlang/usr/lib
               /usr/local/opt/erlang/lib/erlang/usr/lib
               /opt/local/lib/erlang/usr/lib
               /usr/lib
               /usr/local/lib
)

if(NOT ERLANG_EI_LIBRARY)
  message(FATAL_ERROR "Failed to locate Erlang EI library (libei). Please install Erlang/OTP or set ERLANG_EI_LIBRARY.")
else()
  message(STATUS "Found libei: ${ERLANG_EI_LIBRARY}")
endif()

# Include directories for the hb_beamr_lib target
target_include_directories(hb_beamr_lib PUBLIC
    ${CMAKE_CURRENT_LIST_DIR}/include            # Public headers for hb_beamr_lib
    ${WAMR_ROOT_DIR}/core/iwasm/include          # WAMR headers
    ${ERLANG_EI_INCLUDE_DIR}                     # Erlang ei.h
)

# Link against vmlib and export it
target_link_libraries(hb_beamr_lib PUBLIC vmlib ${ERLANG_EI_LIBRARY})

# =============================================================================
# --- hb_beamr_capi_lib Target --- 
# =============================================================================

add_library(hb_beamr_capi_lib STATIC
    src/hb_beamr_capi_lib.c
)

# Include directories for the CAPI library
target_include_directories(hb_beamr_capi_lib PUBLIC
    ${CMAKE_CURRENT_LIST_DIR}/include                 # Public headers
    SYSTEM ${WAMR_ROOT_DIR}/core/iwasm/include        # WAMR headers
    ${ERLANG_EI_INCLUDE_DIR}                          # Erlang ei.h
)

# Link CAPI lib against vmlib (for WAMR specific calls if any, and transitively for runtime components)
# and export it so tests or other consumers can link to it.
target_link_libraries(hb_beamr_capi_lib PUBLIC vmlib ${ERLANG_EI_LIBRARY})

# Conditionally add the test subdirectory and enable testing
if(BUILD_TESTING)
  enable_testing()
  include(CTest)
  add_subdirectory(test)
endif()