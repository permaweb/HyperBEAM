# cmake/wamr.cmake

include(ExternalProject)

# =============================================================================
# --- WAMR Shared Variables --- 
# =============================================================================

# These variables need to be defined *before* including this file by the parent CMakeLists.txt
# - wamr_SOURCE_DIR
# - wamr_RUNTIME_BINARY_DIR
# - wamrc_BINARY_DIR
# - WAMR_INSTALL_DIR
# Note: LLVM directories removed, build handled by wamrc configure step

# Determine WAMR Platform/Arch
if(CMAKE_SYSTEM_NAME STREQUAL "Darwin")
    set(WAMR_BUILD_PLATFORM "darwin")
elseif(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(WAMR_BUILD_PLATFORM "linux")
elsif(CMAKE_SYSTEM_NAME STREQUAL "Windows")
    set(WAMR_BUILD_PLATFORM "windows")
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

# =============================================================================
# --- LLVM Dependency (Removed - Handled by wamrc configure step) ---
# =============================================================================

# =============================================================================
# --- WAMR Runtime Dependency (ExternalProject) ---
# =============================================================================

# WAMR Runtime CMake arguments
set(WAMR_RUNTIME_CMAKE_ARGS
    "-DCMAKE_INSTALL_PREFIX=${WAMR_INSTALL_DIR}"
    "-DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}"
    "-DWAMR_BUILD_TARGET=${WAMR_BUILD_TARGET}"
    "-DWAMR_BUILD_PLATFORM=${WAMR_BUILD_PLATFORM}"
    -DWAMR_BUILD_LIBC_WASI=0 # This can conflict with generic NativeSymbols
    -DWAMR_BUILD_LIBC_UVWASI=0 # This can conflict with generic NativeSymbols
    -DWAMR_BUILD_LIBC_BUILTIN=0 # This can conflict with generic NativeSymbols
    -DWAMR_BUILD_LIBC_EMCC=0 # This can conflict with generic NativeSymbols
    -DWAMR_BUILD_MEMORY64=1
    -DWAMR_DISABLE_HW_BOUND_CHECK=1
    -DWAMR_BUILD_EXCE_HANDLING=1
    -DWAMR_BUILD_SHARED_MEMORY=0
    -DWAMR_BUILD_AOT=1
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
    list(APPEND WAMR_RUNTIME_CMAKE_ARGS "-DWAMR_ENABLE_LOG=1")
endif()

ExternalProject_Add(wamr-runtime-proj
    GIT_REPOSITORY "https://github.com/permaweb/wasm-micro-runtime.git"
    GIT_TAG "2.2.0-nan-canonicalization-fp-boundary-simd-immediate"
    GIT_SHALLOW TRUE
    SOURCE_DIR ${wamr_SOURCE_DIR}
    BINARY_DIR ${wamr_RUNTIME_BINARY_DIR}
    # Configure from the root source directory, let flags control the build
    CONFIGURE_COMMAND ${CMAKE_COMMAND} -S <SOURCE_DIR> -B <BINARY_DIR> ${WAMR_RUNTIME_CMAKE_ARGS}
    BUILD_COMMAND $(MAKE) -C <BINARY_DIR>
    INSTALL_COMMAND $(MAKE) -C <BINARY_DIR> install
)

# =============================================================================
# --- WAMRC Dependency (ExternalProject) ---
# =============================================================================

# WAMRC CMake arguments (minimal, relying on defaults and LLVM script)
set(WAMRC_CMAKE_ARGS
    "-DCMAKE_INSTALL_PREFIX=${WAMR_INSTALL_DIR}" # Install wamrc to bin/
    "-DCMAKE_BUILD_TYPE=${CMAKE_BUILD_TYPE}"
    "-DWAMR_BUILD_TARGET=${WAMR_BUILD_TARGET}"
    "-DWAMR_BUILD_PLATFORM=${WAMR_BUILD_PLATFORM}"
    -DWAMR_BUILD_LIBC_WASI=0 # This can conflict with generic NativeSymbols
    -DWAMR_BUILD_LIBC_UVWASI=0 # This can conflict with generic NativeSymbols
    -DWAMR_BUILD_LIBC_BUILTIN=0 # This can conflict with generic NativeSymbols
    -DWAMR_BUILD_LIBC_EMCC=0 # This can conflict with generic NativeSymbols
    # Flags related to custom LLVM removed
)

# Construct the configure command to first run the LLVM build script
# NOTE: Assumes 'sh' and 'python3' (with pip) are available in the PATH
# Adjust script name/path if necessary (e.g., build_llvm.py on Windows)
set(LLVM_BUILD_SCRIPT_NAME "build_llvm.sh")
if(WAMR_BUILD_PLATFORM STREQUAL "windows")
  set(LLVM_BUILD_SCRIPT_NAME "build_llvm.py") # Use Python script directly on Windows
endif()

set(WAMRC_CONFIGURE_COMMAND
    cd <SOURCE_DIR>/wamr-compiler && ./${LLVM_BUILD_SCRIPT_NAME} && cd <BINARY_DIR> && ${CMAKE_COMMAND} -S <SOURCE_DIR>/wamr-compiler ${WAMRC_CMAKE_ARGS}
)

ExternalProject_Add(wamrc-compiler-proj
    GIT_REPOSITORY "https://github.com/permaweb/wasm-micro-runtime.git" # Same repo
    GIT_TAG "2.2.0-nan-canonicalization-fp-boundary-simd-immediate"       # Same tag
    GIT_SHALLOW TRUE
    SOURCE_DIR ${wamr_SOURCE_DIR}         # Re-use source dir from runtime build
    BINARY_DIR ${wamrc_BINARY_DIR}          # Use specific compiler build dir
    CONFIGURE_COMMAND ${WAMRC_CONFIGURE_COMMAND} # Custom command includes LLVM build script
    BUILD_COMMAND $(MAKE) -C <BINARY_DIR>
    INSTALL_COMMAND $(MAKE) -C <BINARY_DIR> install # Installs wamrc to ${WAMR_INSTALL_DIR}/bin
    DEPENDS wamr-runtime-proj # Depends only on WAMR source checkout
)

# =============================================================================
# --- Target Definitions (Example) ---
# =============================================================================

# Example: Create imported targets for easier linking
# You might need to adjust paths based on actual install layout

# Runtime library (adjust name based on actual built library)
# add_library(wamr_runtime STATIC IMPORTED)
# set_property(TARGET wamr_runtime PROPERTY
#     IMPORTED_LOCATION ${WAMR_INSTALL_DIR}/lib/libiwasm.a) # Example path
# add_dependencies(wamr_runtime wamr-runtime-proj)

# wamrc executable (useful if your build needs to call it)
# add_executable(wamrc_exe IMPORTED)
# set_property(TARGET wamrc_exe PROPERTY
#     IMPORTED_LOCATION ${WAMR_INSTALL_DIR}/bin/wamrc)
# add_dependencies(wamrc_exe wamrc-compiler-proj)
#
# You would then link against 'wamr_runtime' or use 'wamrc_exe'
# in custom commands/targets in your main CMakeLists.txt
