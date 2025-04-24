# LLVM setup module for WAMR
# This module uses the build_llvm.py script from WAMR to set up LLVM
# Requires WAMR repository to be already cloned (depends on wamr-src.cmake)
#
# Usage example:
# ```cmake
# # First include wamr-src to ensure WAMR is available
# include(cmake/wamr-src.cmake)
# fetch_wamr_source()
#
# # Then set up LLVM
# include(cmake/llvm-src.cmake)
# setup_llvm_for_wamr(${WAMR_SRC_DIR})
# run_build_llvm(${WAMR_SRC_DIR})
# ```

cmake_minimum_required(VERSION 3.14)
include(CMakeParseArguments)

# Function to set up LLVM for WAMR
function(setup_llvm_for_wamr WAMR_SRC_DIR)
  cmake_parse_arguments(
    LLVM
    ""
    "LLVM_SRC_DIR"
    ""
    ${ARGN}
  )
  
  # Make sure WAMR_SRC_DIR exists
  if(NOT EXISTS ${WAMR_SRC_DIR})
    message(FATAL_ERROR "WAMR source directory does not exist: ${WAMR_SRC_DIR}. Run fetch_wamr_source() first.")
  endif()
  
  # Set LLVM directory within WAMR - this is where build_llvm.py will clone and build LLVM
  set(LLVM_WAMR_DIR "${WAMR_SRC_DIR}/core/deps/llvm")
  
  # Make sure the deps directory exists
  file(MAKE_DIRECTORY ${WAMR_SRC_DIR}/core/deps)
  
  # Find the build_llvm.py script
  set(BUILD_LLVM_SCRIPT "${WAMR_SRC_DIR}/build-scripts/build_llvm.py")
  if(NOT EXISTS ${BUILD_LLVM_SCRIPT})
    message(FATAL_ERROR "build_llvm.py script not found at: ${BUILD_LLVM_SCRIPT}")
  endif()
  
  # Add custom target to build LLVM
  add_custom_target(
    build_llvm
    COMMAND ${CMAKE_COMMAND} -E echo "Building LLVM for WAMR..."
    COMMAND ${Python3_EXECUTABLE} ${BUILD_LLVM_SCRIPT} --platform ${CMAKE_SYSTEM_NAME}
    WORKING_DIRECTORY ${WAMR_SRC_DIR}
    COMMENT "Building LLVM (this may take a while)..."
  )
  
  # Set LLVM_SRC_ROOT for other scripts to use
  set(LLVM_SRC_ROOT ${LLVM_WAMR_DIR} PARENT_SCOPE)
  
  # Store variables for use by run_build_llvm
  set(BUILD_LLVM_SCRIPT ${BUILD_LLVM_SCRIPT} PARENT_SCOPE)
  
  # Display information
  message(STATUS "LLVM will be built using: ${BUILD_LLVM_SCRIPT}")
  message(STATUS "LLVM will be installed at: ${LLVM_WAMR_DIR}/build")
endfunction()

# Function to run build_llvm.py script
function(run_build_llvm WAMR_SRC_DIR)
  # Find Python 3 (required for build_llvm.py)
  find_package(Python3 REQUIRED COMPONENTS Interpreter)
  
  # Find the build_llvm.py script
  set(BUILD_LLVM_SCRIPT "${WAMR_SRC_DIR}/build-scripts/build_llvm.py")
  if(NOT EXISTS ${BUILD_LLVM_SCRIPT})
    message(FATAL_ERROR "build_llvm.py script not found at: ${BUILD_LLVM_SCRIPT}")
  endif()
  
  # Check if LLVM build directory already exists
  set(LLVM_WAMR_DIR "${WAMR_SRC_DIR}/core/deps/llvm")
  if(EXISTS "${LLVM_WAMR_DIR}/build")
    message(STATUS "LLVM already built at: ${LLVM_WAMR_DIR}/build")
    return()
  endif()
  
  # Important: The build_llvm.py script expects to be called from the WAMR source directory
  # and it will clone into core/deps/llvm there. We don't need to create symlinks.
  
  # Determine platform for the script
  if(CMAKE_HOST_SYSTEM_NAME STREQUAL "Windows")
    set(PLATFORM_ARG "windows")
  elseif(CMAKE_HOST_SYSTEM_NAME STREQUAL "Darwin")
    set(PLATFORM_ARG "darwin")
  elseif(CMAKE_HOST_SYSTEM_NAME STREQUAL "Linux")
    set(PLATFORM_ARG "linux")
  else()
    # Default to the lowercase of the system name
    string(TOLOWER ${CMAKE_HOST_SYSTEM_NAME} PLATFORM_ARG)
  endif()
  
  # Map system processor to LLVM architecture
  if(CMAKE_SYSTEM_PROCESSOR MATCHES "^(x86_64|AMD64|amd64)$")
    set(ARCH_ARG "X86")
  elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^(aarch64|ARM64|arm64)$")
    set(ARCH_ARG "AArch64")
  elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^(arm).*$")
    set(ARCH_ARG "ARM")
  elseif(CMAKE_SYSTEM_PROCESSOR MATCHES "^(riscv64)$")
    set(ARCH_ARG "RISCV")
  else()
    set(ARCH_ARG "X86")  # Default to X86 for unknown processors
  endif()
  
  message(STATUS "Running build_llvm.py from ${WAMR_SRC_DIR}...")
  message(STATUS "Platform: ${PLATFORM_ARG}, Architecture: ${ARCH_ARG}")
  
  # Run the build_llvm.py script WITHOUT capturing output, so it streams to the console
  if(CMAKE_HOST_WIN32)
    # On Windows
    execute_process(
      COMMAND cmd /c "${Python3_EXECUTABLE} ${BUILD_LLVM_SCRIPT} --platform ${PLATFORM_ARG} --arch ${ARCH_ARG} 2>&1"
      WORKING_DIRECTORY ${WAMR_SRC_DIR}
      RESULT_VARIABLE RESULT
    )
  else()
    # On Unix-like systems
    execute_process(
      COMMAND ${CMAKE_COMMAND} -E env "PYTHONUNBUFFERED=1" ${Python3_EXECUTABLE} ${BUILD_LLVM_SCRIPT} --platform ${PLATFORM_ARG} --arch ${ARCH_ARG}
      WORKING_DIRECTORY ${WAMR_SRC_DIR}
      RESULT_VARIABLE RESULT
    )
  endif()
  
  # Check if the build was successful
  if(NOT RESULT EQUAL 0)
    message(FATAL_ERROR "Failed to build LLVM. See output above for details.")
  endif()
  
  # Check if build directory was created
  if(NOT EXISTS "${LLVM_WAMR_DIR}/build")
    message(FATAL_ERROR "build_llvm.py completed but ${LLVM_WAMR_DIR}/build was not created")
  endif()
  
  message(STATUS "LLVM built successfully: ${LLVM_WAMR_DIR}/build")
endfunction()
