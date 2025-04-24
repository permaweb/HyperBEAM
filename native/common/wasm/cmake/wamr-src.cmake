# WAMR source fetching module
# Include this file in your CMakeLists.txt to get access to WAMR sources
#
# Usage example in a parent CMakeLists.txt:
#
# ```cmake
# # Include the WAMR source module
# include(cmake/wamr-src.cmake)
#
# # Configure and fetch WAMR sources (optional: set WAMR_SRC_DIR before this call)
# fetch_wamr_source()
#
# # Now you can use the WAMR_SRC_DIR variable in your build
# add_subdirectory(${WAMR_SRC_DIR}/core)
# include_directories(${WAMR_SRC_DIR}/core/iwasm/include)
# ```

cmake_minimum_required(VERSION 3.14)
include(FetchContent)

# Function to fetch WAMR source code
function(fetch_wamr_source)
  # Define configurable directory for WAMR sources with default value
  if(NOT DEFINED WAMR_SRC_DIR)
    set(WAMR_SRC_DIR "${CMAKE_BINARY_DIR}/wamr-src" CACHE PATH "Directory for WAMR sources")
  endif()

  # Allow customization of repository URL and tag
  if(NOT DEFINED WAMR_GIT_REPOSITORY)
    set(WAMR_GIT_REPOSITORY "https://github.com/bytecodealliance/wasm-micro-runtime.git" CACHE STRING "Git repository URL for WAMR")
  endif()
  
  if(NOT DEFINED WAMR_GIT_TAG)
    set(WAMR_GIT_TAG "main" CACHE STRING "Git tag or branch for WAMR")
  endif()

  message(STATUS "Using WAMR from: ${WAMR_GIT_REPOSITORY}#${WAMR_GIT_TAG}")
  message(STATUS "Using WAMR source directory: ${WAMR_SRC_DIR}")

  # Clone the repository
  FetchContent_Declare(
    wamr
    GIT_REPOSITORY ${WAMR_GIT_REPOSITORY}
    GIT_TAG ${WAMR_GIT_TAG}
    SOURCE_DIR ${WAMR_SRC_DIR}
  )
  
  # Make the content available
  FetchContent_GetProperties(wamr)
  if(NOT wamr_POPULATED)
    message(STATUS "Fetching WAMR repository...")
    FetchContent_MakeAvailable(wamr)
    message(STATUS "WAMR repository cloned to: ${WAMR_SRC_DIR}")
  endif()
  
  # Export the variable to parent scope
  set(WAMR_SRC_DIR ${WAMR_SRC_DIR} PARENT_SCOPE)
endfunction()

# Example CMakeLists.txt that uses this module:
#
# ```cmake
# cmake_minimum_required(VERSION 3.14)
# project(MyWasmProject)
#
# # Include WAMR source module
# include(cmake/wamr-src.cmake)
#
# # Optional: Set custom directory for WAMR sources
# # set(WAMR_SRC_DIR "${CMAKE_SOURCE_DIR}/third_party/wamr" CACHE PATH "Directory for WAMR sources")
#
# # Fetch WAMR sources
# fetch_wamr_source()
#
# # Configure WAMR build options
# set(WAMR_BUILD_PLATFORM "linux")
# set(WAMR_BUILD_TARGET "X86_64")
# set(WAMR_BUILD_INTERP 1)
# set(WAMR_BUILD_AOT 1)
# set(WAMR_BUILD_JIT 0)
# set(WAMR_BUILD_LIBC_BUILTIN 1)
# set(WAMR_BUILD_LIBC_WASI 1)
#
# # Include WAMR runtime
# add_subdirectory(${WAMR_SRC_DIR}/core/iwasm)
#
# # Your application that uses WAMR
# add_executable(my_wasm_app src/main.c)
# target_link_libraries(my_wasm_app vmlib)
# target_include_directories(my_wasm_app PRIVATE ${WAMR_SRC_DIR}/core/iwasm/include)
# ```
