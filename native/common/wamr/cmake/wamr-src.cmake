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
find_package(Git QUIET) # To get GIT_EXECUTABLE for local changes check

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
  message(STATUS "Ensuring WAMR source is available at: ${WAMR_SRC_DIR}")

  # Base arguments for FetchContent_Declare
  set(FC_ARGS
    GIT_REPOSITORY ${WAMR_GIT_REPOSITORY}
    GIT_TAG ${WAMR_GIT_TAG}
    SOURCE_DIR ${WAMR_SRC_DIR}
    GIT_SHALLOW TRUE # Fetch only the latest commit history
  )

  if(EXISTS "${WAMR_SRC_DIR}/.git" AND IS_DIRECTORY "${WAMR_SRC_DIR}/.git")
    message(STATUS "WAMR source directory ${WAMR_SRC_DIR} found with .git folder.")

    if(GIT_EXECUTABLE)
      execute_process(
        COMMAND ${GIT_EXECUTABLE} -C "${WAMR_SRC_DIR}" status --porcelain
        OUTPUT_VARIABLE GIT_STATUS_OUTPUT
        OUTPUT_STRIP_TRAILING_WHITESPACE
        RESULT_VARIABLE GIT_STATUS_RESULT
        ERROR_QUIET # Be resilient to git command issues in atypical setups
      )
      if(NOT GIT_STATUS_RESULT EQUAL 0 AND NOT GIT_STATUS_RESULT EQUAL "") # Check for actual failure code
         message(WARNING "Git status check failed for ${WAMR_SRC_DIR} (Code: ${GIT_STATUS_RESULT}). Proceeding without local changes check.")
      elseif(GIT_STATUS_OUTPUT)
        message(WARNING "WAMR source directory ${WAMR_SRC_DIR} has local changes:\\n${GIT_STATUS_OUTPUT}\\nBuild will use these local changes.")
      else()
        message(STATUS "WAMR source directory ${WAMR_SRC_DIR} has no detected local changes.")
      endif()
    else()
      message(WARNING "Git executable not found. Cannot check for local changes in ${WAMR_SRC_DIR}.")
    endif()

    # For existing directories, try to avoid network access if content matches tag.
    list(APPEND FC_ARGS UPDATE_DISCONNECTED ON)
    message(STATUS "Integrating existing WAMR content from ${WAMR_SRC_DIR} (UPDATE_DISCONNECTED=ON).")
  else()
    message(STATUS "WAMR source directory ${WAMR_SRC_DIR} not found or not a git repository. FetchContent will populate...")
    # No UPDATE_DISCONNECTED here, as it needs to download.
  endif()

  FetchContent_Declare(wamr ${FC_ARGS})
  FetchContent_MakeAvailable(wamr) # Handles population if needed, and integration.

  # After MakeAvailable, wamr_SOURCE_DIR is the definitive source directory.
  message(STATUS "WAMR content from ${wamr_SOURCE_DIR} is now available for the build.")
  
  # Export the variable to parent scope, ensuring it's the one FetchContent actually used.
  set(WAMR_SRC_DIR ${wamr_SOURCE_DIR} PARENT_SCOPE)
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
