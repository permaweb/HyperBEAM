cmake_minimum_required(VERSION 3.11)
project(dev_sev_gpu_nif VERSION 0.1.0 LANGUAGES CXX)

# Debug logging option (default: OFF for quiet operation)
option(NVAT_DEBUG_LOG "Enable verbose debug logging from nvat SDK" OFF)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find Erlang NIF headers
find_path(ERL_NIF_INCLUDE_DIR erl_nif.h
    HINTS /usr/local/lib/erlang/usr/include
          /usr/lib/erlang/usr/include
          $ENV{ERL_TOP}/erts-*/include
)

if(NOT ERL_NIF_INCLUDE_DIR)
    message(FATAL_ERROR "Could not find erl_nif.h. Make sure Erlang development headers are installed.")
endif()

# NVAT SDK paths (passed from Makefile)
set(NVAT_SDK_DIR "" CACHE PATH "Path to nvat SDK source")
set(NVAT_BUILD_DIR "" CACHE PATH "Path to nvat SDK build")

if(NOT NVAT_SDK_DIR OR NOT NVAT_BUILD_DIR)
    message(FATAL_ERROR "NVAT_SDK_DIR and NVAT_BUILD_DIR must be specified")
endif()

# Find nlohmann/json from nvat build (it's fetched by nvat's CMake)
find_path(NLOHMANN_JSON_INCLUDE_DIR nlohmann/json.hpp
    HINTS ${NVAT_BUILD_DIR}/_deps/json-src/include
          ${NVAT_BUILD_DIR}/_deps/json-src/single_include
)

if(NOT NLOHMANN_JSON_INCLUDE_DIR)
    message(FATAL_ERROR "Could not find nlohmann/json.hpp. Make sure nvat SDK was built first.")
endif()

add_library(dev_sev_gpu_nif SHARED
    dev_sev_gpu_nif.cpp
)

target_include_directories(dev_sev_gpu_nif PRIVATE
    ${ERL_NIF_INCLUDE_DIR}
    ${NVAT_BUILD_DIR}/include
    ${NLOHMANN_JSON_INCLUDE_DIR}
)

target_link_directories(dev_sev_gpu_nif PRIVATE
    ${NVAT_BUILD_DIR}
)

target_link_libraries(dev_sev_gpu_nif PRIVATE
    nvat
)

# Add debug log definition if enabled
if(NVAT_DEBUG_LOG)
    target_compile_definitions(dev_sev_gpu_nif PRIVATE NVAT_DEBUG_LOG)
endif()

# Set output properties to match Erlang NIF expectations
set_target_properties(dev_sev_gpu_nif PROPERTIES
    PREFIX ""
    SUFFIX ".so"
    # Set RPATH to find libnvat.so at runtime
    INSTALL_RPATH "$ORIGIN/../lib"
    BUILD_WITH_INSTALL_RPATH TRUE
)
