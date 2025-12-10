cmake_minimum_required(VERSION 3.11)
project(dev_sev_gpu_nif VERSION 0.1.0)

set(CMAKE_C_STANDARD 11)

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

add_library(dev_sev_gpu_nif SHARED
    dev_sev_gpu_nif.c
)

target_include_directories(dev_sev_gpu_nif PRIVATE
    ${ERL_NIF_INCLUDE_DIR}
    ${NVAT_BUILD_DIR}/include
)

target_link_directories(dev_sev_gpu_nif PRIVATE
    ${NVAT_BUILD_DIR}
)

target_link_libraries(dev_sev_gpu_nif PRIVATE
    nvat
)

# Set output properties to match Erlang NIF expectations
set_target_properties(dev_sev_gpu_nif PROPERTIES
    PREFIX ""
    SUFFIX ".so"
    # Set RPATH to find libnvat.so at runtime
    INSTALL_RPATH "$ORIGIN/../lib"
    BUILD_WITH_INSTALL_RPATH TRUE
)
