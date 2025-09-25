# CMakeLists.txt for hb_inference

cmake_minimum_required(VERSION 3.14)
project(hb_inference C CXX)

# Add llama.cpp as a subdirectory
add_subdirectory(../../_build/llama.cpp llama_build)

# Build the shared library
add_library(hb_inference SHARED c_src/hb_inference.cpp)
target_link_libraries(hb_inference PRIVATE llama)
target_include_directories(hb_inference PUBLIC 
    c_src
    ../../_build/llama.cpp/include
    ../../_build/llama.cpp/ggml/include
)


# Build the test executable
add_executable(test c_src/test.cpp)
target_link_libraries(test PRIVATE hb_inference llama)

install(TARGETS test RUNTIME DESTINATION bin)

# Erlang NIF specific paths
set(ERLANG_INCLUDE_DIR "/usr/local/lib/erlang/usr/include")
include_directories(${ERLANG_INCLUDE_DIR})

# Build the NIF as a shared library
add_library(hb_inference_nif MODULE c_src/hb_inference_nif.cpp)
set_target_properties(hb_inference_nif PROPERTIES PREFIX "")
target_link_libraries(hb_inference_nif PRIVATE hb_inference llama)

# Install the NIF library
install(TARGETS hb_inference_nif LIBRARY DESTINATION lib)
