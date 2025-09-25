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
add_executable(test c_src/test.c)
target_link_libraries(test PRIVATE hb_inference llama)

install(TARGETS hb_inference LIBRARY DESTINATION lib)
install(TARGETS test RUNTIME DESTINATION bin)
