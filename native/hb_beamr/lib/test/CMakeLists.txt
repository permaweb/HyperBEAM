cmake_minimum_required(VERSION 3.14)
project(hb_beamr_tests C)

# Testing is enabled in the parent scope
# enable_testing()
# include(CTest)

# --- Compile Fixtures ---

# Find Emscripten compiler (emcc)
find_program(EMCC_EXECUTABLE emcc REQUIRED)
message(STATUS "Using Emscripten (emcc) for WASM compilation: ${EMCC_EXECUTABLE}")

# Set compiler command using emcc for standalone WASM
# -s STANDALONE_WASM: Create a WASM file without JS glue code.
# -s EXPORTED_FUNCTIONS='["_handle"]': Export the _handle function (note leading underscore).
# -O2: Optimization level.
# emcc automatically targets wasm32 and provides necessary stdlibs.
# Try adding -mno-bulk-memory feature flag
set(WASM_CXX_COMPILER ${EMCC_EXECUTABLE} -O2 -s STANDALONE_WASM -s EXPORTED_FUNCTIONS=\[\"_handle\"\] -mno-bulk-memory)

# Path to the fixture source
set(FIB_FIXTURE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/fixtures/basic_fib.cpp)

# Output paths in the build directory for this test
set(FIB_WASM_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/basic_fib.wasm)
set(FIB_AOT_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/basic_fib.aot)

# WAMRC executable path (should be set by parent CMakeLists.txt)
if(NOT DEFINED WAMRC_EXECUTABLE)
  message(FATAL_ERROR "WAMRC_EXECUTABLE variable is not defined. Parent CMakeLists should set it.")
endif()
message(STATUS "Using wamrc executable: ${WAMRC_EXECUTABLE}")

# Custom command to compile C++ to WASM
add_custom_command(
  OUTPUT ${FIB_WASM_OUTPUT}
  COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "+++ Compiling fixture ${FIB_FIXTURE_SRC} to WASM (using emcc) +++"
  COMMAND ${WASM_CXX_COMPILER} ${FIB_FIXTURE_SRC} -o ${FIB_WASM_OUTPUT}
  DEPENDS ${FIB_FIXTURE_SRC}
  COMMENT "Compiling C++ fixture to WASM using emcc..."
  VERBATIM
)

# Custom command to compile WASM to AOT
add_custom_command(
  OUTPUT ${FIB_AOT_OUTPUT}
  COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "+++ Compiling ${FIB_WASM_OUTPUT} to AOT (bulk-memory disabled) +++"
  # Use the wamrc executable built earlier, disable bulk memory
  COMMAND ${WAMRC_EXECUTABLE} -o ${FIB_AOT_OUTPUT} ${FIB_WASM_OUTPUT}
  # Depends on the WASM file and the wamrc executable itself
  DEPENDS ${FIB_WASM_OUTPUT} ${WAMRC_EXECUTABLE}
  COMMENT "Compiling WASM to AOT using wamrc..."
  VERBATIM
)

# Custom target that depends on the AOT file being generated
# Also explicitly depend on the wamrc build target from the parent scope
add_custom_target(fib_aot_file_target DEPENDS ${FIB_AOT_OUTPUT} build_wamrc_exe)

# --- Test Executable ---

# Add the test executable, including the new utils.c
add_executable(load_aot_module_test load_aot_module_test.c utils.c)

# Link against the main library (hb_beamr_lib)
# Ensure the library is built before the test
target_link_libraries(load_aot_module_test PRIVATE hb_beamr_lib)

# Make the test executable depend on the AOT file target being built
# and also depend on the wamrc executable being built (target from parent scope)
add_dependencies(load_aot_module_test fib_aot_file_target build_wamrc_exe)

# Ensure the AOT file is copied next to the test executable in the build tree
# so the test can load it using a relative path like "./basic_fib.aot"
add_custom_command(TARGET load_aot_module_test POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${FIB_AOT_OUTPUT}
        $<TARGET_FILE_DIR:load_aot_module_test>/basic_fib.aot
    COMMENT "Copying AOT fixture to test executable directory"
)

# Add the test to CTest
# Use the target name directly
add_test(NAME LoadAotModuleTest COMMAND load_aot_module_test)
