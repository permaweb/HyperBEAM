cmake_minimum_required(VERSION 3.14)
project(hb_beamr_tests C)

find_program(EMCC_EXECUTABLE emcc)
if(NOT EMCC_EXECUTABLE)
  message(FATAL_ERROR "Emscripten (emcc) not found. Please ensure it is in your PATH.")
endif()
message(STATUS "Using Emscripten (emcc) for WASM compilation: ${EMCC_EXECUTABLE}")

# Attempt to find wat2wasm
find_program(WAT2WASM_EXECUTABLE wat2wasm)
if(NOT WAT2WASM_EXECUTABLE)
  message(WARNING "wat2wasm not found. The simple_memory.wasm fixture for C-API memory tests will not be built from .wat. Please ensure it exists or the test may fail.")
else()
  message(STATUS "Using wat2wasm for WAT to WASM compilation: ${WAT2WASM_EXECUTABLE}")
endif()

# --- Compile Fixture: basic_fib.wasm (common for C-API tests and as input for original AOT tests) ---
set(FIB_FIXTURE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/fixtures/basic_fib.cpp)
set(FIB_WASM_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/basic_fib.wasm)

if(NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/fixtures)
    message(FATAL_ERROR "Fixtures directory not found at ${CMAKE_CURRENT_SOURCE_DIR}/fixtures.")
endif()
if(NOT EXISTS ${FIB_FIXTURE_SRC})
    message(FATAL_ERROR "Fixture source file basic_fib.cpp not found at ${FIB_FIXTURE_SRC}.")
endif()

    add_custom_command(
      OUTPUT ${FIB_WASM_OUTPUT}
      COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "+++ Compiling fixture ${FIB_FIXTURE_SRC} to WASM (using emcc) +++"
  COMMAND ${EMCC_EXECUTABLE} -O2 -s STANDALONE_WASM -s EXPORTED_FUNCTIONS=['_fib'] ${FIB_FIXTURE_SRC} -o ${FIB_WASM_OUTPUT}
      DEPENDS ${FIB_FIXTURE_SRC}
      COMMENT "Compiling C++ fixture to WASM using emcc..."
      VERBATIM
    )
add_custom_target(fib_wasm_file_target DEPENDS ${FIB_WASM_OUTPUT})

# --- Compile Fixture: import_test_module.wasm (common for C-API tests and as input for original AOT tests) ---
set(IMPORT_TEST_FIXTURE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/fixtures/import_test_module.c)
set(IMPORT_TEST_WASM_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/import_test_module.wasm)

if(NOT EXISTS ${IMPORT_TEST_FIXTURE_SRC})
    message(FATAL_ERROR "Fixture source file import_test_module.c not found at ${IMPORT_TEST_FIXTURE_SRC}.")
endif()

    add_custom_command(
      OUTPUT ${IMPORT_TEST_WASM_OUTPUT}
      COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "+++ Compiling fixture ${IMPORT_TEST_FIXTURE_SRC} to WASM (using emcc) +++"
  COMMAND ${EMCC_EXECUTABLE} -O2 -s STANDALONE_WASM -s EXPORTED_FUNCTIONS=['_wasm_add_two_via_host'] -Wl,--no-entry -s ERROR_ON_UNDEFINED_SYMBOLS=0 ${IMPORT_TEST_FIXTURE_SRC} -o ${IMPORT_TEST_WASM_OUTPUT}
      DEPENDS ${IMPORT_TEST_FIXTURE_SRC}
      COMMENT "Compiling C import_test_module fixture to WASM using emcc..."
  VERBATIM
)
add_custom_target(import_test_wasm_file_target DEPENDS ${IMPORT_TEST_WASM_OUTPUT})

# --- Compile Fixture: basic_fib_capi.aot (for C-API AOT tests) ---
if(DEFINED WAMRC_EXECUTABLE AND TARGET build_wamrc_exe)
    set(FIB_CAPI_AOT_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/basic_fib_capi.aot)
    add_custom_command(
      OUTPUT ${FIB_CAPI_AOT_OUTPUT}
      COMMAND ${WAMRC_EXECUTABLE} -o ${FIB_CAPI_AOT_OUTPUT} ${FIB_WASM_OUTPUT} 
      DEPENDS fib_wasm_file_target build_wamrc_exe 
      COMMENT "Compiling basic_fib.wasm to AOT (for C-API tests)..."
      VERBATIM
    )
    add_custom_target(fib_capi_aot_file_target DEPENDS ${FIB_CAPI_AOT_OUTPUT})
else()
    message(STATUS "WAMRC not available. C-API AOT fixture basic_fib_capi.aot will not be built.")
    add_custom_target(fib_capi_aot_file_target) 
endif()

# --- Compile Fixture: import_test_module_capi.aot (for C-API AOT tests) ---
if(DEFINED WAMRC_EXECUTABLE AND TARGET build_wamrc_exe)
    set(IMPORT_CAPI_AOT_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/import_test_module_capi.aot)
    add_custom_command(
      OUTPUT ${IMPORT_CAPI_AOT_OUTPUT}
      COMMAND ${WAMRC_EXECUTABLE} -o ${IMPORT_CAPI_AOT_OUTPUT} ${IMPORT_TEST_WASM_OUTPUT}
      DEPENDS import_test_wasm_file_target build_wamrc_exe 
      COMMENT "Compiling import_test_module.wasm to AOT (for C-API tests)..."
      VERBATIM
    )
    add_custom_target(import_capi_aot_file_target DEPENDS ${IMPORT_CAPI_AOT_OUTPUT})
    else()
    message(STATUS "WAMRC not available. C-API AOT fixture import_test_module_capi.aot will not be built.")
    add_custom_target(import_capi_aot_file_target) 
    endif()


# --- Original Library Tests (AOT based) ---
if(DEFINED WAMRC_EXECUTABLE AND TARGET build_wamrc_exe)
    message(STATUS "WAMRC found. Building AOT fixtures for original hb_beamr_lib tests.")
    set(FIB_AOT_OUTPUT_FOR_ORIG_TESTS ${CMAKE_CURRENT_BINARY_DIR}/basic_fib_orig.aot)
    set(IMPORT_TEST_AOT_OUTPUT_FOR_ORIG_TESTS ${CMAKE_CURRENT_BINARY_DIR}/import_test_module_orig.aot)

    add_custom_command(
      OUTPUT ${FIB_AOT_OUTPUT_FOR_ORIG_TESTS}
      COMMAND ${WAMRC_EXECUTABLE} -o ${FIB_AOT_OUTPUT_FOR_ORIG_TESTS} ${FIB_WASM_OUTPUT}
      DEPENDS fib_wasm_file_target build_wamrc_exe 
      COMMENT "Compiling basic_fib.wasm to AOT (for original hb_beamr_lib tests)..."
      VERBATIM
    )
    add_custom_target(fib_aot_file_target_orig DEPENDS ${FIB_AOT_OUTPUT_FOR_ORIG_TESTS})

    add_custom_command(
      OUTPUT ${IMPORT_TEST_AOT_OUTPUT_FOR_ORIG_TESTS}
      COMMAND ${WAMRC_EXECUTABLE} -o ${IMPORT_TEST_AOT_OUTPUT_FOR_ORIG_TESTS} ${IMPORT_TEST_WASM_OUTPUT}
      DEPENDS import_test_wasm_file_target build_wamrc_exe
      COMMENT "Compiling import_test_module.wasm to AOT (for original hb_beamr_lib tests)..."
      VERBATIM
    )
    add_custom_target(import_test_aot_file_target_orig DEPENDS ${IMPORT_TEST_AOT_OUTPUT_FOR_ORIG_TESTS})

    add_executable(load_aot_module_test load_aot_module_test.c)
    target_link_libraries(load_aot_module_test PRIVATE hb_beamr_lib)
    add_dependencies(load_aot_module_test fib_aot_file_target_orig)
    add_custom_command(TARGET load_aot_module_test POST_BUILD 
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${FIB_AOT_OUTPUT_FOR_ORIG_TESTS} $<TARGET_FILE_DIR:load_aot_module_test>/basic_fib.aot)
    add_test(NAME LoadAotModuleTest COMMAND load_aot_module_test)

    add_executable(instantiate_call_test instantiate_call_test.c)
    target_link_libraries(instantiate_call_test PRIVATE hb_beamr_lib)
    add_dependencies(instantiate_call_test fib_aot_file_target_orig)
    add_custom_command(TARGET instantiate_call_test POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${FIB_AOT_OUTPUT_FOR_ORIG_TESTS} $<TARGET_FILE_DIR:instantiate_call_test>/basic_fib.aot)
    add_test(NAME InstantiateCallExportTest COMMAND instantiate_call_test)

    add_executable(import_call_test import_call_test.c)
    target_link_libraries(import_call_test PRIVATE hb_beamr_lib)
    add_dependencies(import_call_test import_test_aot_file_target_orig)
    add_custom_command(TARGET import_call_test POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different ${IMPORT_TEST_AOT_OUTPUT_FOR_ORIG_TESTS} $<TARGET_FILE_DIR:import_call_test>/import_test_module.aot)
    add_test(NAME ImportCallTest COMMAND import_call_test)
else()
    message(STATUS "WAMRC not available or build_wamrc_exe target missing. Original AOT-dependent tests for hb_beamr_lib will be skipped.")
    add_test(NAME LoadAotModuleTest COMMAND ${CMAKE_COMMAND} -E echo "Skipping LoadAotModuleTest (AOT support for hb_beamr_lib disabled)")
    add_test(NAME InstantiateCallExportTest COMMAND ${CMAKE_COMMAND} -E echo "Skipping InstantiateCallExportTest (AOT support for hb_beamr_lib disabled)")
    add_test(NAME ImportCallTest COMMAND ${CMAKE_COMMAND} -E echo "Skipping ImportCallTest (AOT support for hb_beamr_lib disabled)")
endif()

# Common tests for hb_beamr_lib (not AOT dependent)
add_executable(runtime_init_test runtime_init_test.c)
target_link_libraries(runtime_init_test PRIVATE hb_beamr_lib)
add_test(NAME RuntimeInitDestroyTest COMMAND runtime_init_test)

add_executable(context_management_test context_management_test.c)
target_link_libraries(context_management_test PRIVATE hb_beamr_lib)
add_test(NAME ContextManagementTest COMMAND context_management_test)


# --- C-API Library Tests (WASM and AOT based) ---
add_executable(capi_runtime_init_test capi_runtime_init_test.c)
target_link_libraries(capi_runtime_init_test PRIVATE hb_beamr_capi_lib)
# add_test(NAME CAPIRuntimeInitDestroyTest COMMAND capi_runtime_init_test)

add_executable(capi_context_management_test capi_context_management_test.c)
target_link_libraries(capi_context_management_test PRIVATE hb_beamr_capi_lib)
# add_test(NAME CAPIContextManagementTest COMMAND capi_context_management_test)

# C-API Load WASM Module Test
add_executable(capi_load_wasm_module_test capi_load_wasm_module_test.c utils.c)
target_link_libraries(capi_load_wasm_module_test PRIVATE hb_beamr_capi_lib)
add_dependencies(capi_load_wasm_module_test fib_wasm_file_target) 
add_custom_command(TARGET capi_load_wasm_module_test POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${FIB_WASM_OUTPUT} 
        $<TARGET_FILE_DIR:capi_load_wasm_module_test>/basic_fib.wasm
    COMMENT "Copying basic_fib.wasm to capi_load_wasm_module_test directory"
)
# add_test(NAME CAPILoadWasmModuleTest COMMAND capi_load_wasm_module_test)

# C-API Load AOT Module Test 
if(DEFINED WAMRC_EXECUTABLE AND TARGET build_wamrc_exe)
    add_executable(capi_load_aot_module_test capi_load_aot_module_test.c utils.c)
    target_link_libraries(capi_load_aot_module_test PRIVATE hb_beamr_capi_lib)
    add_dependencies(capi_load_aot_module_test fib_capi_aot_file_target) 
    add_custom_command(TARGET capi_load_aot_module_test POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${FIB_CAPI_AOT_OUTPUT} 
            $<TARGET_FILE_DIR:capi_load_aot_module_test>/basic_fib_capi.aot
        COMMENT "Copying basic_fib_capi.aot to capi_load_aot_module_test directory"
    )
    # add_test(NAME CAPILoadAotModuleTest COMMAND capi_load_aot_module_test)
else()
    # add_test(NAME CAPILoadAotModuleTest COMMAND ${CMAKE_COMMAND} -E echo "Skipping CAPILoadAotModuleTest (WAMRC not available for C-API AOT fixture)")
endif()

# C-API Instantiate and Call Export Test (WASM based)
add_executable(capi_instantiate_call_test capi_instantiate_call_test.c utils.c)
target_link_libraries(capi_instantiate_call_test PRIVATE hb_beamr_capi_lib)
add_dependencies(capi_instantiate_call_test fib_wasm_file_target) 
add_custom_command(TARGET capi_instantiate_call_test POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${FIB_WASM_OUTPUT} 
        $<TARGET_FILE_DIR:capi_instantiate_call_test>/basic_fib.wasm
    COMMENT "Copying basic_fib.wasm to capi_instantiate_call_test directory"
    )
# add_test(NAME CAPIInstantiateCallExportTest COMMAND capi_instantiate_call_test)

# C-API Import Call Test (WASM based)
add_executable(capi_import_call_test capi_import_call_test.c utils.c)
target_link_libraries(capi_import_call_test PRIVATE hb_beamr_capi_lib)
add_dependencies(capi_import_call_test import_capi_aot_file_target) 
add_custom_command(TARGET capi_import_call_test POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${IMPORT_CAPI_AOT_OUTPUT} 
        $<TARGET_FILE_DIR:capi_import_call_test>/import_test_module_capi.aot
    COMMENT "Copying import_test_module_capi.aot to capi_import_call_test directory"
    )
# add_test(NAME CAPIImportCallTest COMMAND capi_import_call_test)

# --- Compile Fixture: import_nested.wasm (for C-API nested import/export test) ---
set(NESTED_IMPORT_FIXTURE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/fixtures/import_nested.c)
set(NESTED_IMPORT_WASM_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/import_nested.wasm)

if(NOT EXISTS ${NESTED_IMPORT_FIXTURE_SRC})
    message(FATAL_ERROR "Fixture source file import_nested.c not found at ${NESTED_IMPORT_FIXTURE_SRC}.")
endif()

add_custom_command(
  OUTPUT ${NESTED_IMPORT_WASM_OUTPUT}
  COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "+++ Compiling fixture ${NESTED_IMPORT_FIXTURE_SRC} to WASM (using emcc) +++"
  COMMAND ${EMCC_EXECUTABLE} -O2 -s STANDALONE_WASM -s EXPORTED_FUNCTIONS=['_get_data_ptr','_xor_memory','_call_host_and_read'] -s ERROR_ON_UNDEFINED_SYMBOLS=0 ${NESTED_IMPORT_FIXTURE_SRC} -o ${NESTED_IMPORT_WASM_OUTPUT}
  DEPENDS ${NESTED_IMPORT_FIXTURE_SRC}
  COMMENT "Compiling C import_nested fixture to WASM using emcc..."
  VERBATIM
)
add_custom_target(nested_import_wasm_file_target DEPENDS ${NESTED_IMPORT_WASM_OUTPUT})

# --- Compile Fixture: import_nested.aot (for C-API nested import/export test) ---
set(NESTED_IMPORT_AOT_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/import_nested.aot)
if(DEFINED WAMRC_EXECUTABLE AND TARGET build_wamrc_exe)
    add_custom_command(
      OUTPUT ${NESTED_IMPORT_AOT_OUTPUT}
      COMMAND ${WAMRC_EXECUTABLE} -o ${NESTED_IMPORT_AOT_OUTPUT} ${NESTED_IMPORT_WASM_OUTPUT}
      DEPENDS nested_import_wasm_file_target build_wamrc_exe
      COMMENT "Compiling import_nested.wasm to AOT (for C-API nested test)..."
      VERBATIM
    )
    add_custom_target(nested_import_aot_file_target DEPENDS ${NESTED_IMPORT_AOT_OUTPUT})
else()
    message(STATUS "WAMRC not available. C-API AOT fixture import_nested.aot will not be built.")
    add_custom_target(nested_import_aot_file_target) # Dummy target
endif()

# C-API Import Call Export Test (AOT based, nested logic)
add_executable(capi_import_call_export_test capi_import_call_export_test.c utils.c)
target_link_libraries(capi_import_call_export_test PRIVATE hb_beamr_capi_lib)
add_dependencies(capi_import_call_export_test nested_import_aot_file_target) 
add_custom_command(TARGET capi_import_call_export_test POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    ${NESTED_IMPORT_AOT_OUTPUT} 
    $<TARGET_FILE_DIR:capi_import_call_export_test>/import_nested.aot
    COMMENT "Copying import_nested.aot to capi_import_call_export_test directory"
)
# add_test(NAME CAPIImportCallExportTest COMMAND capi_import_call_export_test)

# --- Compile Fixture: simple_memory.wasm (for C-API memory access tests using wat2wasm) ---
set(SIMPLE_MEMORY_WAT_SRC ${CMAKE_CURRENT_SOURCE_DIR}/fixtures/simple_memory.wat)
set(SIMPLE_MEMORY_WASM_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/simple_memory.wasm)

if(EXISTS ${SIMPLE_MEMORY_WAT_SRC} AND WAT2WASM_EXECUTABLE)
    add_custom_command(
      OUTPUT ${SIMPLE_MEMORY_WASM_OUTPUT}
      COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "+++ Compiling fixture ${SIMPLE_MEMORY_WAT_SRC} to WASM (using wat2wasm) +++"
      COMMAND ${WAT2WASM_EXECUTABLE} ${SIMPLE_MEMORY_WAT_SRC} -o ${SIMPLE_MEMORY_WASM_OUTPUT}
      DEPENDS ${SIMPLE_MEMORY_WAT_SRC}
      COMMENT "Compiling WAT fixture to WASM using wat2wasm..."
      VERBATIM
    )
    add_custom_target(simple_memory_wasm_file_target DEPENDS ${SIMPLE_MEMORY_WASM_OUTPUT})
else()
    # Create a dummy target if wat2wasm is not found or .wat file is missing
    # The test will rely on simple_memory.wasm being present from a previous build or manually copied.
    add_custom_target(simple_memory_wasm_file_target)
    if(NOT WAT2WASM_EXECUTABLE)
        message(STATUS "wat2wasm not found, skipping build of simple_memory.wasm from .wat. Test will look for pre-existing file.")
    elseif(NOT EXISTS ${SIMPLE_MEMORY_WAT_SRC})
        message(STATUS "simple_memory.wat not found, skipping build. Test will look for pre-existing simple_memory.wasm.")
    endif()
endif()

# --- Compile Fixture: simple_memory.aot (for WAMRMemoryAccessTest) ---
set(SIMPLE_MEMORY_AOT_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/simple_memory.aot)
if(DEFINED WAMRC_EXECUTABLE AND TARGET build_wamrc_exe)
    add_custom_command(
      OUTPUT ${SIMPLE_MEMORY_AOT_OUTPUT}
      COMMAND ${WAMRC_EXECUTABLE} -o ${SIMPLE_MEMORY_AOT_OUTPUT} ${SIMPLE_MEMORY_WASM_OUTPUT}
      DEPENDS simple_memory_wasm_file_target build_wamrc_exe
      COMMENT "Compiling simple_memory.wasm to AOT..."
      VERBATIM
    )
    add_custom_target(simple_memory_aot_file_target DEPENDS ${SIMPLE_MEMORY_AOT_OUTPUT})
else()
    message(STATUS "WAMRC not available. AOT fixture simple_memory.aot will not be built.")
    add_custom_target(simple_memory_aot_file_target)
endif()

# C-API Memory Access Test
add_executable(capi_memory_access_test capi_memory_access_test.c utils.c)
target_link_libraries(capi_memory_access_test PRIVATE hb_beamr_capi_lib)
add_dependencies(capi_memory_access_test simple_memory_wasm_file_target)
add_custom_command(TARGET capi_memory_access_test POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    ${SIMPLE_MEMORY_WASM_OUTPUT} 
    $<TARGET_FILE_DIR:capi_memory_access_test>/simple_memory.wasm
    COMMENT "Copying simple_memory.wasm to capi_memory_access_test directory"
)
# # add_test(NAME CAPIMemoryAccessTest COMMAND capi_memory_access_test) # Disabled as requested

# Test for WAMR-native hb_beamr_lib memory access
add_executable(WAMRMemoryAccessTest wamr_memory_access_test.c utils.c)
# Link against hb_beamr_lib (WAMR-native) and WAMR's iwasm library
target_link_libraries(WAMRMemoryAccessTest PRIVATE hb_beamr_lib iwasm_static)
# Copy the necessary wasm file to the build directory for the test
add_dependencies(WAMRMemoryAccessTest simple_memory_aot_file_target)
add_custom_command(TARGET WAMRMemoryAccessTest POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    ${SIMPLE_MEMORY_AOT_OUTPUT}
    $<TARGET_FILE_DIR:WAMRMemoryAccessTest>/simple_memory.aot
    COMMENT "Copying simple_memory.aot to WAMRMemoryAccessTest directory"
)
add_test(NAME WAMRMemoryAccessTest COMMAND WAMRMemoryAccessTest)

# --- Compile Fixture: simple_indirect.wasm (from WAT for IndirectCallTest) ---
set(SIMPLE_INDIRECT_WAT_SRC ${CMAKE_CURRENT_SOURCE_DIR}/fixtures/simple_indirect.wat)
set(SIMPLE_INDIRECT_WASM_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/simple_indirect.wasm) # Explicitly use this name

if(NOT EXISTS ${SIMPLE_INDIRECT_WAT_SRC})
    message(FATAL_ERROR "Fixture source file simple_indirect.wat not found at ${SIMPLE_INDIRECT_WAT_SRC}.")
endif()

if(NOT WAT2WASM_EXECUTABLE)
    message(FATAL_ERROR "wat2wasm not found. Cannot compile simple_indirect.wat.")
else()
    # Target to produce simple_indirect.wasm from simple_indirect.wat
    add_custom_command(
      OUTPUT ${SIMPLE_INDIRECT_WASM_OUTPUT}
      COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "+++ Compiling fixture ${SIMPLE_INDIRECT_WAT_SRC} to WASM (using wat2wasm) +++"
      COMMAND ${WAT2WASM_EXECUTABLE} ${SIMPLE_INDIRECT_WAT_SRC} -o ${SIMPLE_INDIRECT_WASM_OUTPUT}
      DEPENDS ${SIMPLE_INDIRECT_WAT_SRC}
      COMMENT "Compiling simple_indirect.wat to simple_indirect.wasm using wat2wasm..."
      VERBATIM
    )
    add_custom_target(simple_indirect_wat_to_wasm_target DEPENDS ${SIMPLE_INDIRECT_WASM_OUTPUT})
endif()

# --- Compile Fixture: simple_indirect.aot (for IndirectCallTest) ---
set(SIMPLE_INDIRECT_AOT_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/simple_indirect.aot)
if(DEFINED WAMRC_EXECUTABLE AND TARGET build_wamrc_exe)
    add_custom_command(
      OUTPUT ${SIMPLE_INDIRECT_AOT_OUTPUT}
      COMMAND ${WAMRC_EXECUTABLE} -o ${SIMPLE_INDIRECT_AOT_OUTPUT} ${SIMPLE_INDIRECT_WASM_OUTPUT}
      DEPENDS simple_indirect_wat_to_wasm_target build_wamrc_exe
      COMMENT "Compiling simple_indirect.wasm to AOT..."
      VERBATIM
    )
    add_custom_target(simple_indirect_aot_file_target DEPENDS ${SIMPLE_INDIRECT_AOT_OUTPUT})
else()
    message(STATUS "WAMRC not available. AOT fixture simple_indirect.aot will not be built.")
    add_custom_target(simple_indirect_aot_file_target)
endif()

# Test for indirect function calls using simple_indirect.wasm
add_executable(IndirectCallTest indirect_call_test.c utils.c)
target_link_libraries(IndirectCallTest PRIVATE hb_beamr_lib iwasm_static)
# Make IndirectCallTest depend on the target that creates simple_indirect.aot
add_dependencies(IndirectCallTest simple_indirect_aot_file_target)

add_custom_command(TARGET IndirectCallTest POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    ${SIMPLE_INDIRECT_AOT_OUTPUT}
    $<TARGET_FILE_DIR:IndirectCallTest>/simple_indirect.aot
    COMMENT "Copying simple_indirect.aot for IndirectCallTest"
)
add_test(NAME IndirectCallTest COMMAND IndirectCallTest)

# Test for WAMR-native hb_beamr_lib import/export/callback behavior
add_executable(WAMRImportCallExportTest wamr_import_call_export_test.c utils.c)
target_link_libraries(WAMRImportCallExportTest PRIVATE hb_beamr_lib iwasm_static)

# Depends on nested_import_wasm_file_target (produces import_nested.wasm from import_nested.c)
# This target should already be defined from the C-API tests section.
add_dependencies(WAMRImportCallExportTest nested_import_wasm_file_target) 

add_custom_command(TARGET WAMRImportCallExportTest POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    ${CMAKE_CURRENT_BINARY_DIR}/import_nested.wasm  # Source: _build/test/import_nested.wasm
    $<TARGET_FILE_DIR:WAMRImportCallExportTest>/import_nested.wasm
    COMMENT "Copying import_nested.wasm for WAMRImportCallExportTest"
)
add_test(NAME WAMRImportCallExportTest COMMAND WAMRImportCallExportTest)

# --- Compile Fixture: try_catch.wasm (for exception handling tests) ---
set(TRY_CATCH_FIXTURE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/fixtures/try_catch.cpp)
set(TRY_CATCH_WASM_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/try_catch.wasm)

if(NOT EXISTS ${TRY_CATCH_FIXTURE_SRC})
    message(FATAL_ERROR "Fixture source file try_catch.cpp not found at ${TRY_CATCH_FIXTURE_SRC}.")
endif()

add_custom_command(
  OUTPUT ${TRY_CATCH_WASM_OUTPUT}
  COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "+++ Compiling fixture ${TRY_CATCH_FIXTURE_SRC} to WASM (using emcc for C++) +++"
  COMMAND ${EMCC_EXECUTABLE}
          -O3 -g2 
          -s STANDALONE_WASM
          -s EXPORTED_FUNCTIONS=['_my_funcs']
          -fexceptions
          -s DISABLE_EXCEPTION_CATCHING=0
          --no-entry
          ${TRY_CATCH_FIXTURE_SRC}
          -o ${TRY_CATCH_WASM_OUTPUT}
  DEPENDS ${TRY_CATCH_FIXTURE_SRC}
  COMMENT "Compiling C++ try_catch fixture to WASM using emcc..."
  VERBATIM
)
add_custom_target(try_catch_wasm_file_target DEPENDS ${TRY_CATCH_WASM_OUTPUT})

# --- Compile Fixture: try_catch.aot (for exception handling tests) ---
set(TRY_CATCH_AOT_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/try_catch.aot)
if(DEFINED WAMRC_EXECUTABLE AND TARGET build_wamrc_exe)
    add_custom_command(
      OUTPUT ${TRY_CATCH_AOT_OUTPUT}
      COMMAND ${WAMRC_EXECUTABLE} -o ${TRY_CATCH_AOT_OUTPUT} ${TRY_CATCH_WASM_OUTPUT}
      DEPENDS try_catch_wasm_file_target build_wamrc_exe
      COMMENT "Compiling try_catch.wasm to AOT..."
      VERBATIM
    )
    add_custom_target(try_catch_aot_file_target DEPENDS ${TRY_CATCH_AOT_OUTPUT})
else()
    message(STATUS "WAMRC not available. AOT fixture try_catch.aot will not be built.")
    add_custom_target(try_catch_aot_file_target)
endif()

# Test for C++ exception handling in Wasm with hb_beamr_lib
add_executable(TryCatchTest try_catch_test.c utils.c)
target_link_libraries(TryCatchTest PRIVATE hb_beamr_lib iwasm_static)
add_dependencies(TryCatchTest try_catch_aot_file_target) # Depends on the try_catch.aot fixture
# Copy the necessary wasm file to the build directory for the test
add_custom_command(TARGET TryCatchTest POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    ${TRY_CATCH_AOT_OUTPUT}
    $<TARGET_FILE_DIR:TryCatchTest>/try_catch.aot
    COMMENT "Copying try_catch.aot to TryCatchTest directory"
)
add_test(NAME TryCatchTest COMMAND TryCatchTest) # Re-enabled

# --- Compile Fixture: jmp.wasm (setjmp/longjmp test) ---
set(JMP_FIXTURE_SRC ${CMAKE_CURRENT_SOURCE_DIR}/fixtures/jmp.cpp)
set(JMP_WASM_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/jmp.wasm)

if(NOT EXISTS ${JMP_FIXTURE_SRC})
    message(FATAL_ERROR "Fixture source file jmp.cpp not found at ${JMP_FIXTURE_SRC}.")
endif()

add_custom_command(
  OUTPUT ${JMP_WASM_OUTPUT}
  COMMAND ${CMAKE_COMMAND} -E cmake_echo_color --blue "+++ Compiling fixture ${JMP_FIXTURE_SRC} to WASM (using emcc) +++"
  COMMAND ${EMCC_EXECUTABLE} -O2 -s STANDALONE_WASM -s EXPORTED_FUNCTIONS=['_funcs'] ${JMP_FIXTURE_SRC} -o ${JMP_WASM_OUTPUT}
  DEPENDS ${JMP_FIXTURE_SRC}
  COMMENT "Compiling C++ jmp fixture to WASM using emcc..."
  VERBATIM
)
add_custom_target(jmp_wasm_file_target DEPENDS ${JMP_WASM_OUTPUT})

# --- Compile Fixture: jmp.aot (for LongJmpTest) ---
set(JMP_AOT_OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/jmp.aot)
if(DEFINED WAMRC_EXECUTABLE AND TARGET build_wamrc_exe)
    add_custom_command(
      OUTPUT ${JMP_AOT_OUTPUT}
      COMMAND ${WAMRC_EXECUTABLE} -o ${JMP_AOT_OUTPUT} ${JMP_WASM_OUTPUT}
      DEPENDS jmp_wasm_file_target build_wamrc_exe
      COMMENT "Compiling jmp.wasm to AOT..."
      VERBATIM
    )
    add_custom_target(jmp_aot_file_target DEPENDS ${JMP_AOT_OUTPUT})
else()
    message(STATUS "WAMRC not available. AOT fixture jmp.aot will not be built.")
    add_custom_target(jmp_aot_file_target)
endif()

# --- Test: LongJmpTest (hb_beamr_lib with setjmp/longjmp) ---
add_executable(LongJmpTest long_jmp_test.c utils.c)
# Link against hb_beamr_lib and WAMR static library similar to other tests
target_link_libraries(LongJmpTest PRIVATE hb_beamr_lib iwasm_static)
add_dependencies(LongJmpTest jmp_aot_file_target)
add_custom_command(TARGET LongJmpTest POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
    ${JMP_AOT_OUTPUT}
    $<TARGET_FILE_DIR:LongJmpTest>/jmp.aot
    COMMENT "Copying jmp.aot to LongJmpTest directory"
)
add_test(NAME LongJmpTest COMMAND LongJmpTest)

# TODO: Add C-API tests for AOT loaded modules (instantiate, call export, import call)