cmake_minimum_required(VERSION 3.14)
project(hb_beamr_tests C)

find_program(EMCC_EXECUTABLE emcc)
if(NOT EMCC_EXECUTABLE)
  message(FATAL_ERROR "Emscripten (emcc) not found. Please ensure it is in your PATH.")
endif()
message(STATUS "Using Emscripten (emcc) for WASM compilation: ${EMCC_EXECUTABLE}")

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
add_test(NAME CAPIRuntimeInitDestroyTest COMMAND capi_runtime_init_test)

add_executable(capi_context_management_test capi_context_management_test.c)
target_link_libraries(capi_context_management_test PRIVATE hb_beamr_capi_lib)
add_test(NAME CAPIContextManagementTest COMMAND capi_context_management_test)

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
add_test(NAME CAPILoadWasmModuleTest COMMAND capi_load_wasm_module_test)

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
    add_test(NAME CAPILoadAotModuleTest COMMAND capi_load_aot_module_test)
else()
    add_test(NAME CAPILoadAotModuleTest COMMAND ${CMAKE_COMMAND} -E echo "Skipping CAPILoadAotModuleTest (WAMRC not available for C-API AOT fixture)")
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
add_test(NAME CAPIInstantiateCallExportTest COMMAND capi_instantiate_call_test)

# C-API Import Call Test (WASM based)
add_executable(capi_import_call_test capi_import_call_test.c utils.c)
target_link_libraries(capi_import_call_test PRIVATE hb_beamr_capi_lib)
add_dependencies(capi_import_call_test import_test_wasm_file_target) 
add_custom_command(TARGET capi_import_call_test POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${IMPORT_TEST_WASM_OUTPUT} 
        $<TARGET_FILE_DIR:capi_import_call_test>/import_test_module.wasm
    COMMENT "Copying import_test_module.wasm to capi_import_call_test directory"
    )
add_test(NAME CAPIImportCallTest COMMAND capi_import_call_test)

# TODO: Add C-API tests for AOT loaded modules (instantiate, call export, import call)
# These would depend on fib_capi_aot_file_target and import_capi_aot_file_target