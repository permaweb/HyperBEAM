// native/hb_beamr/lib/test/fixtures/minimal_host_call.cpp

// No complex C++ headers, no std::, no exceptions
#include <stdio.h> // For printf inside Wasm, if needed for debugging, though it might become an import itself.
                   // For a truly minimal import list, avoid even this for now.

extern "C" {
    // Declare the simple host import we expect to be provided by the host environment.
    // Emscripten will likely place this under the "env" module.
    void my_simple_host_call(int val);
}

// Keep a simple global or static buffer for the result to avoid malloc/free complexities within this minimal test case.
static char result_buffer[50] = "Initial";

extern "C" char* handle(char* msg, char* env_str) {
    int value_to_pass_to_host = 0;

    if (env_str != NULL && env_str[0] == '1') {
        value_to_pass_to_host = 1;
    } else if (env_str != NULL && env_str[0] == '2') {
        value_to_pass_to_host = 2;
    } else {
        value_to_pass_to_host = 0;
    }

    // Call our simple host import
    my_simple_host_call(value_to_pass_to_host);
    
    // For simplicity, just return a static string. 
    // If my_simple_host_call changes result_buffer, that could be checked too.
    // sprintf(result_buffer, "Called host with %d", value_to_pass_to_host); // Avoid sprintf to keep it simple
    if (value_to_pass_to_host == 1) {
        return (char*)"HostCalledWith1"; // Return literal, assumes read-only memory is fine.
    } else {
        return (char*)"HostCalledWithOther";
    }
}

// Dummy main to allow Emscripten to compile without specific entry flags if needed,
// though --no-entry should handle it.
int main() { return 0; } 