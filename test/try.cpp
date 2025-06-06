// #define LOG_ENABLED
// #include "log.h"
#ifndef LOG
#define LOG(fmt, ...) do {} while (0)
#endif

#include <cstring>
#include <stdexcept>

char result_str[256] = "Initial";

extern "C" char *handle(char *msg, char *env) {
  LOG("printf: handle()\n");
  LOG("msg: %s\n", msg);
  LOG("env: %s\n", env);

  // This triggers a bug in WAMR
  // Calling `__wasm_call_ctors` before functions are linked
  // cout << "cout handle()" << endl;

  try {
    LOG("Try\n");

    // strcpy(result_str, "Try");
    LOG("result_str: %s\n", result_str);

    LOG("env[0]: %c\n", env[0]);

    // int d = atoi(env);
    if (env[0] == '1') {
      LOG("About to `throw` ...\n");
      throw; // gets converted to call __cxa_rethrow
    }
    else if (env[0] == '2') {
      LOG("About to `throw(37703)` ...\n");
      throw(37703); // gets converted to call __cxa_throw
    }
    else if (env[0] == '3') {
      LOG("About to `throw(std::runtime_error(\"MyRuntimeError\"))` ...\n");
      throw(std::runtime_error("MyRuntimeError"));
    }
    else if (env[0] == '4') {
      int *x = (int *)0xFFFFFFFFFFFF; // 48-bit pointer
      LOG("About to dereference invalid pointer (%p)...\n", x);
      int val = *x;
      if (val == 0) return NULL;
      LOG("x: %d\n", val);
    }

    LOG("No Exception\n");
  } catch (...) {
    LOG("Exception caught\n");

    strcpy(result_str, "Catch");
    LOG("result_str: %s\n", result_str);

    // LOG("e.what(): %s\n", e.what());
  }

  return result_str;
}

int main(int argc, char *argv[]) {
  LOG("C++ - try/catch example\n");
  handle(NULL, "0");
  LOG("Done\n");
  return 0;
}
