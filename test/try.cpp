#define LOG_FN printf
#define LOG_ENABLED

#ifdef LOG_ENABLED
#define LOG(fmt, ...) LOG_FN(fmt, ##__VA_ARGS__)
#else
#define LOG(fmt, ...)
#endif

#include <cstdlib>
#include <iostream>

using namespace std;

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

    if (env[0] == '1') {
      LOG("Throwing exception...\n");

      // throw; // gets converted to call __cxa_rethrow
      throw(37703); // gets converted to call __cxa_throw
      // throw(std::runtime_error("MyRuntimeError"));
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
  handle(NULL, "1");
  LOG("Done\n");
  return 0;
}
