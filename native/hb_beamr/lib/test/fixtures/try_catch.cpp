// #define LOG_ENABLED

#include <cstring>
#include <stdexcept>

char result_str[256] = "Initial";

extern "C" char *my_funcs(int a) {
  try {
    // int d = atoi(env);
    if (a == 1) {
      throw; // gets converted to call __cxa_rethrow
    }
    else if (a == 2) {
      throw(37703); // gets converted to call __cxa_throw
    }
    else if (a == 3) {
      throw(std::runtime_error("MyRuntimeError"));
    }
    else if (a == 4) {
      int *x = (int *)0xFFFFFFFFFFFF; // 48-bit pointer
      int val = *x;
      if (val == 0) return NULL;
    }

  } catch (...) {
    strcpy(result_str, "Catch");
  }

  return result_str;
}
