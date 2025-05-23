// #define LOG_ENABLED
#include "log.h"

#include <cstdlib>
#include <cstring>

void func_inner(int b) {
  // Some stack variables
  int a = 789;

  LOG("func_inner (before): %d\n", a);
  if (b == 1) {
    throw(42);
  } else if (b == 2) {
    throw;
  } else {
    LOG("func_inner (do nothing): %d\n", a);
  }

  LOG("func_inner (unreachable): %d\n", a);
}

void func_outer(int b) {
  // Some stack variables
  int a = 456;

  LOG("func_outer (before): %d\n", a);
  try {
    LOG("func_outer (try): %d\n", a);
    func_inner(b);
    LOG("func_outer (unreachable): %d\n", a);
  // } catch (const std::exception &e) {
  //   LOG("func_outer (exception): %s\n", e.what());
  } catch (...) {
    LOG("func_outer (catch): %d\n", a);
  }
  LOG("func_outer (after): %d\n", a);
}

extern "C" char *handle(char *msg, char *env) {
  // Some stack variables
  int a = 123;
  int b = atoi(env);
  int c = atoi(msg);

  LOG("handle (before): %d\n", a);
  if (c == 1) {
    func_outer(b);
  } else if (c == 2) {
    try {
      func_outer(b);
    } catch (int e) {
      LOG("handle (catch int): e:%d, a:%d\n", e, a);
    } catch (...) {
      LOG("handle (catch all): a:%d\n", a);
    }
  } else {
    LOG("handle (do nothing): %d\n", a);
  }
  LOG("handle (after): %d\n", a);

  char *result = (char *)malloc(256);
  strcpy(result, "Done");
  return result;
}

int main(int argc, char *argv[]) {
  LOG("C++ - try/catch with stack example\n");
  handle("0", "0");
  LOG("Done\n");
  return 0;
}
