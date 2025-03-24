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

void func_inner() {
  // Some stack variables
  int a = 789;

  LOG("func_inner (before): %d\n", a);
  
  throw(42); // std::runtime_error("Exception");

  LOG("func_inner (unreachable): %d\n", a);
}

void func_outer() {
  // Some stack variables
  int a = 456;

  LOG("func_outer (before): %d\n", a);
  try {
    LOG("func_outer (try): %d\n", a);
    func_inner();
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

  LOG("handle (before): %d\n", a);
  func_outer();
  LOG("handle (after): %d\n", a);

  char *result = (char *)malloc(256);
  strcpy(result, "Done");
  return result;
}

int main(int argc, char *argv[]) {
  LOG("C++ - try/catch with stack example\n");
  handle(NULL, NULL);
  LOG("Done\n");
  return 0;
}
