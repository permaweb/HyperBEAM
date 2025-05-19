// #define LOG_ENABLED
#include "log.h"

#include <cstdio>
#include <cstring>

using namespace std;

unsigned long fib(unsigned long i) {
  if (i <= 1) {
    return i;
  }
  return fib(i - 1) + fib(i - 2);
}

char result_str[256] = "\0";

extern "C" char *handle(char *msg, char *env) {
  LOG("handle\n");
  unsigned long n = 10;
  LOG("n: %ld\n", n);
  unsigned long result = fib(n);
  LOG("fib: %ld\n", result);

  // write result string to result
  sprintf(result_str, "Fibonacci index %ld is %ld\n", n, result);
  LOG("result_str: %s\n", result_str);
  return result_str;
}

int main(int argc, char *argv[]) {
  LOG("C++ - Fibonacci sequence example\n");
  handle(NULL, "1");
  LOG("result_str: %s\n", result_str);
  return 0;
}
