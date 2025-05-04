#include <cstdio>

using namespace std;

unsigned long fib(unsigned long i) {
  if (i <= 1) {
    return i;
  }
  return fib(i - 1) + fib(i - 2);
}

char result_str[256] = "\0";

extern "C" char *handle(char *msg, char *env) {
  // fprintf(stderr, "handle\n");
  unsigned long n = 10;
  // fprintf(stderr, "n: %ld\n", n);
  unsigned long result = fib(n);
  // fprintf(stderr, "fib: %ld\n", result);

  // write result string to result
  snprintf(result_str, sizeof(result_str), "Fibonacci index %ld is %ld\n", n, result);
  // fprintf(stderr, "result_str: %s", result_str);
  return result_str;
}

int main(int argc, char *argv[]) {
  printf("C++ - Fibonacci sequence example\n");
  handle(NULL, NULL);
  printf("result_str: %s", result_str);
  return 0;
}
