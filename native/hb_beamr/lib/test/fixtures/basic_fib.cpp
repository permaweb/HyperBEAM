// Standard C/C++ include, not strictly necessary for this simple Wasm module
// but good practice if more complex C++ features were used.
// #include <vector>

#include <cstdio>
#include <emscripten.h> // For EMSCRIPTEN_KEEPALIVE

using namespace std;

#ifdef __cplusplus
extern "C" {
#endif

// EMSCRIPTEN_KEEPALIVE is not strictly needed if functions are listed in EXPORTED_FUNCTIONS
// but can be useful for C functions intended to be callable from JS/Wasm.
// #include <emscripten.h>
// EMSCRIPTEN_KEEPALIVE
EMSCRIPTEN_KEEPALIVE
int fib(int n) {
    if (n <= 0) {
        return 0;
    }
    if (n == 1) {
        return 1;
    }
    int a = 0, b = 1, c;
    for (int i = 2; i <= n; ++i) {
        c = a + b;
        a = b;
        b = c;
    }
    return b;
}

#ifdef __cplusplus
}
#endif

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
