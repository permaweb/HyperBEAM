// #define LOG_ENABLED
#include "log.h"

#include <csetjmp>
#include <cstdio>
#include <cstring>

char result_str[256] = "Initial";
int last_level = -1;

jmp_buf env;

void deep_function(int level)
{
    last_level = level;
    if (level == 3) {                       /* Pick an arbitrary depth…  */
        LOG("!  deep_function() hit the trigger … jumping out!");
        longjmp(env, 42);                   /* Jump back, return value = 42 */
    }
    LOG("  > deep_function(level=%d)\n", level);
    deep_function(level + 1);               /* Recurse one level deeper  */
    /* ⚠️ This line is never reached when longjmp fires */
    LOG("  < deep_function(level=%d) returns normally\n", level);
}

extern "C" char *handle(char *msg, char *env_str) {
  LOG("printf: handle()\n");
  LOG("msg: %s\n", msg);
  LOG("env_str: %s\n", env_str);

  volatile int local_state = 7;           /* Must be volatile if you rely on its value after longjmp */

  int ret = setjmp(env);                  /* Save context → returns 0 first time */
  if (ret == 0) {
    LOG("First time through main() after setjmp.");
    deep_function(0);                   /* Dive into the call tree   */
    LOG("This line is skipped because deep_function longjmps.");
  } else {
    /* Execution resumes here after longjmp, with ret carrying the argument (42) */
    LOG("Back in main() via longjmp. ret = %d, local_state = %d\n",
            ret, local_state);
    sprintf(result_str, "last_level: %d, ret: %d, local_state: %d", last_level, ret, local_state);
  }

  LOG("Program continues normally after the non-local jump.");

  LOG("result_str: %s\n", result_str);
  return result_str;
}

int main(int argc, char *argv[]) {
  LOG("C++ - setjmp/longjmp example\n");
  handle(NULL, "0");
  LOG("Done\n");
  return 0;
}
