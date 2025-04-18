#ifndef HB_LOGGING_H
#define HB_LOGGING_H

#include "hb_core.h"
// Enable debug logging by default if not defined
#define HB_DEBUG 0
#ifndef HB_DEBUG
#endif


#define DRV_DEBUG(format, ...) beamr_print(HB_DEBUG, __FILE__, __LINE__, format, ##__VA_ARGS__)
#define DRV_PRINT(format, ...) beamr_print(1, __FILE__, __LINE__, format, ##__VA_ARGS__)

/*
 * Function: beamr_print
 * --------------------
 * This function prints a formatted message to the standard output, prefixed with the thread
 * ID, file name, and line number where the log was generated.
 * 
 *  print: A flag that controls whether the message is printed (1 to print, 0 to skip).
 *  file: The source file name where the log was generated.
 *  line: The line number where the log was generated.
 *  format: The format string for the message.
 *  ...: The variables to be printed in the format.
 */
void beamr_print(int print, const char* file, int line, const char* format, ...);

/*
 * Function: send_error
 * --------------------
 * This function sends an error message to the Erlang process, formatted according to the provided
 * message format and arguments. The message is also logged using the DRV_DEBUG macro.
 * 
 *  proc: The process to send the error message to.
 *  message_fmt: The format string for the error message.
 *  ...: The variables to be printed in the error message.
 */
void send_error(Proc* proc, const char* message_fmt, ...);

#endif // HB_LOGGING_H
