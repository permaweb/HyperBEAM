#include <stdint.h>
#include <stdio.h>
#include <float.h>

#define BUFFER_SIZE 1024

char I32_BUFFER[BUFFER_SIZE];
char *format_i32(int32_t value) {
    sprintf(I32_BUFFER, "%d", value);
    return I32_BUFFER;
}

char U32_BUFFER[BUFFER_SIZE];
char *format_u32(uint32_t value) {
    sprintf(U32_BUFFER, "%u", value);
    return U32_BUFFER;
}

char I64_BUFFER[BUFFER_SIZE];
char *format_i64(int64_t value) {
    sprintf(I64_BUFFER, "%lld", value);
    return I64_BUFFER;
}

char U64_BUFFER[BUFFER_SIZE];
char *format_u64(uint64_t value) {
    sprintf(U64_BUFFER, "%llu", value);
    return U64_BUFFER;
}

char F32_BUFFER[BUFFER_SIZE];
char *format_f32(float value) {
    sprintf(F32_BUFFER, "%f", value);
    return F32_BUFFER;
}

char F64_BUFFER[BUFFER_SIZE];
char *format_f64(double value) {
    sprintf(F64_BUFFER, "%lf", value);
    return F64_BUFFER;
}

int main() {
    // For each format function print out highest allowed, lowest allowed, and one outside of each
    format_i32(0);
    printf("INT32_0: %s\n", I32_BUFFER);
    format_i32(INT32_MAX);
    printf("INT32_MAX: %s\n", I32_BUFFER);
    format_i32(INT32_MIN);
    printf("INT32_MIN: %s\n", I32_BUFFER);

    format_u32(0);
    printf("UINT32_0: %s\n", U32_BUFFER);
    format_u32(UINT32_MAX);
    printf("UINT32_MAX: %s\n", U32_BUFFER);

    format_u64(0);
    printf("UINT64_0: %s\n", U64_BUFFER);
    format_i64(INT64_MAX);
    printf("INT64_MAX: %s\n", I64_BUFFER);
    format_i64(INT64_MIN);
    printf("INT64_MIN: %s\n", I64_BUFFER);

    format_u64(0);
    printf("UINT64_0: %s\n", U64_BUFFER);
    format_u64(UINT64_MAX);
    printf("UINT64_MAX: %s\n", U64_BUFFER);

    format_f32(0);
    printf("FLT_0: %s\n", F32_BUFFER);
    format_f32(FLT_MAX);
    printf("FLT_MAX: %s\n", F32_BUFFER);
    format_f32(FLT_MIN);
    printf("FLT_MIN: %s\n", F32_BUFFER);

    format_f64(0);
    printf("DBL_0: %s\n", F64_BUFFER);
    format_f64(DBL_MAX);
    printf("DBL_MAX: %s\n", F64_BUFFER);
    format_f64(DBL_MIN);
    printf("DBL_MIN: %s\n", F64_BUFFER);
}
