#include <stdio.h>
#include <stdint.h>

volatile char GLOBAL_BUFFER[1024];

volatile char *global_buffer() {
    return GLOBAL_BUFFER;
}

int32_t parse_i32(char *value) {
    int32_t result;
    sscanf(value, "%d", &result);
    return result;
}

uint32_t parse_u32(char *value) {
    uint32_t result;
    sscanf(value, "%u", &result);
    return result;
}

int64_t parse_i64(char *value) {
    int64_t result;
    sscanf(value, "%lld", &result);
    return result;
}

uint64_t parse_u64(char *value) {
    uint64_t result;
    sscanf(value, "%llu", &result);
    return result;
}

float parse_f32(char *value) {
    float result;
    sscanf(value, "%f", &result);
    return result;
}

double parse_f64(char *value) {
    double result;
    sscanf(value, "%lf", &result);
    return result;
}

int main() {
    // For each parse function print out zero, highest allowed, lowest allowed
    int32_t res_i32;
    uint32_t res_u32;
    int64_t res_i64;
    uint64_t res_u64;
    float res_f32;
    double res_f64;

    parse_i32("0");
    printf("INT32_0: %d\n", res_i32);
    parse_i32("2147483647");
    printf("INT32_MAX: %d\n", res_i32);
    parse_i32("-2147483648");
    printf("INT32_MIN: %d\n", res_i32);

    parse_u32("0");
    printf("UINT32_0: %u\n", res_u32);
    parse_u32("4294967295");
    printf("UINT32_MAX: %u\n", res_u32);

    parse_i64("0");
    printf("INT64_0: %lld\n", res_i64);
    parse_i64("9223372036854775807");
    printf("INT64_MAX: %lld\n", res_i64);
    parse_i64("-9223372036854775808");
    printf("INT64_MIN: %lld\n", res_i64);

    parse_u64("0");
    printf("UINT64_0: %llu\n", res_u64);
    parse_u64("18446744073709551615");
    printf("UINT64_MAX: %llu\n", res_u64);

    parse_f32("0");
    printf("FLOAT_0: %f\n", res_f32);
    parse_f32("3.40282346638528859811704183484516925440e+38");
    printf("FLOAT_MAX: %f\n", res_f32);
    parse_f32("-3.40282346638528859811704183484516925440e+38");
    printf("FLOAT_MIN: %f\n", res_f32);

    parse_f64("0");
    printf("DOUBLE_0: %lf\n", res_f64);
    parse_f64("1.797693134862315708145274237317043567981e+308");
    printf("DOUBLE_MAX: %lf\n", res_f64);
    parse_f64("-1.797693134862315708145274237317043567981e+308");
    printf("DOUBLE_MIN: %lf\n", res_f64);
}
