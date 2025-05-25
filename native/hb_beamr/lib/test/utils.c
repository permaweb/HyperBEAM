#include "utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h> // For uint8_t

uint8_t* read_file_to_buffer(const char *filename, uint32_t *p_size) {
    FILE *file = NULL;
    uint8_t *buffer = NULL;
    long file_size = 0;
    size_t bytes_read = 0;

    if (!filename || !p_size) {
        return NULL;
    }

    file = fopen(filename, "rb");
    if (!file) {
        perror("Error opening file");
        return NULL;
    }

    fseek(file, 0, SEEK_END);
    file_size = ftell(file);
    rewind(file);

    if (file_size < 0) {
         perror("Error getting file size");
         fclose(file);
         return NULL;
    }

    buffer = (uint8_t *)malloc(file_size); // No +1 needed for binary data
    if (!buffer) {
        fprintf(stderr, "Error allocating memory for file buffer\n");
        fclose(file);
        return NULL;
    }

    bytes_read = fread(buffer, 1, file_size, file);
    if (bytes_read != (size_t)file_size) {
        fprintf(stderr, "Error reading file (read %zu, expected %ld)\n", bytes_read, file_size);
        free(buffer);
        fclose(file);
        return NULL;
    }

    *p_size = (uint32_t)file_size;

    fclose(file);
    return buffer;
}

void free_buffer(void *buffer) {
    if (buffer) {
        free(buffer);
    }
} 