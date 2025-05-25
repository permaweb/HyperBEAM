#ifndef HB_TEST_UTILS_H
#define HB_TEST_UTILS_H

#include <stddef.h> // For size_t
#include <stdint.h> // For uint32_t

/**
 * @brief Reads the entire content of a file into a buffer.
 *
 * @param filename The path to the file to read.
 * @param p_size Pointer to store the size of the buffer read.
 * @return A pointer to the allocated buffer (uint8_t*) containing the file content, 
 *         or NULL if an error occurred. The caller is responsible for
 *         freeing this buffer using free_buffer().
 */
uint8_t* read_file_to_buffer(const char *filename, uint32_t *p_size);

/**
 * @brief Frees the buffer allocated by read_file_to_buffer.
 *
 * @param buffer The buffer to free.
 */
void free_buffer(void *buffer);

#endif // HB_TEST_UTILS_H 