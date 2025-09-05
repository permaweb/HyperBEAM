/*
 * Copyright (C) 2019 Intel Corporation.  All rights reserved.
 * SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
 */

 #ifndef WASI_NN_TYPES_H
 #define WASI_NN_TYPES_H
 
#include <stdint.h>
#include <stdbool.h>

 #ifdef __cplusplus
 extern "C" {
 #endif
 
 /**
  * ERRORS
  *
  */
 
 // sync up with
 // https://github.com/WebAssembly/wasi-nn/blob/main/wit/wasi-nn.wit#L136 Error
 // codes returned by functions in this API.
 typedef enum {
	 // No error occurred.
	 success = 0,
	 // Caller module passed an invalid argument.
	 invalid_argument,
	 // Invalid encoding.
	 invalid_encoding,
	 // The operation timed out.
	 timeout,
	 // Runtime Error.
	 runtime_error,
	 // Unsupported operation.
	 unsupported_operation,
	 // Graph is too large.
	 too_large,
	 // Graph not found.
	 not_found,
	 // The operation is insecure or has insufficient privilege to be performed.
	 // e.g., cannot access a hardware feature requested
	 security,
	 // The operation failed for an unspecified reason.
	 unknown,
	 // for WasmEdge-wasi-nn
	 end_of_sequence = 100,  // End of Sequence Found.
	 context_full = 101,     // Context Full.
	 prompt_tool_long = 102, // Prompt Too Long.
	 model_not_found = 103,  // Model Not Found.
 } wasi_nn_error;
 
 /**
  * TENSOR
  *
  */
 
 // The dimensions of a tensor.
 //
 // The array length matches the tensor rank and each element in the array
 // describes the size of each dimension.
 typedef struct {
	 uint32_t *buf;
	 uint32_t size;
 } tensor_dimensions;
 
 #if WASM_ENABLE_WASI_EPHEMERAL_NN != 0
 // sync up with
 // https://github.com/WebAssembly/wasi-nn/blob/main/wit/wasi-nn.wit#L27
 // The type of the elements in a tensor.
 typedef enum { fp16 = 0, fp32, fp64, bf16, u8, i32, i64 } tensor_type;
 #else
 typedef enum { fp16 = 0, fp32, up8, ip32 } tensor_type;
 #endif /* WASM_ENABLE_WASI_EPHEMERAL_NN != 0 */
 
 // The tensor data.
 //
 // Initially conceived as a sparse representation, each empty cell would be
 // filled with zeros and the array length must match the product of all of the
 // dimensions and the number of bytes in the type (e.g., a 2x2 tensor with
 // 4-byte f32 elements would have a data array of length 16). Naturally, this
 // representation requires some knowledge of how to lay out data in
 // memory--e.g., using row-major ordering--and could perhaps be improved.
 typedef uint8_t *tensor_data;
 
 // A tensor.
 typedef struct {
	 // Describe the size of the tensor (e.g., 2x2x2x2 -> [2, 2, 2, 2]). To
	 // represent a tensor containing a single value, use `[1]` for the tensor
	 // dimensions.
	 tensor_dimensions *dimensions;
	 // Describe the type of element in the tensor (e.g., f32).
	 tensor_type type;
	 // Contains the tensor data.
	 tensor_data data;
 } tensor;
 
 /**
  * GRAPH
  *
  */
 
 // The graph initialization data.
 //
 // This consists of an array of buffers because implementing backends may encode
 // their graph IR in parts (e.g., OpenVINO stores its IR and weights
 // separately).
 typedef struct {
	 uint8_t *buf;
	 uint32_t size;
 } graph_builder;
 
 typedef struct {
	 graph_builder *buf;
	 uint32_t size;
 } graph_builder_array;
 
 // An execution graph for performing inference (i.e., a model).
 typedef uint32_t graph;
 
 // sync up with
 // https://github.com/WebAssembly/wasi-nn/blob/main/wit/wasi-nn.wit#L75
 // Describes the encoding of the graph. This allows the API to be implemented by
 // various backends that encode (i.e., serialize) their graph IR with different
 // formats.
 typedef enum {
	 openvino = 0,
	 onnx,
	 tensorflow,
	 pytorch,
	 tensorflowlite,
	 ggml,
	 autodetect,
	 unknown_backend,
 } graph_encoding;
 
 // Define where the graph should be executed.
 typedef enum execution_target { cpu = 0, gpu, tpu } execution_target;
 
 // Bind a `graph` to the input and output tensors for an inference.
 typedef uint32_t graph_execution_context;
 

 __attribute__((visibility("default"))) wasi_nn_error
 init_backend(void **ctx) ;

 __attribute__((visibility("default"))) wasi_nn_error
 init_backend_with_config(void **ctx, const char *config, uint32_t config_len);
 
 __attribute__((visibility("default"))) wasi_nn_error
 deinit_backend(void *ctx);

 __attribute__((visibility("default"))) wasi_nn_error
 load_by_name_with_config(void *ctx, const char *filename, uint32_t filename_len,
	const char *config, uint32_t config_len, graph *g);

 __attribute__((visibility("default"))) wasi_nn_error
 init_execution_context(void *ctx, const char *session_id, graph_execution_context *exec_ctx);

 __attribute__((visibility("default"))) wasi_nn_error
 close_execution_context(void *ctx, graph_execution_context exec_ctx);
 
 __attribute__((visibility("default"))) wasi_nn_error
 run_inference(void *ctx, graph_execution_context exec_ctx, uint32_t index,
		   tensor *input_tensor,tensor_data output_tensor, uint32_t *output_tensor_size, const char *options);

 __attribute__((visibility("default"))) wasi_nn_error
 set_input(void *ctx, graph_execution_context exec_ctx, uint32_t index,
	  tensor *wasi_nn_tensor);
 
 __attribute__((visibility("default"))) wasi_nn_error
 compute(void *ctx, graph_execution_context exec_ctx);
 
 __attribute__((visibility("default"))) wasi_nn_error
 get_output(void *ctx, graph_execution_context exec_ctx, uint32_t index,
	  tensor_data output_tensor, uint32_t *output_tensor_size);

 
 #ifdef __cplusplus
 }
 #endif
 #endif
 