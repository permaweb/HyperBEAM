#include "webgpu.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "wgpu.h"
extern WGPUBool wgpuDevicePoll(WGPUDevice device, WGPUBool wait, WGPU_NULLABLE WGPUWrappedSubmissionIndex const * wrappedSubmissionIndex);

// Define UNUSED macro for unused parameters
#define UNUSED(x) (void)(x);
// LOG does nothing
#define LOG(...) {}
#define assert(x) \
    do { \
        if (!(x)) { \
            LOG("assert failed: %s\n", #x); \
            abort(); \
        } else { \
            LOG("assert passed: %s\n", #x); \
        } \
    } while (0);

#define LOG_PREFIX "[compute]"

#define QUOTE(...) #__VA_ARGS__
const char *shader_src = "\
@group(0)\
@binding(0)\
var<storage, read_write> v_indices: array<u32>;\
\
@compute\
@workgroup_size(1, 1, 1)\
fn main(\
    @builtin(global_invocation_id) global_id: vec3<u32>,\
    @builtin(num_workgroups) gridDim : vec3<u32>,\
) {\
    if (gridDim.y != 1 || gridDim.z != 1) { return ; }\
    v_indices[global_id.x] = v_indices[global_id.x] * 2u;\
}\
";

static void handle_request_adapter(WGPURequestAdapterStatus status,
                                   WGPUAdapter adapter, char const *message,
                                   void *userdata) {
  UNUSED(status)
  UNUSED(message)
  *(WGPUAdapter *)userdata = adapter;
}
static void handle_request_device(WGPURequestDeviceStatus status,
                                  WGPUDevice device, char const *message,
                                  void *userdata) {
  UNUSED(status)
  UNUSED(message)
  *(WGPUDevice *)userdata = device;
}
static void handle_buffer_map(WGPUBufferMapAsyncStatus status, void *userdata) {
  UNUSED(userdata)
  LOG(LOG_PREFIX " buffer_map status=%#.8x\n", status);
}

char* handle(char *env, char *msg) {
  UNUSED(env)
  UNUSED(msg)

  uint32_t numbers[] = {1, 2, 3, 100, 200, 300};
  uint32_t numbers_size = sizeof(numbers);
  uint32_t numbers_length = numbers_size / sizeof(uint32_t);

  LOG("wgpuCreateInstance\n");
  WGPUInstance instance = wgpuCreateInstance(NULL);
  assert(instance);

  LOG("wgpuInstanceRequestAdapter\n");
  WGPUAdapter adapter = NULL;
  wgpuInstanceRequestAdapter(instance, NULL, handle_request_adapter,
                             (void *)&adapter);
  LOG("wgpuInstanceRequestAdapter end\n");
  assert(adapter);

  LOG("wgpuAdapterRequestDevice\n");
  WGPUDevice device = NULL;
  wgpuAdapterRequestDevice(adapter, NULL, handle_request_device,
                           (void *)&device);
  assert(device);
  
  LOG("wgpuDeviceGetQueue\n");
  WGPUQueue queue = wgpuDeviceGetQueue(device);
  assert(queue);

  LOG("wgpuDeviceCreateShaderModule\n");
  WGPUShaderModule shader_module = wgpuDeviceCreateShaderModule(
      device, &(const WGPUShaderModuleDescriptor){
                  .label = "shader.wgsl",
                  .nextInChain =
                      (const WGPUChainedStruct *)&(
                          const WGPUShaderModuleWGSLDescriptor){
                          .chain =
                              (const WGPUChainedStruct){
                                  .sType = WGPUSType_ShaderModuleWGSLDescriptor,
                              },
                          .code = shader_src,
                      },
              });
  assert(shader_module);

  LOG("wgpuDeviceCreateBuffer\n");
  WGPUBuffer staging_buffer = wgpuDeviceCreateBuffer(
      device, &(const WGPUBufferDescriptor){
                  .label = "staging_buffer",
                  .usage = WGPUBufferUsage_MapRead | WGPUBufferUsage_CopyDst,
                  .size = numbers_size,
                  .mappedAtCreation = false,
              });
  assert(staging_buffer);

  LOG("wgpuDeviceCreateBuffer\n");
  WGPUBuffer storage_buffer = wgpuDeviceCreateBuffer(
      device, &(const WGPUBufferDescriptor){
                  .label = "storage_buffer",
                  .usage = WGPUBufferUsage_Storage | WGPUBufferUsage_CopyDst |
                           WGPUBufferUsage_CopySrc,
                  .size = numbers_size,
                  .mappedAtCreation = false,
              });
  assert(storage_buffer);

  LOG("wgpuDeviceCreateComputePipeline\n");
  WGPUComputePipeline compute_pipeline = wgpuDeviceCreateComputePipeline(
      device, &(const WGPUComputePipelineDescriptor){
                  .label = "compute_pipeline",
                  .compute =
                      (const WGPUProgrammableStageDescriptor){
                          .module = shader_module,
                          .entryPoint = "main",
                      },
              });
  assert(compute_pipeline);

  LOG("wgpuComputePipelineGetBindGroupLayout\n");
  WGPUBindGroupLayout bind_group_layout =
      wgpuComputePipelineGetBindGroupLayout(compute_pipeline, 0);
  assert(bind_group_layout);

  LOG("wgpuDeviceCreateBindGroup\n");
  WGPUBindGroup bind_group = wgpuDeviceCreateBindGroup(
      device, &(const WGPUBindGroupDescriptor){
                  .label = "bind_group",
                  .layout = bind_group_layout,
                  .entryCount = 1,
                  .entries =
                      (const WGPUBindGroupEntry[]){
                          (const WGPUBindGroupEntry){
                              .binding = 0,
                              .buffer = storage_buffer,
                              .offset = 0,
                              .size = numbers_size,
                          },
                      },
              });
  assert(bind_group);

  LOG("wgpuDeviceCreateCommandEncoder\n");
  WGPUCommandEncoder command_encoder = wgpuDeviceCreateCommandEncoder(
      device, &(const WGPUCommandEncoderDescriptor){
                  .label = "command_encoder",
              });
  assert(command_encoder);

  LOG("wgpuCommandEncoderWriteBuffer\n");
  WGPUComputePassEncoder compute_pass_encoder =
      wgpuCommandEncoderBeginComputePass(command_encoder,
                                         &(const WGPUComputePassDescriptor){
                                             .label = "compute_pass",
                                         });
  assert(compute_pass_encoder);

  LOG("wgpuComputePassEncoderSetPipeline\n");
  wgpuComputePassEncoderSetPipeline(compute_pass_encoder, compute_pipeline);
  wgpuComputePassEncoderSetBindGroup(compute_pass_encoder, 0, bind_group, 0,
                                     NULL);
  wgpuComputePassEncoderDispatchWorkgroups(compute_pass_encoder, numbers_length,
                                           1, 1);
  wgpuComputePassEncoderEnd(compute_pass_encoder);
  wgpuComputePassEncoderRelease(compute_pass_encoder);

  LOG("wgpuCommandEncoderCopyBufferToBuffer\n");
  wgpuCommandEncoderCopyBufferToBuffer(command_encoder, storage_buffer, 0,
                                       staging_buffer, 0, numbers_size);

  WGPUCommandBuffer command_buffer = wgpuCommandEncoderFinish(
      command_encoder, &(const WGPUCommandBufferDescriptor){
                           .label = "command_buffer",
                       });
  assert(command_buffer);

  LOG("wgpuQueueWriteBuffer\n");
  wgpuQueueWriteBuffer(queue, storage_buffer, 0, &numbers, numbers_size);
  wgpuQueueSubmit(queue, 1, &command_buffer);

  LOG("wgpuBufferMapAsync\n");
  wgpuBufferMapAsync(staging_buffer, WGPUMapMode_Read, 0, numbers_size,
                     handle_buffer_map, NULL);
  wgpuDevicePoll(device, true, NULL);

  LOG("wgpuBufferGetMappedRange\n");
  uint32_t *buf =
      (uint32_t *)wgpuBufferGetMappedRange(staging_buffer, 0, numbers_size);
  assert(buf);

//   LOG("times: [%d, %d, %d, %d]\n", buf[0], buf[1], buf[2], buf[3]);
    char *out = malloc(100);
    sprintf(out, "times: [%d, %d, %d, %d, %d, %d]\n", buf[0], buf[1], buf[2], buf[3], buf[4], buf[5]);
    return out;

//   wgpuBufferUnmap(staging_buffer);
//   wgpuCommandBufferRelease(command_buffer);
//   wgpuCommandEncoderRelease(command_encoder);
//   wgpuBindGroupRelease(bind_group);
//   wgpuBindGroupLayoutRelease(bind_group_layout);
//   wgpuComputePipelineRelease(compute_pipeline);
//   wgpuBufferRelease(storage_buffer);
//   wgpuBufferRelease(staging_buffer);
//   wgpuShaderModuleRelease(shader_module);
//   wgpuQueueRelease(queue);
//   wgpuDeviceRelease(device);
//   wgpuAdapterRelease(adapter);
//   wgpuInstanceRelease(instance);
}

int main(int argc, char *argv[]) {
  LOG("main");
  return EXIT_SUCCESS;
}
