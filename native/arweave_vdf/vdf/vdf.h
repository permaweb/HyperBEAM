#ifndef VDF_H
#define VDF_H

#include <stdbool.h>

const int SALT_SIZE = 32;
const int VDF_SHA_HASH_SIZE = 32;

static inline void long_add(unsigned char* saltBuffer, int checkpointIdx) {
	unsigned int acc = checkpointIdx;
	// big endian from erlang
	for(int i=SALT_SIZE-1;i>=0;i--) {
		unsigned int value = saltBuffer[i];
		value += acc;
		saltBuffer[i] = value & 0xFF;
		acc = value >> 8;
		if (acc == 0) break;
	}
}

#if defined(__cplusplus)
extern "C" {
#endif

// out checkpoint should return all checkpoints including skipCheckpointCount
void vdf_sha2(unsigned char* saltBuffer, unsigned char* seed, unsigned char* out, unsigned char* outCheckpoint, int checkpointCount, int skipCheckpointCount, int hashingIterations);
void vdf_sha2_fused_x86(unsigned char* saltBuffer, unsigned char* seed, unsigned char* out, unsigned char* outCheckpoint, int checkpointCount, int skipCheckpointCount, int hashingIterations);
void vdf_sha2_fused_arm(unsigned char* saltBuffer, unsigned char* seed, unsigned char* out, unsigned char* outCheckpoint, int checkpointCount, int skipCheckpointCount, int hashingIterations);
void vdf_sha2_hiopt_arm(unsigned char* saltBuffer, unsigned char* seed, unsigned char* out, unsigned char* outCheckpoint, int checkpointCount, int skipCheckpointCount, int hashingIterations);
bool vdf_parallel_sha_verify_with_reset(unsigned char* startSaltBuffer, unsigned char* seed, int checkpointCount, int skipCheckpointCount, int hashingIterations, unsigned char* inRes, unsigned char* inCheckpoint, unsigned char* outCheckpoint, unsigned char* resetSalt, unsigned char* resetSeed, int maxThreadCount);

/* VENDOR: upstream reaches the hardware SHA-256 kernels only from the three
   vdf_sha2*_nif computation entry points. The verification driver in vdf.cpp
   calls _vdf_sha2 -- the OpenSSL kernel -- directly, so a node that only ever
   validates never runs them. The declarations below let ar_vdf_nif.c hand
   vdf.cpp the kernel its load-time architecture probe already selected.

   The underscored entry points, not the public vdf_sha2* wrappers, are the
   ones the driver needs: they advance saltBuffer in place, which it relies on
   ("NOTE long_add included"), while the wrappers copy it to the stack. */
typedef void (*vdf_sha2_fn)(unsigned char* saltBuffer, unsigned char* seed, unsigned char* out, unsigned char* outCheckpoint, int checkpointCount, int skipCheckpointCount, int hashingIterations);
void _vdf_sha2(unsigned char* saltBuffer, unsigned char* seed, unsigned char* out, unsigned char* outCheckpoint, int checkpointCount, int skipCheckpointCount, int hashingIterations);
void _vdf_sha2_fused_x86(unsigned char* saltBuffer, unsigned char* seed, unsigned char* out, unsigned char* outCheckpoint, int checkpointCount, int skipCheckpointCount, int hashingIterations);
void _vdf_sha2_fused_arm(unsigned char* saltBuffer, unsigned char* seed, unsigned char* out, unsigned char* outCheckpoint, int checkpointCount, int skipCheckpointCount, int hashingIterations);
void _vdf_sha2_hiopt_arm(unsigned char* saltBuffer, unsigned char* seed, unsigned char* out, unsigned char* outCheckpoint, int checkpointCount, int skipCheckpointCount, int hashingIterations);
/* Install the kernel the verification driver hashes with. Returns false and
   leaves _vdf_sha2 installed if the candidate disagrees with it on the
   known-answer test, so a machine whose fast kernel is wrong -- or whose
   feature probe lied -- validates exactly as it does today. */
bool vdf_set_verify_sha2(vdf_sha2_fn kernel);

#if defined(__cplusplus)
}
#endif


#endif
