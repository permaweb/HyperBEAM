// Measurement calculation functions for SEV-SNP launch digest
// This implements the algorithm from the SEV crate's snp_calc_launch_digest

#include "dev_snp_nif.h"
#include <openssl/sha.h>
#include <openssl/evp.h>
#include <string.h>
#include <stdint.h>

#define LD_BYTES 48  // Launch digest size (SHA-384 = 48 bytes)
#define PAGE_SIZE 4096
#define VMSA_GPA 0xFFFFFFFFF000ULL

// Page types
#define PAGE_TYPE_NORMAL 0x01
#define PAGE_TYPE_VMSA 0x02
#define PAGE_TYPE_ZERO 0x03
#define PAGE_TYPE_UNMEASURED 0x04
#define PAGE_TYPE_SECRETS 0x05
#define PAGE_TYPE_CPUID 0x06

// Guest Context structure
typedef struct {
    unsigned char ld[LD_BYTES];  // Launch digest (SHA-384)
} gctx_t;

// Initialize GCTX with zeros
static void gctx_init(gctx_t *gctx) {
    memset(gctx->ld, 0, LD_BYTES);
}

// Initialize GCTX with seed (OVMF hash)
static int gctx_init_with_seed(gctx_t *gctx, const unsigned char *seed, size_t seed_len) {
    if (seed_len != LD_BYTES) {
        return -1;
    }
    memcpy(gctx->ld, seed, LD_BYTES);
    return 0;
}

// Update launch digest with page data
// This implements the Gctx::update algorithm from the SEV crate
static int gctx_update_page(gctx_t *gctx, uint8_t page_type, uint64_t gpa,
                           const unsigned char *contents, size_t contents_len) {
    uint16_t page_info_len = 0x70;  // 112 bytes
    uint8_t is_imi = 0;
    uint8_t vmpl3_perms = 0;
    uint8_t vmpl2_perms = 0;
    uint8_t vmpl1_perms = 0;
    
    // Build page_info structure
    unsigned char page_info[0x70];
    size_t pos = 0;
    
    // Copy current launch digest
    memcpy(page_info + pos, gctx->ld, LD_BYTES);
    pos += LD_BYTES;
    
    // Copy page contents (or hash if it's a full page)
    if (contents && contents_len > 0) {
        if (contents_len == PAGE_SIZE && page_type == PAGE_TYPE_NORMAL) {
            // Hash the page contents using EVP API
            unsigned char page_hash[SHA384_DIGEST_LENGTH];
            EVP_MD_CTX *md_ctx = EVP_MD_CTX_new();
            if (!md_ctx) return -1;
            
            const EVP_MD *md = EVP_sha384();
            if (EVP_DigestInit_ex(md_ctx, md, NULL) == 1 &&
                EVP_DigestUpdate(md_ctx, contents, contents_len) == 1) {
                unsigned int hash_len = SHA384_DIGEST_LENGTH;
                if (EVP_DigestFinal_ex(md_ctx, page_hash, &hash_len) == 1) {
                    memcpy(page_info + pos, page_hash, SHA384_DIGEST_LENGTH);
                    pos += SHA384_DIGEST_LENGTH;
                }
            }
            EVP_MD_CTX_free(md_ctx);
        } else {
            memcpy(page_info + pos, contents, contents_len);
            pos += contents_len;
        }
    }
    
    // Append page_info_len (little-endian)
    page_info[pos++] = (uint8_t)(page_info_len & 0xFF);
    page_info[pos++] = (uint8_t)((page_info_len >> 8) & 0xFF);
    
    // Append page_type
    page_info[pos++] = page_type;
    
    // Append is_imi
    page_info[pos++] = is_imi;
    
    // Append VMPL permissions
    page_info[pos++] = vmpl3_perms;
    page_info[pos++] = vmpl2_perms;
    page_info[pos++] = vmpl1_perms;
    page_info[pos++] = 0;  // Reserved
    
    // Append GPA (little-endian, 8 bytes)
    for (int i = 0; i < 8; i++) {
        page_info[pos++] = (uint8_t)((gpa >> (i * 8)) & 0xFF);
    }
    
    // Verify we have exactly page_info_len bytes
    if (pos != page_info_len) {
        return -1;
    }
    
    // Hash the page_info to get new launch digest
    // Use OpenSSL 3.0 EVP API instead of deprecated SHA384_* functions
    EVP_MD_CTX *md_ctx = EVP_MD_CTX_new();
    if (!md_ctx) return -1;
    
    const EVP_MD *md = EVP_sha384();
    if (EVP_DigestInit_ex(md_ctx, md, NULL) != 1) {
        EVP_MD_CTX_free(md_ctx);
        return -1;
    }
    
    if (EVP_DigestUpdate(md_ctx, page_info, page_info_len) != 1) {
        EVP_MD_CTX_free(md_ctx);
        return -1;
    }
    
    unsigned int digest_len = LD_BYTES;
    if (EVP_DigestFinal_ex(md_ctx, gctx->ld, &digest_len) != 1) {
        EVP_MD_CTX_free(md_ctx);
        return -1;
    }
    
    EVP_MD_CTX_free(md_ctx);
    
    return 0;
}

// Compute launch digest - this is a framework that needs to be completed
// The full algorithm requires:
// 1. OVMF parsing and page updates
// 2. VMSA structure creation
// 3. Metadata page handling
// 4. Complex page update logic
int compute_launch_digest(
    uint32_t vcpus,
    uint8_t vcpu_type,
    uint8_t vmm_type,
    uint64_t guest_features,
    const char *ovmf_hash_hex,  // SHA-384 hash as hex string
    const unsigned char *kernel_hash,  // 32 bytes
    const unsigned char *initrd_hash,  // 32 bytes
    const unsigned char *append_hash,  // 32 bytes
    unsigned char *output_digest  // 48 bytes output
) {
    gctx_t gctx;
    
    // Initialize GCTX with OVMF hash if provided
    if (ovmf_hash_hex && strlen(ovmf_hash_hex) == 96) {  // 48 bytes * 2 hex chars
        // Convert hex to binary
        unsigned char ovmf_hash[LD_BYTES];
        for (int i = 0; i < LD_BYTES; i++) {
            char hex_byte[3] = {ovmf_hash_hex[i*2], ovmf_hash_hex[i*2+1], 0};
            ovmf_hash[i] = (unsigned char)strtoul(hex_byte, NULL, 16);
        }
        if (gctx_init_with_seed(&gctx, ovmf_hash, LD_BYTES) != 0) {
            return -1;
        }
    } else {
        gctx_init(&gctx);
        // TODO: Load and process OVMF file if provided
    }
    
    // TODO: Update with kernel hashes (SEV hashes table)
    // TODO: Update metadata pages
    // TODO: Create and update VMSA pages
    
    // For now, return the current launch digest
    // This is a placeholder - the full implementation requires
    // porting the entire SEV crate measurement algorithm
    memcpy(output_digest, gctx.ld, LD_BYTES);
    
    return 0;
}

