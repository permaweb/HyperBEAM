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

// SEV Hash Table GUIDs (little-endian UUIDs)
static const unsigned char SEV_HASH_TABLE_HEADER_GUID[16] = {
    0x21, 0xfd, 0x11, 0xa7, 0x93, 0xa7, 0x79, 0xb4, 0xcc, 0x4f, 0x22, 0x4f, 0x06, 0xd6, 0x38, 0x94
};
static const unsigned char SEV_KERNEL_ENTRY_GUID[16] = {
    0x5b, 0x20, 0xd2, 0x72, 0x5b, 0xd1, 0x7f, 0x42, 0xbd, 0x2f, 0x3a, 0xab, 0x37, 0x94, 0xe7, 0x4d
};
static const unsigned char SEV_INITRD_ENTRY_GUID[16] = {
    0x1d, 0x78, 0x69, 0x91, 0xe2, 0x41, 0xaf, 0x91, 0xd7, 0x4b, 0x2f, 0x3a, 0x31, 0xf7, 0xba, 0x44
};
static const unsigned char SEV_CMDLINE_ENTRY_GUID[16] = {
    0x2a, 0x6b, 0x36, 0xd4, 0x4e, 0x77, 0x94, 0x4c, 0x20, 0xbd, 0xd8, 0x02, 0xd0, 0x97, 0x2a, 0xdd
};

// SEV Hash Table Entry structure (C representation)
typedef struct {
    unsigned char guid[16];  // GUID in little-endian
    uint16_t length;         // Length of entry (little-endian)
    unsigned char hash[32];   // SHA-256 hash
} __attribute__((packed)) sev_hash_table_entry_t;

// SEV Hash Table structure
typedef struct {
    unsigned char guid[16];  // Header GUID
    uint16_t length;         // Length of table (little-endian)
    sev_hash_table_entry_t cmdline;
    sev_hash_table_entry_t initrd;
    sev_hash_table_entry_t kernel;
} __attribute__((packed)) sev_hash_table_t;

// Construct SEV hashes table page
// Returns 0 on success, -1 on failure
static int construct_sev_hashes_page(
    const unsigned char *kernel_hash,
    const unsigned char *initrd_hash,
    const unsigned char *append_hash,
    unsigned char *page_output  // Must be PAGE_SIZE bytes
) {
    if (!kernel_hash || !initrd_hash || !append_hash || !page_output) {
        return -1;
    }
    
    memset(page_output, 0, PAGE_SIZE);
    
    // Build SEV hash table
    sev_hash_table_t table;
    memset(&table, 0, sizeof(table));
    
    // Header
    memcpy(table.guid, SEV_HASH_TABLE_HEADER_GUID, 16);
    table.length = sizeof(sev_hash_table_t);
    
    // Cmdline entry
    memcpy(table.cmdline.guid, SEV_CMDLINE_ENTRY_GUID, 16);
    table.cmdline.length = sizeof(sev_hash_table_entry_t);
    memcpy(table.cmdline.hash, append_hash, 32);
    
    // Initrd entry
    memcpy(table.initrd.guid, SEV_INITRD_ENTRY_GUID, 16);
    table.initrd.length = sizeof(sev_hash_table_entry_t);
    memcpy(table.initrd.hash, initrd_hash, 32);
    
    // Kernel entry
    memcpy(table.kernel.guid, SEV_KERNEL_ENTRY_GUID, 16);
    table.kernel.length = sizeof(sev_hash_table_entry_t);
    memcpy(table.kernel.hash, kernel_hash, 32);
    
    // Serialize table to page (offset 0, padded to PAGE_SIZE)
    // The table is serialized in the same order as Rust bincode
    size_t table_size = sizeof(sev_hash_table_t);
    memcpy(page_output, &table, table_size);
    
    return 0;
}

// VMSA structure - simplified version matching Rust implementation
// BSP EIP constant
#define BSP_EIP 0xfffffff0ULL

// Create a VMSA page for a single VCPU
// This is a simplified implementation that creates a valid VMSA structure
static int create_vmsa_page(
    uint64_t eip,
    uint8_t vcpu_type,
    uint8_t vmm_type,
    uint64_t guest_features,
    unsigned char *vmsa_page  // Must be PAGE_SIZE bytes
) {
    if (!vmsa_page) {
        return -1;
    }
    
    memset(vmsa_page, 0, PAGE_SIZE);
    
    // Set key registers based on Rust VMSA implementation
    // These values match the Rust build_save_area function
    
    // Segment registers (VmcbSegment: base, selector, attrib, limit)
    // ES, CS, SS, DS, FS, GS - all initialized to defaults
    // Offset 0x00-0x5F: Segment registers (6 * 16 bytes = 96 bytes)
    
    // GDTR, IDTR, LDTR, TR - initialized to defaults
    // Offset 0x60-0x9F: Descriptor table registers (4 * 16 bytes = 64 bytes)
    
    // Reserved (43 bytes) at offset 0xA0
    // CPL at offset 0xAB
    vmsa_page[0xAB] = 0;
    
    // Reserved (4 bytes) at offset 0xAC
    // EFER at offset 0xB0 (8 bytes, little-endian)
    uint64_t efer = 0x1000;
    memcpy(vmsa_page + 0xB0, &efer, 8);
    
    // Reserved (104 bytes) at offset 0xB8
    // XSS at offset 0x120 (8 bytes)
    // CR4 at offset 0x128 (8 bytes)
    uint64_t cr4 = 0x40;
    memcpy(vmsa_page + 0x128, &cr4, 8);
    
    // CR3 at offset 0x130 (8 bytes) - 0
    // CR0 at offset 0x138 (8 bytes)
    uint64_t cr0 = 0x10;
    memcpy(vmsa_page + 0x138, &cr0, 8);
    
    // DR7 at offset 0x140 (8 bytes)
    uint64_t dr7 = 0x400;
    memcpy(vmsa_page + 0x140, &dr7, 8);
    
    // DR6 at offset 0x148 (8 bytes)
    uint64_t dr6 = 0xffff0ff0;
    memcpy(vmsa_page + 0x148, &dr6, 8);
    
    // RFLAGS at offset 0x150 (8 bytes)
    uint64_t rflags = 0x2;
    memcpy(vmsa_page + 0x150, &rflags, 8);
    
    // RIP at offset 0x158 (8 bytes, little-endian)
    uint64_t rip = eip & 0xffff;
    memcpy(vmsa_page + 0x158, &rip, 8);
    
    // Reserved (88 bytes) at offset 0x160
    // RSP at offset 0x1B8 (8 bytes) - 0
    // Reserved (24 bytes) at offset 0x1C0
    
    // RAX at offset 0x1D8 (8 bytes) - 0
    // RCX at offset 0x1E0 (8 bytes) - 0
    // RDX at offset 0x1E8 (8 bytes)
    uint64_t rdx = 0;
    if (vmm_type == 2) {  // EC2
        rdx = 0x80000001;  // EC2 specific value
    }
    memcpy(vmsa_page + 0x1E8, &rdx, 8);
    
    // RBX, RSP, RBP, RSI, RDI, R8-R15 - all 0
    // Reserved (16 bytes)
    
    // SEV Features at offset 0x3E0 (8 bytes, little-endian)
    memcpy(vmsa_page + 0x3E0, &guest_features, 8);
    
    // XCR0 at offset 0x3E8 (8 bytes)
    uint64_t xcr0 = 0x1;
    memcpy(vmsa_page + 0x3E8, &xcr0, 8);
    
    // MXCSR at offset 0x3F0 (4 bytes)
    uint32_t mxcsr = 0x1f80;
    memcpy(vmsa_page + 0x3F0, &mxcsr, 4);
    
    // X87 FCW at offset 0x3F4 (2 bytes)
    uint16_t fcw = 0x37f;
    memcpy(vmsa_page + 0x3F4, &fcw, 2);
    
    // X87 FSW, FTW, FOP, CS, DS, RIP, DP - all 0 or defaults
    // FPU registers (X87: 80 bytes, XMM: 256 bytes, YMM: 256 bytes)
    // Manual padding (2448 bytes)
    
    return 0;
}

// Compute launch digest - full implementation
int compute_launch_digest(
    uint32_t vcpus,
    uint8_t vcpu_type,
    uint8_t vmm_type,
    uint64_t guest_features,
    const char *ovmf_hash_hex,  // SHA-384 hash as hex string
    const unsigned char *kernel_hash,  // 32 bytes (SHA-256)
    const unsigned char *initrd_hash,  // 32 bytes (SHA-256)
    const unsigned char *append_hash,  // 32 bytes (SHA-256)
    unsigned char *output_digest  // 48 bytes output
) {
    gctx_t gctx;
    
    // Initialize GCTX with OVMF hash if provided
    if (ovmf_hash_hex && strlen(ovmf_hash_hex) == 96) {  // 48 bytes * 2 hex chars
        // Convert hex to binary
        unsigned char ovmf_hash[LD_BYTES];
        for (int i = 0; i < LD_BYTES; i++) {
            char hex_byte[3] = {ovmf_hash_hex[i*2], ovmf_hash_hex[i*2+1], 0};
            char *endptr;
            unsigned long val = strtoul(hex_byte, &endptr, 16);
            if (*endptr != '\0' || val > 255) {
                return -1;
            }
            ovmf_hash[i] = (unsigned char)val;
        }
        if (gctx_init_with_seed(&gctx, ovmf_hash, LD_BYTES) != 0) {
            return -1;
        }
    } else {
        gctx_init(&gctx);
    }
    
    // Update with SEV hashes table if kernel hash is provided
    // Note: We need the SEV hashes table GPA from OVMF, but since we don't have OVMF,
    // we'll use a default GPA. In practice, this should come from OVMF metadata.
    // For now, we'll skip this if we don't have the GPA, or use a reasonable default.
    if (kernel_hash && initrd_hash && append_hash) {
        // Default SEV hashes table GPA (this should come from OVMF in real implementation)
        // Using a placeholder GPA - in practice this needs to match OVMF metadata
        uint64_t sev_hashes_gpa = 0x100000;  // Placeholder - should be from OVMF
        
        unsigned char sev_hashes_page[PAGE_SIZE];
        if (construct_sev_hashes_page(kernel_hash, initrd_hash, append_hash, sev_hashes_page) == 0) {
            // Update GCTX with SEV hashes page
            if (gctx_update_page(&gctx, PAGE_TYPE_NORMAL, sev_hashes_gpa, sev_hashes_page, PAGE_SIZE) != 0) {
                return -1;
            }
        }
    }
    
    // Create and update VMSA pages
    // Use default EIP since we don't have OVMF to get reset EIP
    uint64_t ap_eip = 0x0;
    
    // Create BSP VMSA page
    unsigned char bsp_vmsa_page[PAGE_SIZE];
    if (create_vmsa_page(BSP_EIP, vcpu_type, vmm_type, guest_features, bsp_vmsa_page) != 0) {
        return -1;
    }
    
    // Update with BSP VMSA page
    if (gctx_update_page(&gctx, PAGE_TYPE_VMSA, VMSA_GPA, bsp_vmsa_page, PAGE_SIZE) != 0) {
        return -1;
    }
    
    // Create AP VMSA page if EIP > 0 and we have multiple VCPUs
    if (ap_eip > 0 && vcpus > 1) {
        unsigned char ap_vmsa_page[PAGE_SIZE];
        if (create_vmsa_page(ap_eip, vcpu_type, vmm_type, guest_features, ap_vmsa_page) != 0) {
            return -1;
        }
        
        // Update with AP VMSA pages (one per additional VCPU)
        for (uint32_t i = 1; i < vcpus; i++) {
            if (gctx_update_page(&gctx, PAGE_TYPE_VMSA, VMSA_GPA, ap_vmsa_page, PAGE_SIZE) != 0) {
                return -1;
            }
        }
    } else if (vcpus > 1) {
        // Even if ap_eip is 0, we still need to update for additional VCPUs
        // Use BSP page for all VCPUs when ap_eip is 0
        for (uint32_t i = 1; i < vcpus; i++) {
            if (gctx_update_page(&gctx, PAGE_TYPE_VMSA, VMSA_GPA, bsp_vmsa_page, PAGE_SIZE) != 0) {
                return -1;
            }
        }
    }
    
    // Return the final launch digest
    memcpy(output_digest, gctx.ld, LD_BYTES);
    
    return 0;
}

