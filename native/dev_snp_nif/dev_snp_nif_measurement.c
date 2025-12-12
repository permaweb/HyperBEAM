// Measurement calculation functions for SEV-SNP launch digest
// This implements the algorithm from the SEV crate's snp_calc_launch_digest

#include "dev_snp_nif.h"
#include <openssl/sha.h>
#include <openssl/evp.h>
#include <string.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>

#define LD_BYTES 48  // Launch digest size (SHA-384 = 48 bytes)
#define PAGE_SIZE 4096
#define VMSA_GPA 0xFFFFFFFFF000ULL
#define FOUR_GB 0x100000000ULL

// OVMF GUIDs (little-endian)
static const unsigned char OVMF_TABLE_FOOTER_GUID[16] = {
    0x2d, 0x08, 0x5a, 0xa3, 0x66, 0x0c, 0x5a, 0xa3, 0xea, 0xab, 0xf7, 0x45, 0xb2, 0x1f, 0xb2, 0x96
};
static const unsigned char SEV_HASH_TABLE_RV_GUID[16] = {
    0x54, 0xa8, 0xda, 0x1f, 0x6b, 0x04, 0x4b, 0x3b, 0x7b, 0x92, 0x04, 0x4b, 0x3b, 0x3a, 0x35, 0x72
};

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
// Note: This must match Rust's PaddedSevHashTable serialization exactly
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
    
    // Calculate padding size to match Rust: ((size + 15) & !15) - size
    size_t table_size = sizeof(sev_hash_table_t);
    size_t padded_size = ((table_size + 15) & ~15);  // Round up to 16-byte boundary
    size_t padding_size = padded_size - table_size;
    
    // Serialize table to page (offset 0)
    // Copy the table, then add padding to match PaddedSevHashTable
    if (table_size > PAGE_SIZE) {
        return -1;  // Safety check
    }
    memcpy(page_output, &table, table_size);
    // Padding is already zero from memset above
    
    return 0;
}

// VMSA structure - simplified version matching Rust implementation
// BSP EIP constant
#define BSP_EIP 0xfffffff0ULL

// Helper to safely write to VMSA page with bounds checking
static int vmsa_write_u64(unsigned char *page, size_t offset, uint64_t value) {
    if (offset + 8 > PAGE_SIZE) {
        return -1;
    }
    memcpy(page + offset, &value, 8);
    return 0;
}

static int vmsa_write_u32(unsigned char *page, size_t offset, uint32_t value) {
    if (offset + 4 > PAGE_SIZE) {
        return -1;
    }
    memcpy(page + offset, &value, 4);
    return 0;
}

static int vmsa_write_u16(unsigned char *page, size_t offset, uint16_t value) {
    if (offset + 2 > PAGE_SIZE) {
        return -1;
    }
    memcpy(page + offset, &value, 2);
    return 0;
}

static int vmsa_write_u8(unsigned char *page, size_t offset, uint8_t value) {
    if (offset >= PAGE_SIZE) {
        return -1;
    }
    page[offset] = value;
    return 0;
}

// Create a VMSA page for a single VCPU
// This is a simplified implementation that creates a valid VMSA structure
// Based on Rust SevEsSaveArea structure (4096 bytes when serialized)
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
    
    // Segment registers are at offsets 0x00-0x5F (already zero from memset)
    // GDTR, IDTR, LDTR, TR are at offsets 0x60-0x9F (already zero)
    
    // CPL at offset 0xAB
    if (vmsa_write_u8(vmsa_page, 0xAB, 0) != 0) return -1;
    
    // EFER at offset 0xB0 (8 bytes, little-endian)
    if (vmsa_write_u64(vmsa_page, 0xB0, 0x1000) != 0) return -1;
    
    // CR4 at offset 0x128 (8 bytes)
    if (vmsa_write_u64(vmsa_page, 0x128, 0x40) != 0) return -1;
    
    // CR3 at offset 0x130 (8 bytes) - 0 (already zero)
    // CR0 at offset 0x138 (8 bytes)
    if (vmsa_write_u64(vmsa_page, 0x138, 0x10) != 0) return -1;
    
    // DR7 at offset 0x140 (8 bytes)
    if (vmsa_write_u64(vmsa_page, 0x140, 0x400) != 0) return -1;
    
    // DR6 at offset 0x148 (8 bytes)
    if (vmsa_write_u64(vmsa_page, 0x148, 0xffff0ff0) != 0) return -1;
    
    // RFLAGS at offset 0x150 (8 bytes)
    if (vmsa_write_u64(vmsa_page, 0x150, 0x2) != 0) return -1;
    
    // RIP at offset 0x158 (8 bytes, little-endian)
    uint64_t rip = eip & 0xffff;
    if (vmsa_write_u64(vmsa_page, 0x158, rip) != 0) return -1;
    
    // RDX at offset 0x1E8 (8 bytes)
    uint64_t rdx = 0;
    if (vmm_type == 2) {  // EC2
        rdx = 0x80000001;  // EC2 specific value
    } else {
        rdx = (uint64_t)vcpu_type;  // QEMU uses vcpu_type signature
    }
    if (vmsa_write_u64(vmsa_page, 0x1E8, rdx) != 0) return -1;
    
    // G_PAT at offset 0x240 (8 bytes)
    if (vmsa_write_u64(vmsa_page, 0x240, 0x7040600070406ULL) != 0) return -1;
    
    // SEV Features at offset 0x3E0 (8 bytes, little-endian)
    if (vmsa_write_u64(vmsa_page, 0x3E0, guest_features) != 0) return -1;
    
    // XCR0 at offset 0x3E8 (8 bytes)
    if (vmsa_write_u64(vmsa_page, 0x3E8, 0x1) != 0) return -1;
    
    // MXCSR at offset 0x3F0 (4 bytes) - only set for QEMU
    uint32_t mxcsr = 0;
    uint16_t fcw = 0;
    if (vmm_type == 1) {  // QEMU
        mxcsr = 0x1f80;
        fcw = 0x37f;
    }
    if (vmsa_write_u32(vmsa_page, 0x3F0, mxcsr) != 0) return -1;
    
    // X87 FCW at offset 0x3F4 (2 bytes)
    if (vmsa_write_u16(vmsa_page, 0x3F4, fcw) != 0) return -1;
    
    // All other fields remain zero (from memset)
    // The structure is 4096 bytes total with manual_padding at the end
    
    return 0;
}

// Parse OVMF file to extract SEV hashes table GPA
// Returns 0 on success with GPA in *gpa_out, -1 on failure
static int parse_ovmf_sev_hashes_gpa(const char *ovmf_path, uint64_t *gpa_out) {
    if (!ovmf_path || !gpa_out) {
        return -1;
    }
    
    FILE *f = fopen(ovmf_path, "rb");
    if (!f) {
        return -1;
    }
    
    // Get file size
    fseek(f, 0, SEEK_END);
    long file_size = ftell(f);
    if (file_size < 0 || file_size < 50) {  // Need at least footer entry
        fclose(f);
        return -1;
    }
    fseek(f, 0, SEEK_SET);
    
    // Read the last 32 bytes to find footer entry
    // Footer entry is at offset: file_size - 32 - ENTRY_HEADER_SIZE (18 bytes)
    const size_t ENTRY_HEADER_SIZE = 18;  // 2 bytes size + 16 bytes GUID
    long footer_entry_offset = file_size - 32 - ENTRY_HEADER_SIZE;
    if (footer_entry_offset < 0) {
        fclose(f);
        return -1;
    }
    
    fseek(f, footer_entry_offset, SEEK_SET);
    unsigned char footer_entry[ENTRY_HEADER_SIZE];
    if (fread(footer_entry, 1, ENTRY_HEADER_SIZE, f) != ENTRY_HEADER_SIZE) {
        fclose(f);
        return -1;
    }
    
    // Check if this is the footer table GUID
    if (memcmp(footer_entry + 2, OVMF_TABLE_FOOTER_GUID, 16) != 0) {
        fclose(f);
        return -1;
    }
    
    // Get footer size (first 2 bytes, little-endian)
    uint16_t footer_size = footer_entry[0] | (footer_entry[1] << 8);
    if (footer_size < ENTRY_HEADER_SIZE) {
        fclose(f);
        return -1;
    }
    
    // Calculate table size and start
    size_t table_size = footer_size - ENTRY_HEADER_SIZE;
    long table_start = footer_entry_offset - table_size;
    if (table_start < 0) {
        fclose(f);
        return -1;
    }
    
    // Read the table
    unsigned char *table_data = malloc(table_size);
    if (!table_data) {
        fclose(f);
        return -1;
    }
    
    fseek(f, table_start, SEEK_SET);
    if (fread(table_data, 1, table_size, f) != table_size) {
        free(table_data);
        fclose(f);
        return -1;
    }
    fclose(f);
    
    // Parse entries backwards
    size_t offset = table_size;
    int found = 0;
    while (offset >= ENTRY_HEADER_SIZE) {
        // Read entry header
        unsigned char *entry_ptr = table_data + offset - ENTRY_HEADER_SIZE;
        uint16_t entry_size = entry_ptr[0] | (entry_ptr[1] << 8);
        
        if (entry_size < ENTRY_HEADER_SIZE || offset < entry_size) {
            break;
        }
        
        // Check if this is the SEV_HASH_TABLE_RV_GUID entry
        if (memcmp(entry_ptr + 2, SEV_HASH_TABLE_RV_GUID, 16) == 0) {
            // Entry data is before the header
            size_t data_offset = offset - entry_size;
            if (data_offset + 4 <= table_size) {
                // First 4 bytes are the GPA (little-endian u32)
                uint32_t gpa_u32 = (uint32_t)table_data[data_offset] |
                                  ((uint32_t)table_data[data_offset + 1] << 8) |
                                  ((uint32_t)table_data[data_offset + 2] << 16) |
                                  ((uint32_t)table_data[data_offset + 3] << 24);
                *gpa_out = (uint64_t)gpa_u32;
                found = 1;
            }
            break;
        }
        
        offset -= entry_size;
    }
    
    free(table_data);
    return found ? 0 : -1;
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
    uint64_t sev_hashes_gpa,  // SEV hashes table GPA (0 if not provided)
    unsigned char *output_digest  // 48 bytes output
) {
    gctx_t gctx;
    
    // Initialize GCTX with OVMF hash if provided
    if (ovmf_hash_hex) {
        size_t hex_len = strlen(ovmf_hash_hex);
        if (hex_len == 96) {  // 48 bytes * 2 hex chars
            // Convert hex to binary
            unsigned char ovmf_hash[LD_BYTES];
            for (int i = 0; i < LD_BYTES; i++) {
                if (i*2+1 >= hex_len) {
                    return -1;  // Bounds check
                }
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
    } else {
        gctx_init(&gctx);
    }
    
    // Update with SEV hashes table if kernel hash is provided and GPA is available
    if (kernel_hash && initrd_hash && append_hash && sev_hashes_gpa != 0) {
        unsigned char sev_hashes_page[PAGE_SIZE];
        if (construct_sev_hashes_page(kernel_hash, initrd_hash, append_hash, sev_hashes_page) == 0) {
            if (gctx_update_page(&gctx, PAGE_TYPE_NORMAL, sev_hashes_gpa, sev_hashes_page, PAGE_SIZE) != 0) {
                return -1;
            }
        }
    }
    
    // Create and update VMSA pages
    // For SEV-SNP, all VCPUs use the same EIP value (BSP_EIP)
    // When OVMF is available, we could extract the reset EIP, but for now we use the default
    // Each VCPU needs its own VMSA page, even if they have the same EIP
    
    // Create VMSA page with BSP EIP (used for all VCPUs when OVMF reset EIP is not available)
    unsigned char vmsa_page[PAGE_SIZE];
    if (create_vmsa_page(BSP_EIP, vcpu_type, vmm_type, guest_features, vmsa_page) != 0) {
        return -1;
    }
    
    // Update with VMSA page for each VCPU
    // Each VCPU gets its own page, even though they may have the same EIP
    // This matches the Rust implementation which calls vmsa.pages(vcpus) to generate separate pages
    for (uint32_t i = 0; i < vcpus; i++) {
        if (gctx_update_page(&gctx, PAGE_TYPE_VMSA, VMSA_GPA, vmsa_page, PAGE_SIZE) != 0) {
            return -1;
        }
    }
    
    // Return the final launch digest
    memcpy(output_digest, gctx.ld, LD_BYTES);
    
    return 0;
}

