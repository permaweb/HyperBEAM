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
#include <errno.h>

#define LD_BYTES 48  // Launch digest size (SHA-384 = 48 bytes)
#define PAGE_SIZE 4096
#define VMSA_GPA 0xFFFFFFFFF000ULL
#define FOUR_GB 0x100000000ULL

// OVMF GUIDs (little-endian)
// OVMF_TABLE_FOOTER_GUID: 96b582de-1fb2-45f7-baea-a366c55a082d
// Converted to little-endian bytes using guid.to_bytes_le()
static const unsigned char OVMF_TABLE_FOOTER_GUID[16] = {
    0xde, 0x82, 0xb5, 0x96, 0xb2, 0x1f, 0xf7, 0x45, 0xba, 0xea, 0xa3, 0x66, 0xc5, 0x5a, 0x08, 0x2d
};
// SEV_HASH_TABLE_RV_GUID: 7255371f-3a3b-4b04-927b-1da6efa8d454
// Converted to little-endian bytes
static const unsigned char SEV_HASH_TABLE_RV_GUID[16] = {
    0x1f, 0x37, 0x55, 0x72, 0x3b, 0x3a, 0x04, 0x4b, 0x7b, 0x92, 0xa6, 0x1d, 0xa8, 0xef, 0x54, 0xd4
};
// OVMF_SEV_META_DATA_GUID: dc886566-984a-4798-a75e-5585a7bf67cc (little-endian bytes)
static const unsigned char OVMF_SEV_META_DATA_GUID[16] = {
    0x66, 0x65, 0x88, 0xdc, 0x4a, 0x98, 0x98, 0x47, 0xa7, 0x5e, 0x55, 0x85, 0xa7, 0xbf, 0x67, 0xcc
};
// SEV_ES_RESET_BLOCK_GUID: 00f771de-1a7e-4fcb-890e-68c77e2fb44e
// Converted to little-endian bytes
static const unsigned char SEV_ES_RESET_BLOCK_GUID[16] = {
    0xde, 0x71, 0xf7, 0x00, 0x7e, 0x1a, 0xcb, 0x4f, 0x89, 0x0e, 0x68, 0xc7, 0x7e, 0x2f, 0xb4, 0x4e
};

// Page types
#define PAGE_TYPE_NORMAL 0x01
#define PAGE_TYPE_VMSA 0x02
#define PAGE_TYPE_ZERO 0x03
#define PAGE_TYPE_UNMEASURED 0x04
#define PAGE_TYPE_SECRETS 0x05
#define PAGE_TYPE_CPUID 0x06

// OVMF Section types
#define SECTION_TYPE_SNP_SEC_MEMORY 1
#define SECTION_TYPE_SNP_SECRETS 2
#define SECTION_TYPE_CPUID 3
#define SECTION_TYPE_SVSM_CAA 4
#define SECTION_TYPE_SNP_KERNEL_HASHES 0x10

// OVMF Metadata structures
typedef struct {
    uint32_t gpa;        // Guest Physical Address (little-endian)
    uint32_t size;       // Size (little-endian)
    uint8_t section_type;  // Section type
    uint8_t _padding[3];   // Padding to align to 4 bytes
} __attribute__((packed)) ovmf_metadata_section_desc_t;

typedef struct {
    unsigned char signature[4];  // "ASEV"
    uint32_t size;              // Total size (little-endian)
    uint32_t version;           // Version (little-endian, should be 1)
    uint32_t num_items;         // Number of items (little-endian)
} __attribute__((packed)) ovmf_metadata_header_t;

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
    // For PAGE_TYPE_NORMAL and PAGE_TYPE_VMSA, hash the full page first
    // For PAGE_TYPE_ZERO, PAGE_TYPE_SECRETS, and PAGE_TYPE_CPUID, use 48 bytes of zeros
    // For other types, use the contents directly (should be small)
    if (page_type == PAGE_TYPE_ZERO || 
        page_type == PAGE_TYPE_SECRETS || 
        page_type == PAGE_TYPE_CPUID) {
        // Zero pages, secrets, and CPUID pages: use 48 bytes of zeros (hash of a zero page)
        memset(page_info + pos, 0, SHA384_DIGEST_LENGTH);
        pos += SHA384_DIGEST_LENGTH;
    } else if (contents && contents_len > 0) {
        if (contents_len == PAGE_SIZE && 
            (page_type == PAGE_TYPE_NORMAL || page_type == PAGE_TYPE_VMSA)) {
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
                } else {
                    EVP_MD_CTX_free(md_ctx);
                    return -1;
                }
            } else {
                EVP_MD_CTX_free(md_ctx);
                return -1;
            }
            EVP_MD_CTX_free(md_ctx);
        } else {
            // For non-page-sized contents, copy directly
            // But ensure it fits in page_info buffer
            if (pos + contents_len > page_info_len) {
                return -1;  // Would overflow page_info buffer
            }
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
    
    // Debug: print launch digest after update (first 16 bytes)
    fprintf(stderr, "[SNP_DEBUG] gctx_update_page: page_type=%u, gpa=0x%016llx, new_ld (first 16 bytes): ", 
            page_type, (unsigned long long)gpa);
    for (int i = 0; i < 16 && i < LD_BYTES; i++) {
        fprintf(stderr, "%02x", gctx->ld[i]);
    }
    fprintf(stderr, "\n");
    
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
// Note: This must match Rust's construct_page() which places the table at the page offset
static int construct_sev_hashes_page(
    const unsigned char *kernel_hash,
    const unsigned char *initrd_hash,
    const unsigned char *append_hash,
    size_t page_offset,  // Offset within page (from GPA & PAGE_MASK)
    unsigned char *page_output  // Must be PAGE_SIZE bytes
) {
    if (!kernel_hash || !initrd_hash || !append_hash || !page_output) {
        return -1;
    }
    
    if (page_offset >= PAGE_SIZE) {
        return -1;  // Invalid offset
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
    
    // Check that table fits in page at the given offset
    if (page_offset + padded_size > PAGE_SIZE) {
        return -1;  // Table would overflow page
    }
    
    // Serialize table to page at the specified offset (matching Rust construct_page)
    // The padded table is placed starting at page_offset
    memcpy(page_output + page_offset, &table, table_size);
    // Padding bytes are already zero from memset above
    
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
    
    // Determine segment register values based on VMM type
    uint16_t cs_flags, ss_flags, tr_flags;
    if (vmm_type == 2) {  // EC2
        if (eip == 0xfffffff0) {
            cs_flags = 0x9a;
            ss_flags = 0x92;
            tr_flags = 0x83;
        } else {
            cs_flags = 0x9b;
            ss_flags = 0x92;
            tr_flags = 0x83;
        }
    } else if (vmm_type == 3) {  // KRUN
        cs_flags = 0x9a;
        ss_flags = 0x92;
        tr_flags = 0x83;
    } else {  // QEMU (vmm_type == 1) or default
        cs_flags = 0x9b;
        ss_flags = 0x93;
        tr_flags = 0x8b;
    }
    
    // Write segment registers (each VmcbSeg is 16 bytes: selector:u16, attrib:u16, limit:u32, base:u64)
    // ES at offset 0x00: selector=0, attrib=0x93, limit=0xffff, base=0
    if (vmsa_write_u16(vmsa_page, 0x00, 0) != 0) return -1;
    if (vmsa_write_u16(vmsa_page, 0x02, 0x93) != 0) return -1;
    if (vmsa_write_u32(vmsa_page, 0x04, 0xffff) != 0) return -1;
    if (vmsa_write_u64(vmsa_page, 0x08, 0) != 0) return -1;
    
    // CS at offset 0x10: selector=0xf000, attrib=cs_flags, limit=0xffff, base=(eip & 0xffff0000)
    if (vmsa_write_u16(vmsa_page, 0x10, 0xf000) != 0) return -1;
    if (vmsa_write_u16(vmsa_page, 0x12, cs_flags) != 0) return -1;
    if (vmsa_write_u32(vmsa_page, 0x14, 0xffff) != 0) return -1;
    if (vmsa_write_u64(vmsa_page, 0x18, eip & 0xffff0000ULL) != 0) return -1;
    
    // SS at offset 0x20: selector=0, attrib=ss_flags, limit=0xffff, base=0
    if (vmsa_write_u16(vmsa_page, 0x20, 0) != 0) return -1;
    if (vmsa_write_u16(vmsa_page, 0x22, ss_flags) != 0) return -1;
    if (vmsa_write_u32(vmsa_page, 0x24, 0xffff) != 0) return -1;
    if (vmsa_write_u64(vmsa_page, 0x28, 0) != 0) return -1;
    
    // DS at offset 0x30: selector=0, attrib=0x93, limit=0xffff, base=0
    if (vmsa_write_u16(vmsa_page, 0x30, 0) != 0) return -1;
    if (vmsa_write_u16(vmsa_page, 0x32, 0x93) != 0) return -1;
    if (vmsa_write_u32(vmsa_page, 0x34, 0xffff) != 0) return -1;
    if (vmsa_write_u64(vmsa_page, 0x38, 0) != 0) return -1;
    
    // FS at offset 0x40: selector=0, attrib=0x93, limit=0xffff, base=0
    if (vmsa_write_u16(vmsa_page, 0x40, 0) != 0) return -1;
    if (vmsa_write_u16(vmsa_page, 0x42, 0x93) != 0) return -1;
    if (vmsa_write_u32(vmsa_page, 0x44, 0xffff) != 0) return -1;
    if (vmsa_write_u64(vmsa_page, 0x48, 0) != 0) return -1;
    
    // GS at offset 0x50: selector=0, attrib=0x93, limit=0xffff, base=0
    if (vmsa_write_u16(vmsa_page, 0x50, 0) != 0) return -1;
    if (vmsa_write_u16(vmsa_page, 0x52, 0x93) != 0) return -1;
    if (vmsa_write_u32(vmsa_page, 0x54, 0xffff) != 0) return -1;
    if (vmsa_write_u64(vmsa_page, 0x58, 0) != 0) return -1;
    
    // GDTR at offset 0x60: selector=0, attrib=0, limit=0xffff, base=0
    if (vmsa_write_u16(vmsa_page, 0x60, 0) != 0) return -1;
    if (vmsa_write_u16(vmsa_page, 0x62, 0) != 0) return -1;
    if (vmsa_write_u32(vmsa_page, 0x64, 0xffff) != 0) return -1;
    if (vmsa_write_u64(vmsa_page, 0x68, 0) != 0) return -1;
    
    // IDTR at offset 0x70: selector=0, attrib=0, limit=0xffff, base=0
    if (vmsa_write_u16(vmsa_page, 0x70, 0) != 0) return -1;
    if (vmsa_write_u16(vmsa_page, 0x72, 0) != 0) return -1;
    if (vmsa_write_u32(vmsa_page, 0x74, 0xffff) != 0) return -1;
    if (vmsa_write_u64(vmsa_page, 0x78, 0) != 0) return -1;
    
    // LDTR at offset 0x80: selector=0, attrib=0x82, limit=0xffff, base=0
    if (vmsa_write_u16(vmsa_page, 0x80, 0) != 0) return -1;
    if (vmsa_write_u16(vmsa_page, 0x82, 0x82) != 0) return -1;
    if (vmsa_write_u32(vmsa_page, 0x84, 0xffff) != 0) return -1;
    if (vmsa_write_u64(vmsa_page, 0x88, 0) != 0) return -1;
    
    // TR at offset 0x90: selector=0, attrib=tr_flags, limit=0xffff, base=0
    if (vmsa_write_u16(vmsa_page, 0x90, 0) != 0) return -1;
    if (vmsa_write_u16(vmsa_page, 0x92, tr_flags) != 0) return -1;
    if (vmsa_write_u32(vmsa_page, 0x94, 0xffff) != 0) return -1;
    if (vmsa_write_u64(vmsa_page, 0x98, 0) != 0) return -1;
    
    // VMPL0_SSP through U_CET at offsets 0xA0-0xC8 (all zero from memset)
    // CPL at offset 0xCB (after vmpl at 0xCA)
    if (vmsa_write_u8(vmsa_page, 0xCB, 0) != 0) return -1;
    
    // EFER at offset 0xD0 (8 bytes, little-endian)
    // After: 10 VmcbSegs (0xA0) + vmpl0_ssp..u_cet (0x28) + reserved_0xc8 (2) + vmpl (1) + cpl (1) + reserved_0xcc (4) = 0xD0
    if (vmsa_write_u64(vmsa_page, 0xD0, 0x1000) != 0) return -1;
    
    // Reserved 0xD8 (104 bytes = 0x68) - already zero from memset
    
    // XSS at offset 0x140 (8 bytes) - 0 (already zero)
    
    // CR4 at offset 0x148 (8 bytes)
    // After: efer (0xD0) + reserved_0xd8 (0x68) + xss (0x8) = 0x140, then cr4 = 0x148
    if (vmsa_write_u64(vmsa_page, 0x148, 0x40) != 0) return -1;
    
    // CR3 at offset 0x150 (8 bytes) - 0 (already zero)
    
    // CR0 at offset 0x158 (8 bytes)
    if (vmsa_write_u64(vmsa_page, 0x158, 0x10) != 0) return -1;
    
    // DR7 at offset 0x160 (8 bytes)
    if (vmsa_write_u64(vmsa_page, 0x160, 0x400) != 0) return -1;
    
    // DR6 at offset 0x168 (8 bytes)
    if (vmsa_write_u64(vmsa_page, 0x168, 0xffff0ff0) != 0) return -1;
    
    // RFLAGS at offset 0x170 (8 bytes)
    if (vmsa_write_u64(vmsa_page, 0x170, 0x2) != 0) return -1;
    
    // RIP at offset 0x178 (8 bytes, little-endian)
    uint64_t rip = eip & 0xffff;
    if (vmsa_write_u64(vmsa_page, 0x178, rip) != 0) return -1;
    
    // DR0-DR3, DR0_ADDR_MASK-DR3_ADDR_MASK, reserved_0x1c0, rsp, s_cet, ssp, isst_addr, rax, star, lstar, cstar, sfmask, kernel_gs_base, sysenter_cs, sysenter_esp, sysenter_eip, cr2, reserved_0x248
    // (all zero from memset or not set for QEMU)
    
    // G_PAT at offset 0x268 (8 bytes)
    // After: rip (0x178) + dr0..dr3 (0x20) + dr0_addr_mask..dr3_addr_mask (0x20) + reserved_0x1c0 (0x18) + rsp..cr2 (0x70) + reserved_0x248 (0x20) = 0x268
    if (vmsa_write_u64(vmsa_page, 0x268, 0x7040600070406ULL) != 0) return -1;
    
    // dbgctrl, br_from, br_to, last_excp_from, last_excp_to, reserved_0x298, pkru, tsc_aux, reserved_0x2f0, rcx
    // (all zero from memset or not set for QEMU)
    
    // RDX at offset 0x318 (8 bytes)
    // After: rcx (0x310) + rdx = 0x318
    // For EC2: rdx = 0 (always)
    // For QEMU: rdx = vcpu_type.sig() (CPU signature)
    // For KRUN: rdx = 0
    uint64_t rdx = 0;
    if (vmm_type == 1) {  // QEMU
        // Calculate CPU signature from vcpu_type
        // This matches Rust's cpu_sig function and CpuType::sig()
        int32_t cpu_sig = 0;
        switch (vcpu_type) {
            case 0:  // Epyc
            case 1:  // EpycV1
            case 3:  // EpycIBPB
            case 4:  // EpycV3
            case 5:  // EpycV4
                // cpu_sig(23, 1, 2):
                // family=23 > 15: family_low=15, family_high=(23-15)=8
                // model=1: model_low=1, model_high=0
                // stepping=2: stepping_low=2
                // = (8 << 20) | (0 << 16) | (15 << 8) | (1 << 4) | 2
                // = 0x800f12
                cpu_sig = 0x800f12;
                break;
            case 6:  // EpycRome
            case 7:  // EpycRomeV1
            case 8:  // EpycRomeV2
            case 9:  // EpycRomeV3
                // cpu_sig(23, 49, 0):
                // family=23 > 15: family_low=15, family_high=(23-15)=8
                // model=49: model_low=1, model_high=3
                // stepping=0: stepping_low=0
                // = (8 << 20) | (3 << 16) | (15 << 8) | (1 << 4) | 0
                // = 0x803f10
                cpu_sig = 0x803f10;
                break;
            case 10: // EpycMilan
            case 11: // EpycMilanV1
            case 12: // EpycMilanV2
                // cpu_sig(25, 1, 1):
                // family=25 > 15: family_low=15, family_high=(25-15)=10
                // model=1: model_low=1, model_high=0
                // stepping=1: stepping_low=1
                // = (10 << 20) | (0 << 16) | (15 << 8) | (1 << 4) | 1
                // = 0xa00f11
                cpu_sig = 0xa00f11;
                break;
            case 13: // EpycGenoa
            case 14: // EpycGenoaV1
                // cpu_sig(25, 17, 0):
                // family=25 > 15: family_low=15, family_high=(25-15)=10
                // model=17: model_low=1, model_high=1
                // stepping=0: stepping_low=0
                // = (10 << 20) | (1 << 16) | (15 << 8) | (1 << 4) | 0
                // = 0xa01f10
                cpu_sig = 0xa01f10;
                break;
            default:
                // Default to EpycV4 signature
                cpu_sig = 0x800f12;
                break;
        }
        rdx = (uint64_t)(uint32_t)cpu_sig;  // Sign-extend to u64
    }
    // For EC2 (vmm_type == 2) and KRUN (vmm_type == 3), rdx remains 0
    if (vmsa_write_u64(vmsa_page, 0x318, rdx) != 0) return -1;
    
    // rbx, reserved_0x320, rbp, rsi, rdi, r8..r15, reserved_0x380, guest_exit_info_1..event_inj
    // (all zero from memset or not set for QEMU)
    
    // SEV Features at offset 0x3E8 (8 bytes, little-endian)
    // After: guest_exit_info_1 (0x388) + guest_exit_info_2 (0x390) + guest_exit_int_info (0x398) + guest_nrip (0x3A0) + sev_features = 0x3E8
    if (vmsa_write_u64(vmsa_page, 0x3E8, guest_features) != 0) return -1;
    
    // vintr_ctrl, guest_exit_code, virtual_tom, tlb_id, pcpu_id, event_inj, reserved_0x3f0
    // (all zero from memset or not set for QEMU)
    
    // XCR0 at offset 0x3F0 (8 bytes)
    // After: sev_features (0x3E8) + vintr_ctrl..event_inj (0x8) = 0x3F0, then xcr0 = 0x3F0
    if (vmsa_write_u64(vmsa_page, 0x3F0, 0x1) != 0) return -1;
    
    // x87_dp, reserved_0x3f0 (already handled)
    
    // MXCSR at offset 0x3FC (4 bytes) - only set for QEMU
    // After: xcr0 (0x3F0) + reserved_0x3f0 (0x10) + x87_dp (0x3F8) + mxcsr = 0x3FC
    uint32_t mxcsr = 0;
    uint16_t fcw = 0;
    if (vmm_type == 1) {  // QEMU
        mxcsr = 0x1f80;
        fcw = 0x37f;
    }
    if (vmsa_write_u32(vmsa_page, 0x3FC, mxcsr) != 0) return -1;
    
    // x87_ftw, x87_fsw (not set for QEMU, remain zero)
    
    // X87 FCW at offset 0x402 (2 bytes)
    // After: mxcsr (0x3FC) + x87_ftw (0x3FE) + x87_fsw (0x400) + x87_fcw = 0x402
    if (vmsa_write_u16(vmsa_page, 0x402, fcw) != 0) return -1;
    
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

// Parse OVMF footer table to find a GUID entry
// Returns 0 on success with entry data in *entry_data_out, -1 on failure
static int parse_ovmf_footer_table_entry(const char *ovmf_path, const unsigned char *target_guid, 
                                         unsigned char **entry_data_out, size_t *entry_data_len) {
    if (!ovmf_path || !target_guid || !entry_data_out || !entry_data_len) {
        return -1;
    }
    
    FILE *f = fopen(ovmf_path, "rb");
    if (!f) {
        fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: failed to open file %s (errno=%d)\n", ovmf_path, errno);
        return -1;
    }
    fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: successfully opened file %s\n", ovmf_path);
    
    // Get file size
    fseek(f, 0, SEEK_END);
    long file_size = ftell(f);
    if (file_size < 0 || file_size < 50) {
        fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: file too small (%ld bytes)\n", file_size);
        fclose(f);
        return -1;
    }
    fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: file size=%ld\n", file_size);
    fseek(f, 0, SEEK_SET);
    
    const size_t ENTRY_HEADER_SIZE = 18;  // 2 bytes size + 16 bytes GUID
    long footer_entry_offset = file_size - 32 - ENTRY_HEADER_SIZE;
    if (footer_entry_offset < 0) {
        fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: footer offset negative\n");
        fclose(f);
        return -1;
    }
    
    fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: reading footer entry at offset %ld\n", footer_entry_offset);
    fseek(f, footer_entry_offset, SEEK_SET);
    unsigned char footer_entry[ENTRY_HEADER_SIZE];
    if (fread(footer_entry, 1, ENTRY_HEADER_SIZE, f) != ENTRY_HEADER_SIZE) {
        fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: failed to read footer entry\n");
        fclose(f);
        return -1;
    }
    
    // Check if this is the footer table GUID
    fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: checking footer GUID: ");
    for (int i = 0; i < 16; i++) {
        fprintf(stderr, "%02x", footer_entry[2 + i]);
    }
    fprintf(stderr, "\n[SNP_DEBUG] parse_ovmf_footer_table_entry: expected footer GUID: ");
    for (int i = 0; i < 16; i++) {
        fprintf(stderr, "%02x", OVMF_TABLE_FOOTER_GUID[i]);
    }
    fprintf(stderr, "\n");
    
    if (memcmp(footer_entry + 2, OVMF_TABLE_FOOTER_GUID, 16) != 0) {
        fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: footer GUID mismatch - not an OVMF footer table\n");
        fclose(f);
        return -1;
    }
    
    fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: footer GUID matches, reading table\n");
    
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
    
    // Parse entries backwards to find target GUID
    size_t offset = table_size;
    int found = 0;
    int entry_count = 0;
    fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: searching table (size=%zu) for target GUID: ", table_size);
    for (int i = 0; i < 16; i++) {
        fprintf(stderr, "%02x", target_guid[i]);
    }
    fprintf(stderr, "\n");
    
    while (offset >= ENTRY_HEADER_SIZE) {
        unsigned char *entry_ptr = table_data + offset - ENTRY_HEADER_SIZE;
        uint16_t entry_size = entry_ptr[0] | (entry_ptr[1] << 8);
        
        if (entry_size < ENTRY_HEADER_SIZE || offset < entry_size) {
            fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: invalid entry size %u at offset %zu\n", entry_size, offset);
            break;
        }
        
        entry_count++;
        fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: entry %d: size=%u, offset=%zu, GUID: ", entry_count, entry_size, offset);
        for (int i = 0; i < 16; i++) {
            fprintf(stderr, "%02x", entry_ptr[2 + i]);
        }
        fprintf(stderr, "\n");
        
        // Check if this is the target GUID entry
        if (memcmp(entry_ptr + 2, target_guid, 16) == 0) {
            fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: found target GUID!\n");
            // Entry data is before the header
            size_t data_offset = offset - entry_size;
            size_t data_len = entry_size - ENTRY_HEADER_SIZE;
            fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: data_offset=%zu, data_len=%zu\n", data_offset, data_len);
            if (data_offset + data_len <= table_size) {
                *entry_data_out = malloc(data_len);
                if (!*entry_data_out) {
                    free(table_data);
                    return -1;
                }
                memcpy(*entry_data_out, table_data + data_offset, data_len);
                *entry_data_len = data_len;
                found = 1;
            } else {
                fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: data out of bounds\n");
            }
            break;
        }
        
        offset -= entry_size;
    }
    
    if (!found) {
        fprintf(stderr, "[SNP_DEBUG] parse_ovmf_footer_table_entry: searched %d entries, target GUID not found\n", entry_count);
    }
    free(table_data);
    return found ? 0 : -1;
}

// Extract reset EIP from OVMF footer table
// Returns 0 on success with EIP in *eip_out, -1 on failure
static int parse_ovmf_reset_eip(const char *ovmf_path, uint32_t *eip_out) {
    if (!ovmf_path || !eip_out) {
        return -1;
    }
    
    unsigned char *entry_data = NULL;
    size_t entry_len = 0;
    if (parse_ovmf_footer_table_entry(ovmf_path, SEV_ES_RESET_BLOCK_GUID, 
                                      &entry_data, &entry_len) != 0) {
        return -1;  // Entry not found
    }
    
    if (entry_len < 4) {
        free(entry_data);
        return -1;
    }
    
    // First 4 bytes are the EIP (little-endian u32)
    *eip_out = (uint32_t)entry_data[0] |
              ((uint32_t)entry_data[1] << 8) |
              ((uint32_t)entry_data[2] << 16) |
              ((uint32_t)entry_data[3] << 24);
    
    free(entry_data);
    return 0;
}

// Parse OVMF SEV metadata and update GCTX with all metadata sections
// Returns 0 on success, -1 on failure
static int parse_and_update_ovmf_metadata(gctx_t *gctx, const char *ovmf_path, 
                                          uint8_t vmm_type,
                                          const unsigned char *kernel_hash,
                                          const unsigned char *initrd_hash,
                                          const unsigned char *append_hash) {
    if (!gctx || !ovmf_path) {
        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: invalid args\n");
        return -1;
    }
    
    fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: trying path=%s\n", ovmf_path);
    
    // Get SEV hashes table GPA from footer table (needed for page offset calculation)
    uint64_t sev_hashes_table_gpa = 0;
    if (kernel_hash && initrd_hash && append_hash) {
        // Only need this if we have kernel hashes
        if (parse_ovmf_sev_hashes_gpa(ovmf_path, &sev_hashes_table_gpa) != 0) {
            fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: SEV hashes GPA not found, will use section descriptor GPA\n");
            sev_hashes_table_gpa = 0;  // Not found, will use section descriptor GPA
        } else {
            fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: SEV hashes table GPA=0x%016llx\n", (unsigned long long)sev_hashes_table_gpa);
        }
    }
    
    // Find OVMF_SEV_META_DATA_GUID entry in footer table
    unsigned char *meta_entry_data = NULL;
    size_t meta_entry_len = 0;
    if (parse_ovmf_footer_table_entry(ovmf_path, OVMF_SEV_META_DATA_GUID, 
                                      &meta_entry_data, &meta_entry_len) != 0) {
        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: metadata entry not found in footer table\n");
        return -1;  // Metadata entry not found
    }
    
    fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: found metadata entry, len=%zu\n", meta_entry_len);
    
    if (meta_entry_len < 4) {
        free(meta_entry_data);
        return -1;
    }
    
    // First 4 bytes are offset_from_end (i32, little-endian)
    int32_t offset_from_end = (int32_t)(meta_entry_data[0] |
                                       ((int32_t)meta_entry_data[1] << 8) |
                                       ((int32_t)meta_entry_data[2] << 16) |
                                       ((int32_t)meta_entry_data[3] << 24));
    
    fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: offset_from_end=%d\n", offset_from_end);
    
    free(meta_entry_data);
    
    // Read OVMF file to get metadata header
    FILE *f = fopen(ovmf_path, "rb");
    if (!f) {
        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: failed to open file for header reading\n");
        return -1;
    }
    
    fseek(f, 0, SEEK_END);
    long file_size = ftell(f);
    if (file_size < 0) {
        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: failed to get file size\n");
        fclose(f);
        return -1;
    }
    
    long header_start = file_size - (long)offset_from_end;
    fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: file_size=%ld, header_start=%ld\n", file_size, header_start);
    if (header_start < 0 || header_start >= file_size) {
        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: header_start out of bounds\n");
        fclose(f);
        return -1;
    }
    
    // Read metadata header
    ovmf_metadata_header_t header;
    fseek(f, header_start, SEEK_SET);
    if (fread(&header, 1, sizeof(header), f) != sizeof(header)) {
        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: failed to read header\n");
        fclose(f);
        return -1;
    }
    
    fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: header signature: %c%c%c%c\n", 
            header.signature[0], header.signature[1], header.signature[2], header.signature[3]);
    
    // Verify header signature "ASEV"
    if (memcmp(header.signature, "ASEV", 4) != 0) {
        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: invalid header signature\n");
        fclose(f);
        return -1;
    }
    
    // Verify version is 1
    uint32_t version = header.version & 0xFFFFFFFF;
    uint32_t header_size = header.size & 0xFFFFFFFF;
    uint32_t num_items = header.num_items & 0xFFFFFFFF;
    fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: header version=%u, size=%u, num_items=%u\n", 
            version, header_size, num_items);
    
    if (version != 1) {
        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: invalid header version\n");
        fclose(f);
        return -1;
    }
    
    // Read metadata items
    size_t items_size = header_size - sizeof(ovmf_metadata_header_t);
    fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: items_size=%zu, expected_min=%zu\n", 
            items_size, num_items * sizeof(ovmf_metadata_section_desc_t));
    
    if (items_size < num_items * sizeof(ovmf_metadata_section_desc_t)) {
        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: items_size too small\n");
        fclose(f);
        return -1;
    }
    
    unsigned char *items_data = malloc(items_size);
    if (!items_data) {
        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: malloc failed for items_data\n");
        fclose(f);
        return -1;
    }
    
    if (fread(items_data, 1, items_size, f) != items_size) {
        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: fread failed for items_data\n");
        free(items_data);
        fclose(f);
        return -1;
    }
    fclose(f);
    fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: items_data read successfully\n");
    
    // Process each metadata section
    fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: processing %u sections\n", num_items);
    for (uint32_t i = 0; i < num_items; i++) {
        size_t offset = i * sizeof(ovmf_metadata_section_desc_t);
        if (offset + sizeof(ovmf_metadata_section_desc_t) > items_size) {
            fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: section %u offset out of bounds\n", i);
            break;
        }
        
        ovmf_metadata_section_desc_t *desc = (ovmf_metadata_section_desc_t *)(items_data + offset);
        uint64_t gpa = desc->gpa;
        uint32_t size = desc->size & 0xFFFFFFFF;
        uint8_t section_type = desc->section_type;
        
        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: section %u: type=%u, gpa=0x%016llx, size=%u\n", 
                i, section_type, (unsigned long long)gpa, size);
        
        // Update GCTX based on section type
        switch (section_type) {
            case SECTION_TYPE_SNP_SEC_MEMORY:
                // Zero pages - process page by page (each 4KB)
                fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: updating SNP_SEC_MEMORY at GPA=0x%016llx, size=%u\n", 
                        (unsigned long long)gpa, size);
                for (uint32_t page_offset = 0; page_offset < size; page_offset += PAGE_SIZE) {
                    uint64_t page_gpa = gpa + page_offset;
                    if (gctx_update_page(gctx, PAGE_TYPE_ZERO, page_gpa, NULL, 0) != 0) {
                        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: gctx_update_page failed for SNP_SEC_MEMORY at GPA=0x%016llx\n", 
                                (unsigned long long)page_gpa);
                        free(items_data);
                        return -1;
                    }
                }
                break;
                
            case SECTION_TYPE_SNP_SECRETS:
                // Secrets page - uses 48 bytes of zeros (same as zero page)
                fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: updating SNP_SECRETS at GPA=0x%016llx\n", 
                        (unsigned long long)gpa);
                if (gctx_update_page(gctx, PAGE_TYPE_SECRETS, gpa, NULL, 0) != 0) {
                    fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: gctx_update_page failed for SNP_SECRETS\n");
                    free(items_data);
                    return -1;
                }
                break;
                
            case SECTION_TYPE_CPUID:
                // CPUID page (only for non-EC2 VMM types, or special handling for EC2)
                // Uses 48 bytes of zeros (same as zero page)
                if (vmm_type != 2) {  // Not EC2
                    fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: updating CPUID at GPA=0x%016llx\n", 
                            (unsigned long long)gpa);
                    if (gctx_update_page(gctx, PAGE_TYPE_CPUID, gpa, NULL, 0) != 0) {
                        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: gctx_update_page failed for CPUID\n");
                        free(items_data);
                        return -1;
                    }
                } else {
                    fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: skipping CPUID for EC2 VMM type\n");
                }
                break;
                
            case SECTION_TYPE_SVSM_CAA:
                // Zero pages - process page by page
                fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: updating SVSM_CAA at GPA=0x%016llx, size=%u\n", 
                        (unsigned long long)gpa, size);
                // Process each page individually
                for (uint32_t page_offset = 0; page_offset < size; page_offset += PAGE_SIZE) {
                    uint64_t page_gpa = gpa + page_offset;
                    if (gctx_update_page(gctx, PAGE_TYPE_ZERO, page_gpa, NULL, 0) != 0) {
                        fprintf(stderr, "[SNP_DEBUG] parse_and_update_ovmf_metadata: gctx_update_page failed for SVSM_CAA at GPA=0x%016llx\n", 
                                (unsigned long long)page_gpa);
                        free(items_data);
                        return -1;
                    }
                }
                break;
                
            case SECTION_TYPE_SNP_KERNEL_HASHES:
                // SEV hashes table - handled separately if kernel_hash is provided
                // Rust uses sev_hashes_table_gpa from footer table for page offset,
                // but section descriptor gpa for the actual update
                if (kernel_hash && initrd_hash && append_hash) {
                    // Use SEV hashes table GPA from footer table for page offset (if available)
                    // Otherwise fall back to section descriptor GPA
                    uint64_t offset_gpa = (sev_hashes_table_gpa != 0) ? sev_hashes_table_gpa : (uint64_t)gpa;
                    size_t page_offset = (size_t)(offset_gpa & 0xFFF);
                    // Use section descriptor GPA for the actual update (matches Rust)
                    uint64_t update_gpa = (uint64_t)gpa;
                    uint64_t page_aligned_gpa = update_gpa & ~0xFFFULL;
                    
                    fprintf(stderr, "[SNP_DEBUG] SNP_KERNEL_HASHES: offset_gpa=0x%016llx, page_offset=%zu, update_gpa=0x%016llx, page_aligned_gpa=0x%016llx\n",
                            (unsigned long long)offset_gpa, page_offset, (unsigned long long)update_gpa, (unsigned long long)page_aligned_gpa);
                    
                    unsigned char *sev_hashes_page = malloc(PAGE_SIZE);
                    if (!sev_hashes_page) {
                        free(items_data);
                        return -1;
                    }
                    
                    if (construct_sev_hashes_page(kernel_hash, initrd_hash, append_hash, 
                                                  page_offset, sev_hashes_page) == 0) {
                        fprintf(stderr, "[SNP_DEBUG] SNP_KERNEL_HASHES: constructed page, updating GCTX\n");
                        if (gctx_update_page(gctx, PAGE_TYPE_NORMAL, page_aligned_gpa, 
                                            sev_hashes_page, PAGE_SIZE) != 0) {
                            fprintf(stderr, "[SNP_DEBUG] SNP_KERNEL_HASHES: gctx_update_page failed\n");
                            free(sev_hashes_page);
                            free(items_data);
                            return -1;
                        }
                    } else {
                        fprintf(stderr, "[SNP_DEBUG] SNP_KERNEL_HASHES: construct_sev_hashes_page failed\n");
                    }
                    free(sev_hashes_page);
                } else {
                    fprintf(stderr, "[SNP_DEBUG] SNP_KERNEL_HASHES: no hashes provided, treating as zero page\n");
                    // No hashes provided, treat as zero page
                    if (gctx_update_page(gctx, PAGE_TYPE_ZERO, (uint64_t)gpa, NULL, (size_t)size) != 0) {
                        free(items_data);
                        return -1;
                    }
                }
                break;
                
            default:
                // Unknown section type - skip
                break;
        }
    }
    
    // For EC2 VMM type, process CPUID sections again (special handling)
    if (vmm_type == 2) {  // EC2
        for (uint32_t i = 0; i < num_items; i++) {
            size_t offset = i * sizeof(ovmf_metadata_section_desc_t);
            if (offset + sizeof(ovmf_metadata_section_desc_t) > items_size) {
                break;
            }
            
            ovmf_metadata_section_desc_t *desc = (ovmf_metadata_section_desc_t *)(items_data + offset);
            if (desc->section_type == SECTION_TYPE_CPUID) {
                uint32_t gpa = desc->gpa & 0xFFFFFFFF;
                if (gctx_update_page(gctx, PAGE_TYPE_CPUID, (uint64_t)gpa, NULL, 0) != 0) {
                    free(items_data);
                    return -1;
                }
            }
        }
    }
    
    free(items_data);
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
            fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: initialized with OVMF hash (first 16 bytes): ");
            for (int i = 0; i < 16 && i < LD_BYTES; i++) {
                fprintf(stderr, "%02x", gctx.ld[i]);
            }
            fprintf(stderr, "\n");
        } else {
            fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: OVMF hash hex length invalid (%zu), initializing with zeros\n", hex_len);
            gctx_init(&gctx);
        }
    } else {
        fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: no OVMF hash provided, initializing with zeros\n");
        gctx_init(&gctx);
    }
    
    fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: after GCTX init (first 16 bytes): ");
    for (int i = 0; i < 16 && i < LD_BYTES; i++) {
        fprintf(stderr, "%02x", gctx.ld[i]);
    }
    fprintf(stderr, "\n");
    
    // Update with all OVMF metadata pages
    // This parses the OVMF file and updates GCTX with all metadata sections:
    // - SnpSecMemory (zero pages)
    // - SnpSecrets (secrets page)
    // - Cpuid (CPUID page)
    // - SnpKernelHashes (SEV hashes table)
    // - SvsmCaa (zero pages)
    // Try to find OVMF file in common locations
    // Note: These paths are relative to the current working directory
    // In a release deployment, the working directory may be different
    const char *ovmf_paths[] = {
        "/root/hb-release/test/OVMF-1.55.fd",
        NULL
    };
    
    int metadata_updated = 0;
    for (int i = 0; ovmf_paths[i] != NULL; i++) {
        fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: trying OVMF path %d: %s\n", i, ovmf_paths[i]);
        if (parse_and_update_ovmf_metadata(&gctx, ovmf_paths[i], vmm_type,
                                          kernel_hash, initrd_hash, append_hash) == 0) {
            fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: OVMF metadata updated successfully from %s\n", ovmf_paths[i]);
            metadata_updated = 1;
            break;
        } else {
            fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: OVMF metadata update failed from %s\n", ovmf_paths[i]);
        }
    }
    
    if (!metadata_updated) {
        fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: OVMF metadata parsing failed for all paths\n");
    }
    
    // If OVMF parsing failed but we have SEV hashes GPA, try to update just the hashes table
    if (!metadata_updated && kernel_hash && initrd_hash && append_hash && sev_hashes_gpa != 0) {
        fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: fallback to SEV hashes GPA=0x%016llx\n", (unsigned long long)sev_hashes_gpa);
        unsigned char *sev_hashes_page = (unsigned char *)malloc(PAGE_SIZE);
        if (sev_hashes_page) {
            size_t page_offset = (size_t)(sev_hashes_gpa & 0xFFF);
            uint64_t page_aligned_gpa = sev_hashes_gpa & ~0xFFFULL;
            if (construct_sev_hashes_page(kernel_hash, initrd_hash, append_hash, 
                                          page_offset, sev_hashes_page) == 0) {
                gctx_update_page(&gctx, PAGE_TYPE_NORMAL, page_aligned_gpa, 
                               sev_hashes_page, PAGE_SIZE);
            }
            free(sev_hashes_page);
        }
    }
    
    // Create and update VMSA pages
    // VCPU 0 uses BSP_EIP, VCPUs 1+ use reset EIP from OVMF (if available)
    // Try to extract reset EIP from OVMF
    uint32_t reset_eip = 0;
    if (metadata_updated) {
        // Try to find OVMF file to extract reset EIP
        for (int i = 0; ovmf_paths[i] != NULL; i++) {
            if (parse_ovmf_reset_eip(ovmf_paths[i], &reset_eip) == 0) {
                fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: extracted reset EIP=0x%08x from OVMF\n", reset_eip);
                break;
            }
        }
    }
    
    // Create BSP VMSA page (VCPU 0)
    unsigned char *bsp_vmsa_page = (unsigned char *)malloc(PAGE_SIZE);
    if (!bsp_vmsa_page) {
        return -1;
    }
    
    if (create_vmsa_page(BSP_EIP, vcpu_type, vmm_type, guest_features, bsp_vmsa_page) != 0) {
        free(bsp_vmsa_page);
        return -1;
    }
    
    // Create AP VMSA page (VCPUs 1+) if reset EIP is available
    unsigned char *ap_vmsa_page = NULL;
    if (reset_eip > 0) {
        ap_vmsa_page = (unsigned char *)malloc(PAGE_SIZE);
        if (!ap_vmsa_page) {
            free(bsp_vmsa_page);
            return -1;
        }
        if (create_vmsa_page((uint64_t)reset_eip, vcpu_type, vmm_type, guest_features, ap_vmsa_page) != 0) {
            free(bsp_vmsa_page);
            free(ap_vmsa_page);
            return -1;
        }
    }
    
    // Update with VMSA pages for each VCPU
    // VCPU 0 gets BSP page, VCPUs 1+ get AP page (if available) or BSP page (if not)
    fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: updating GCTX with %u VMSA pages\n", vcpus);
    for (uint32_t i = 0; i < vcpus; i++) {
        unsigned char *vmsa_page_to_use;
        if (i == 0) {
            vmsa_page_to_use = bsp_vmsa_page;
        } else if (ap_vmsa_page) {
            vmsa_page_to_use = ap_vmsa_page;
        } else {
            vmsa_page_to_use = bsp_vmsa_page;  // Fallback to BSP if no AP
        }
        
        // Debug: print first 64 bytes of VMSA page for VCPU 0
        if (i == 0) {
            fprintf(stderr, "[SNP_DEBUG] VMSA page (BSP, first 64 bytes): ");
            for (int j = 0; j < 64; j++) {
                fprintf(stderr, "%02x", vmsa_page_to_use[j]);
            }
            fprintf(stderr, "\n");
        }
        
        if (gctx_update_page(&gctx, PAGE_TYPE_VMSA, VMSA_GPA, vmsa_page_to_use, PAGE_SIZE) != 0) {
            fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: gctx_update_page failed for VCPU %u\n", i);
            free(bsp_vmsa_page);
            if (ap_vmsa_page) free(ap_vmsa_page);
            return -1;
        }
    }
    
    free(bsp_vmsa_page);
    if (ap_vmsa_page) free(ap_vmsa_page);
    
    // Return the final launch digest
    memcpy(output_digest, gctx.ld, LD_BYTES);
    
    fprintf(stderr, "[SNP_DEBUG] compute_launch_digest: final digest computed (first 16 bytes): ");
    for (int i = 0; i < 16 && i < LD_BYTES; i++) {
        fprintf(stderr, "%02x", output_digest[i]);
    }
    fprintf(stderr, "\n");
    
    return 0;
}


