#ifndef DEV_SNP_NIF_H
#define DEV_SNP_NIF_H

#include "erl_nif.h"
#include <linux/types.h>
#include <openssl/x509.h>

// AttestationReport structure (1184 bytes) - matches SEV crate
struct snp_attestation_report {
    __u32 version;
    __u32 guest_svn;
    __u64 policy;
    __u8 family_id[16];
    __u8 image_id[16];
    __u32 vmpl;
    __u32 sig_algo;
    __u8 current_tcb[8];  // TcbVersion: 4 u8s + 4 reserved
    __u64 plat_info;
    __u32 _author_key_en;
    __u32 _reserved_0;
    __u8 report_data[64];
    __u8 measurement[48];
    __u8 host_data[32];
    __u8 id_key_digest[48];
    __u8 author_key_digest[48];
    __u8 report_id[32];
    __u8 report_id_ma[32];
    __u8 reported_tcb[8];
    __u8 _reserved_1[24];
    __u8 chip_id[64];
    __u8 committed_tcb[8];
    __u8 current_build;
    __u8 current_minor;
    __u8 current_major;
    __u8 _reserved_2;
    __u8 committed_build;
    __u8 committed_minor;
    __u8 committed_major;
    __u8 _reserved_3;
    __u8 launch_tcb[8];
    __u8 _reserved_4[168];
    __u8 signature_r[72];
    __u8 signature_s[72];
    __u8 signature_reserved[368];
};

// Certificate verification functions
int parse_cert_chain_pem(const unsigned char *pem_data, size_t pem_len,
                        X509 **ark, X509 **ask);
int verify_ark_self_signed(X509 *ark);
int verify_ask_signed_by_ark(X509 *ask, X509 *ark);
int verify_vcek_signed_by_ask(X509 *vcek, X509 *ask);
int verify_report_signature(struct snp_attestation_report *report, X509 *vcek);

#endif

