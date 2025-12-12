// Certificate verification functions for SEV-SNP

#include "dev_snp_nif.h"
#include <openssl/pem.h>
#include <openssl/x509.h>
#include <openssl/ec.h>
#include <openssl/ecdsa.h>
#include <openssl/evp.h>
#include <openssl/sha.h>
#include <openssl/err.h>
#include <string.h>

// Parse PEM certificate chain (ARK + ASK)
int parse_cert_chain_pem(const unsigned char *pem_data, size_t pem_len,
                        X509 **ark, X509 **ask) {
    BIO *bio = BIO_new_mem_buf(pem_data, pem_len);
    if (!bio) return -1;
    
    STACK_OF(X509) *certs = sk_X509_new_null();
    if (!certs) {
        BIO_free(bio);
        return -1;
    }
    
    // Parse all certificates from PEM
    X509 *cert;
    while ((cert = PEM_read_bio_X509(bio, NULL, NULL, NULL)) != NULL) {
        sk_X509_push(certs, cert);
    }
    
    BIO_free(bio);
    
    int count = sk_X509_num(certs);
    if (count < 2) {
        sk_X509_pop_free(certs, X509_free);
        return -1;
    }
    
    // ASK is the first certificate, ARK is the second (as per SEV spec)
    *ask = sk_X509_value(certs, 0);
    *ark = sk_X509_value(certs, 1);
    
    // Increment reference counts so certs survive stack free
    X509_up_ref(*ask);
    X509_up_ref(*ark);
    
    // Free the stack (certs are now referenced separately)
    sk_X509_pop_free(certs, X509_free);
    
    return 0;
}

// Verify ARK is self-signed
int verify_ark_self_signed(X509 *ark) {
    EVP_PKEY *ark_key = X509_get_pubkey(ark);
    if (!ark_key) return -1;
    
    int ret = X509_verify(ark, ark_key);
    EVP_PKEY_free(ark_key);
    
    return (ret == 1) ? 0 : -1;
}

// Verify ASK is signed by ARK
int verify_ask_signed_by_ark(X509 *ask, X509 *ark) {
    EVP_PKEY *ark_key = X509_get_pubkey(ark);
    if (!ark_key) return -1;
    
    int ret = X509_verify(ask, ark_key);
    EVP_PKEY_free(ark_key);
    
    return (ret == 1) ? 0 : -1;
}

// Verify VCEK is signed by ASK
int verify_vcek_signed_by_ask(X509 *vcek, X509 *ask) {
    EVP_PKEY *ask_key = X509_get_pubkey(ask);
    if (!ask_key) return -1;
    
    int ret = X509_verify(vcek, ask_key);
    EVP_PKEY_free(ask_key);
    
    return (ret == 1) ? 0 : -1;
}

// Verify attestation report signature using VCEK
// The report signature is ECDSA P-384
// Uses OpenSSL 3.0 EVP API (not deprecated low-level APIs)
int verify_report_signature(struct snp_attestation_report *report, X509 *vcek) {
    EVP_PKEY *vcek_key = X509_get_pubkey(vcek);
    if (!vcek_key) return -1;
    
    // Create EVP MD context for SHA-384 hashing
    EVP_MD_CTX *md_ctx = EVP_MD_CTX_new();
    if (!md_ctx) {
        EVP_PKEY_free(vcek_key);
        return -1;
    }
    
    // Hash the report from start to 0x29F (672 bytes)
    // This is the report without the signature field
    unsigned char report_hash[SHA384_DIGEST_LENGTH];
    const EVP_MD *md = EVP_sha384();
    
    if (EVP_DigestInit_ex(md_ctx, md, NULL) != 1) {
        EVP_MD_CTX_free(md_ctx);
        EVP_PKEY_free(vcek_key);
        return -1;
    }
    
    unsigned char *report_bytes = (unsigned char *)report;
    if (EVP_DigestUpdate(md_ctx, report_bytes, 0x2A0) != 1) { // 672 bytes
        EVP_MD_CTX_free(md_ctx);
        EVP_PKEY_free(vcek_key);
        return -1;
    }
    
    unsigned int hash_len = SHA384_DIGEST_LENGTH;
    if (EVP_DigestFinal_ex(md_ctx, report_hash, &hash_len) != 1) {
        EVP_MD_CTX_free(md_ctx);
        EVP_PKEY_free(vcek_key);
        return -1;
    }
    EVP_MD_CTX_free(md_ctx);
    
    // Create ECDSA signature from r and s values
    BIGNUM *r = BN_new();
    BIGNUM *s = BN_new();
    if (!r || !s) {
        BN_free(r);
        BN_free(s);
        EVP_PKEY_free(vcek_key);
        return -1;
    }
    
    // Convert r and s from little-endian to BIGNUM
    // The signature values are stored in little-endian format
    unsigned char r_le[72], s_le[72];
    for (int i = 0; i < 72; i++) {
        r_le[i] = report->signature_r[71 - i];
        s_le[i] = report->signature_s[71 - i];
    }
    
    BN_lebin2bn(r_le, 72, r);
    BN_lebin2bn(s_le, 72, s);
    
    ECDSA_SIG *sig = ECDSA_SIG_new();
    if (!sig) {
        BN_free(r);
        BN_free(s);
        EVP_PKEY_free(vcek_key);
        return -1;
    }
    
    // ECDSA_SIG_set0 takes ownership of r and s
    if (ECDSA_SIG_set0(sig, r, s) != 1) {
        ECDSA_SIG_free(sig);
        BN_free(r);
        BN_free(s);
        EVP_PKEY_free(vcek_key);
        return -1;
    }
    
    // Encode signature to DER format for EVP API
    unsigned char *sig_der = NULL;
    int sig_der_len = i2d_ECDSA_SIG(sig, &sig_der);
    ECDSA_SIG_free(sig);
    
    if (sig_der_len <= 0) {
        EVP_PKEY_free(vcek_key);
        return -1;
    }
    
    // Create EVP context for signature verification
    EVP_MD_CTX *verify_ctx = EVP_MD_CTX_new();
    if (!verify_ctx) {
        OPENSSL_free(sig_der);
        EVP_PKEY_free(vcek_key);
        return -1;
    }
    
    // Initialize verification with SHA-384
    int ret = EVP_DigestVerifyInit(verify_ctx, NULL, md, NULL, vcek_key);
    if (ret == 1) {
        // Verify the signature
        ret = EVP_DigestVerify(verify_ctx, sig_der, sig_der_len, report_hash, SHA384_DIGEST_LENGTH);
    }
    
    OPENSSL_free(sig_der);
    EVP_MD_CTX_free(verify_ctx);
    EVP_PKEY_free(vcek_key);
    
    // EVP_DigestVerify returns 1 on success, 0 on failure
    return (ret == 1) ? 0 : -1;
}

