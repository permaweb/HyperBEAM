#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Copyright (c) 2024 HyperBEAM Contributors. All rights reserved.
#
"""
NVIDIA GPU TEE Attestation Module for HyperBEAM dev_sev_gpu device.
This module provides GPU attestation capabilities through NVIDIA's attestation SDK.
"""

import sys
import json
import os
import logging
from typing import Dict, Any, Optional

try:
    from nv_attestation_sdk import attestation
except ImportError as e:
    print(json.dumps({
        "status": "error",
        "error": f"Failed to import NVIDIA attestation SDK: {str(e)}"
    }))
    sys.exit(1)

# Configure logging
logging.basicConfig(level=logging.ERROR)
attestation_logger = logging.getLogger(__name__)
sdk_logger = logging.getLogger("sdk-logger")
sdk_logger.setLevel(logging.CRITICAL)
decorative_logger = logging.getLogger("nv-attestation-sdk")
decorative_logger.setLevel(logging.CRITICAL)
event_log = logging.getLogger('gpu-verifier-event')
event_log.setLevel(logging.CRITICAL)
info_log = logging.getLogger('gpu-verifier-info')
info_log.setLevel(logging.CRITICAL)


# Default URLs for NVIDIA services
DEFAULT_OCSP_URL = "https://ocsp.ndis.nvidia.com/"
DEFAULT_RIM_URL = "https://rim.attestation.nvidia.com/v1/rim/"


class CCAttestationHandler:
    """Handler for NVIDIA GPU TEE attestation operations."""
    
    def __init__(self):
        self.client = None
        
    def _create_client(self, request_data: Dict[str, Any]) -> attestation.Attestation:
        """Create and configure attestation client."""
        client = attestation.Attestation()
        
        # Set basic client properties
        name = request_data.get("name", "hyperbeam-node")
        client.set_name(name)
        
        # Set nonce if provided
        if "nonce" in request_data:
            client.set_nonce(request_data["nonce"])
            
        # Set claims version
        claims_version = request_data.get("claims_version", "3.0")
        client.set_claims_version(claims_version)
        
        # Set service key if provided
        if "service_key" in request_data:
            client.set_service_key(request_data["service_key"])
            
        return client
    
    def _add_verifier(self, client: attestation.Attestation, request_data: Dict[str, Any]) -> None:
        """Add verifier to the client based on request parameters."""
        device_type = request_data.get("device_type", "gpu").upper()
        environment = request_data.get("environment", "local").upper()
        
        # Map string to enum values
        device_enum = getattr(attestation.Devices, device_type, attestation.Devices.GPU)
        env_enum = getattr(attestation.Environment, environment, attestation.Environment.LOCAL)
        
        # Get service URLs
        remote_url = request_data.get("remote_url", "")
        policy_url = request_data.get("policy_url", "")
        ocsp_url = request_data.get("ocsp_url", DEFAULT_OCSP_URL)
        rim_url = request_data.get("rim_url", DEFAULT_RIM_URL)
        
        # Add verifier with appropriate parameters
        if env_enum == attestation.Environment.LOCAL:
            client.add_verifier(device_enum, env_enum, "", "", ocsp_url, rim_url)
        else:
            client.add_verifier(device_enum, env_enum, remote_url, policy_url, ocsp_url, rim_url)
    
    def generate(self, request_data: Dict[str, Any]) -> Dict[str, Any]:
        """Generate GPU TEE attestation report."""
        try:
            # Create and configure client
            client = self._create_client(request_data)
            self._add_verifier(client, request_data)
            
            # Get evidence
            evidence_list = client.get_evidence()
            if not evidence_list:
                return {
                    "status": "error",
                    "error": "No evidence available for attestation"
                }
            
            # Perform attestation
            attest_result = client.attest(evidence_list)
            if not attest_result:
                return {
                    "status": "error", 
                    "error": "Attestation failed"
                }
            
            # Get the token
            token = client.get_token()
            if not token:
                return {
                    "status": "error",
                    "error": "Failed to get attestation token"
                }
            
            return {
                "status": "ok",
                "result": {
                    "token": token,
                }
            }
            
        except Exception as e:
            attestation_logger.error(f"Generate attestation failed: {str(e)}")
            return {
                "status": "error",
                "error": f"Generate attestation failed: {str(e)}"
            }
    
    def verify(self, request_data: Dict[str, Any]) -> Dict[str, Any]:
        """Verify GPU TEE attestation token against policy."""
        try:
            token = request_data.get("token")
            if not token:
                return {
                    "status": "error",
                    "error": "Token is required for verification"
                }
            
            # Create client for verification
            client = self._create_client(request_data)
            self._add_verifier(client, request_data)
            
            client.set_token(client.get_name(), token)
            
            # Validate token against policy
            policy_file = "./NVGPULocalv4Policy.json"
            with open(os.path.join(os.path.dirname(__file__), policy_file)) as json_file:
                json_data = json.load(json_file)
                att_result_policy = json.dumps(json_data)
            validation_result = client.validate_token(att_result_policy)
            
            return {
                "status": "ok",
                "result": {
                    "valid": validation_result
                }
            }
            
        except Exception as e:
            attestation_logger.error(f"Verify attestation failed: {str(e)}")
            return {
                "status": "error",
                "error": f"Verify attestation failed: {str(e)}"
            } 