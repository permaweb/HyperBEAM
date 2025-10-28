#!/usr/bin/env python3
# -*- coding: utf-8 -*-
#
# Copyright (c) 2024 HyperBEAM Contributors. All rights reserved.
#
"""
Main entry point for HyperBEAM dev_sev_gpu_nif attestation service.
"""

import sys
import json
import logging
from cc_attestation import CCAttestationHandler

# Configure logging
logging.basicConfig(level=logging.ERROR)
main_logger = logging.getLogger(__name__)


def main():
    """Main entry point for the attestation service."""
    try:
        # Read JSON input from stdin
        input_data = sys.stdin.read().strip()
        if not input_data:
            print(json.dumps({
                "status": "error",
                "error": "No input data provided"
            }))
            return
        
        # Parse JSON request
        try:
            request = json.loads(input_data)
        except json.JSONDecodeError as e:
            print(json.dumps({
                "status": "error", 
                "error": f"Invalid JSON input: {str(e)}"
            }))
            return
        
        # Get action
        action = request.get("action")
        if not action:
            print(json.dumps({
                "status": "error",
                "error": "Action is required (generate or verify)"
            }))
            return
        
        # Create handler and process request
        handler = CCAttestationHandler()
        
        if action == "generate":
            result = handler.generate(request.get("data", {}))
        elif action == "verify":
            result = handler.verify(request.get("data", {}))
        else:
            result = {
                "status": "error",
                "error": f"Unknown action: {action}. Supported actions: generate, verify"
            }
        
        # Output result as JSON
        print(json.dumps(result))
        
    except Exception as e:
        main_logger.error(f"Script execution failed: {str(e)}")
        print(json.dumps({
            "status": "error",
            "error": f"Script execution failed: {str(e)}"
        }))


if __name__ == "__main__":
    main()
