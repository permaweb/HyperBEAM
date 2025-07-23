FROM ubuntu:22.04

# Avoid interactive prompts during package installation
ENV DEBIAN_FRONTEND=noninteractive

# Install required packages
RUN apt update && apt install -y \
    libtss2-dev \
    swtpm \
    swtpm-tools \
    tpm2-tools \
    xxd \
    && rm -rf /var/lib/apt/lists/*

# Create TPM state directory
RUN mkdir -p /tpm-state

# Create and initialize TPM
RUN swtpm_setup --tpmstate /tpm-state --tpm2 --create-ek-cert --create-platform-cert

# Set environment variables
ENV TPM2TOOLS_TCTI="swtpm:host=127.0.0.1,port=2321"

# Expose TPM ports
EXPOSE 2321 2322

# Create startup script
RUN echo '#!/bin/bash\n\
set -e\n\
\n\
# Start swtpm in background with better network binding\n\
swtpm socket --tpmstate dir=/tpm-state --tpm2 \\\n\
  --ctrl type=tcp,port=2322,bindaddr=0.0.0.0 \\\n\
  --server type=tcp,port=2321,bindaddr=0.0.0.0 \\\n\
  --flags not-need-init,startup-clear &\n\
\n\
SWTPM_PID=$!\n\
\n\
# Wait a moment for swtpm to start\n\
sleep 3\n\
\n\
# Initialize TPM\n\
export TPM2TOOLS_TCTI="swtpm:host=127.0.0.1,port=2321"\n\
tpm2_startup -c\n\
\n\
echo "TPM is ready and listening on ports 2321 (data) and 2322 (control)"\n\
echo "TPM2TOOLS_TCTI is set to: $TPM2TOOLS_TCTI"\n\
\n\
# Test TPM from within container (non-fatal)\n\
echo "Testing TPM from container:"\n\
if tpm2_getrandom 8 | xxd; then\n\
  echo "TPM test successful!"\n\
else\n\
  echo "TPM test failed, but continuing..."\n\
fi\n\
\n\
# Keep container running and wait for swtpm process\n\
echo "Container ready. TPM daemon running with PID $SWTPM_PID"\n\
wait $SWTPM_PID\n\
' > /start-tpm.sh && chmod +x /start-tpm.sh

CMD ["/start-tpm.sh"] 