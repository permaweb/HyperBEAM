#!/bin/bash

# Exit on any error
set -e

echo "Building project..."
./build.sh

echo "Creating systemd service file..."
cat > /etc/systemd/system/hyperbeam.service << 'EOF'
[Unit]
Description=HyperBEAM Service
After=network.target

[Service]
Type=simple
User=root
WorkingDirectory=/opt/load_hb
Environment=SHELL=/bin/bash
Environment=HOME=/root
Environment=LANG=en_US.UTF-8
Environment=PATH=/root/.cargo/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin
Environment=DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/0/bus
ExecStart=/usr/local/bin/rebar3 shell
Restart=always
RestartSec=5

[Install]
WantedBy=multi-user.target
EOF

echo "Reloading systemd..."
systemctl daemon-reload

echo "Enabling and starting service..."
systemctl enable hyperbeam
systemctl restart hyperbeam

echo "Service status:"
systemctl status hyperbeam

echo "Deployment complete! Check logs with: journalctl -fu hyperbeam"
