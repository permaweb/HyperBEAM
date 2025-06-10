# **Installing Node.js**

Node.js is required for running a legacynet compatible local Compute Unit (CU) with the delegated-compute@1.0 device, if you plan to do so. This guide covers installing Node.js on Ubuntu 22.04.

## Installing Node.js 22.x

We recommend using Node.js version 22.x for optimal compatibility with the CU:

```bash
curl -fsSL https://deb.nodesource.com/setup_22.x | sudo -E bash - && \
sudo apt-get install -y nodejs
```

## Verify Installation

Verify that Node.js and npm are installed correctly:

```bash
node -v
npm -v
```

These commands should display the installed versions of Node.js and npm respectively.
