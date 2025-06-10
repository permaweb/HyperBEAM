# **Software Dependencies**

HyperBEAM requires several software packages to be installed on your system. This document provides instructions for installing all required dependencies.

## Base Packages

Install all dependencies with the following command:

```bash
sudo apt-get update && sudo apt-get install -y --no-install-recommends \
    build-essential cmake git pkg-config ncurses-dev \
	libssl-dev sudo curl ca-certificates
``` 

The following software packages are required:

* **build-essential**: Contains basic build tools including gcc/g++ compilers
* **cmake**: Cross-platform build system
* **git**: Version control system
* **pkg-config**: Helper tool for compiling applications and libraries
* **ncurses-dev**: Development libraries for terminal-based interfaces
* **libssl-dev**: Development libraries for SSL (Secure Sockets Layer)
* **sudo**: Allows running programs with security privileges of another user
* **curl**: Command line tool for transferring data with URL syntax
* **ca-certificates**: Common CA certificates for SSL applications

