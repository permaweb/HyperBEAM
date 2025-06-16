# Node Operator FAQ

This page answers common questions about running and maintaining a HyperBEAM node.

### What is HyperBEAM?

HyperBEAM is a client implementation of the AO-Core protocol written in Erlang. It serves as the node software for a decentralized operating system that allows operators to offer computational resources to users in the AO network.

### What are the system requirements for running HyperBEAM?

Currently, HyperBEAM is primarily tested and documented for Ubuntu 22.04 and macOS. Other platforms will be added in future updates. For detailed requirements, see the [System Requirements](../../run/configuring-your-machine.md) page.

### Can I run HyperBEAM in a container?

While technically possible, running HyperBEAM in Docker containers or other containerization technologies is currently not recommended. The containerization approach may introduce additional complexity and potential performance issues. We recommend running HyperBEAM directly on the host system until container support is more thoroughly tested and optimized.

### How do I update HyperBEAM to the latest version?

To update HyperBEAM:

1. Pull the latest code from the repository
2. Rebuild the application
3. Restart the HyperBEAM service

Specific update instructions will vary depending on your [installation method](../../run/running-a-hyperbeam-node.md).

### Can I run multiple HyperBEAM nodes on a single machine?

Yes, you can run multiple HyperBEAM nodes on a single machine, but you'll need to configure them to use different ports and data directories to avoid conflicts. However, this is not recommended for production environments as each node should ideally have a unique IP address to properly participate in the network. Running multiple nodes on a single machine is primarily useful for development and testing purposes.

### Is there a limit to how many processes can run on a node?

The practical limit depends on your hardware resources. Erlang is designed to handle millions of lightweight processes efficiently, but the actual number will be determined by:

- Available memory
- CPU capacity
- Network bandwidth
- Storage speed
- The complexity of your processes

### Where can I get help if I encounter issues?

If you encounter issues:

- Check the [Troubleshooting](troubleshooting.md) guide
- Search or ask questions on [GitHub Issues](https://github.com/permaweb/HyperBEAM/issues)
- Join the community on [Discord](https://discord.gg/V3yjzrBxPM) 