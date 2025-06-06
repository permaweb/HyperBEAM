# Extending HyperBEAM with Devices

We encourage you to extend HyperBEAM with devices for functionality that is general purpose and reusable across different applications.

## What are Devices?

As explained in [What are HyperBEAM Devices?](../devices/what-are-devices.md), devices are the core functional units within HyperBEAM. They are self-contained modules that process messages and perform specific actions, forming the building blocks of your application's logic.

HyperBEAM comes with a set of powerful [built-in devices](../devices/overview.md) that handle everything from process management (`~process@1.0`) and message scheduling (`~scheduler@1.0`) to executing WebAssembly (`~wasm64@1.0`) and Lua scripts (`~lua@5.3a`).

## Creating Your Own Devices (Coming Soon)

We will create more in depth guides for building devices in Lua and Erlang in the future.
