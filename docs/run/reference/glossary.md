# Node Operator Glossary

This glossary provides definitions for terms and concepts relevant to running a HyperBEAM node.

## AO-Core Protocol
The underlying protocol that HyperBEAM implements, enabling decentralized computing and communication between nodes.

## Checkpoint
A saved state of a process that can be used to resume execution from a known point, used for persistence and recovery.

## Compute Unit (CU)
The NodeJS component of HyperBEAM that executes WebAssembly modules. While developers interact with it more, operators should know it's a key part of the stack.

## Erlang
The programming language used to implement the HyperBEAM core, known for its robustness and support for building distributed, fault-tolerant applications.

## ~flat@1.0
A format used for encoding settings files in HyperBEAM configuration, using HTTP header styling.

## HyperBEAM
The Erlang-based node software that handles message routing, process management, and device coordination in the HyperBEAM ecosystem.

## Node
An instance of HyperBEAM running on a physical or virtual machine that participates in the distributed network.

## ~meta@1.0
A device used to configure the node's hardware, supported devices, metering and payments information, amongst other configuration options.

## ~p4@1.0
A device that runs as a pre-processor and post-processor in HyperBEAM, enabling a framework for node operators to sell usage of their machine's hardware to execute AO-Core devices.

## ~simple-pay@1.0
A simple, flexible pricing device that can be used in conjunction with p4@1.0 to offer flat-fees for the execution of AO-Core messages.

## ~snp@1.0
A device used to generate and validate proofs that a node is executing inside a Trusted Execution Environment (TEE).

## Trusted Execution Environment (TEE)
A secure area inside a processor that ensures the confidentiality and integrity of code and data loaded within it. Used in HyperBEAM for trust-minimized computation.

## Permaweb Glossary

For a more comprehensive glossary of terms used in the permaweb, try the [Permaweb Glossary](https://glossary.arweave.net). Or use it below:


<style>
.dark-mode-iframe-container { display: none; }
.light-mode-iframe-container { display: block; }
[data-md-color-scheme="slate"] .light-mode-iframe-container { display: none; }
[data-md-color-scheme="slate"] .dark-mode-iframe-container { display: block; }

/* Explicitly handle default scheme */
[data-md-color-scheme="default"] .dark-mode-iframe-container { display: none; }
[data-md-color-scheme="default"] .light-mode-iframe-container { display: block; }
</style>

<div class="mt-6">
  <div class="light-mode-iframe-container">
    <iframe 
     id="glossary-frame-light" 
     src="https://glossary.arweave.net/?hide-header=true&bg-color=%23FFFFFF&text-color=%231A1A1A&heading-color=%233C3C3C&tag-text=%23ffffff&button-text=%23ffffff"
     width="100%" 
     height="400" 
     frameborder="0" 
     scrolling="no">
    </iframe>
  </div>
  <div class="dark-mode-iframe-container">
    <iframe 
     class="w-full"
	 width="100%" 
     height="400"  
	 frameborder="0" 
	 scrolling="no"
     src="https://glossary.arweave.net/?hide-header=true&bg-color=%23000000&text-color=%23e0e0e0&heading-color=%23ffffff&border-color=%23444444&hover-bg=%23222222&button-text=%23ffffff&section-bg=%23333333&section-color=%23ffffff&category-bg=%23333333&category-text=%23ffffff&tag-text=%23ffffff&secondary-text=%23a0a0a0&result-bg=%231e1e1e&result-hover=%23333333"
     >
    </iframe>
  </div>
</div> 