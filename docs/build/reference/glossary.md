# Developer Glossary

This glossary provides definitions for terms and concepts relevant to building on HyperBEAM.

## AO-Core Protocol
The underlying protocol that HyperBEAM implements, enabling decentralized computing and communication between nodes. AO-Core provides a framework into which any number of different computational models, encapsulated as primitive devices, can be attached.

## Asynchronous Message Passing
A communication paradigm where senders don't wait for receivers to be ready, allowing for non-blocking operations and better scalability.

## Compute Unit (CU)
The NodeJS component of HyperBEAM that executes WebAssembly modules and handles computational tasks.

## Device
A functional unit in HyperBEAM that provides specific capabilities to the system, such as storage, networking, or computational resources.

## Hashpaths
A mechanism for referencing locations in a program's state-space prior to execution. These state-space links are represented as Merklized lists of programs inputs and initial states.

## Message
A data structure used for communication between processes in the HyperBEAM system. Messages can be interpreted as a binary term or as a collection of named functions (a Map of functions).

## Module
A unit of code that can be loaded and executed by the Compute Unit, typically in WebAssembly format.

## Process
An independent unit of computation in HyperBEAM with its own state and execution context.

## Process ID
A unique identifier assigned to a process within the HyperBEAM system.

## WebAssembly (WASM)
A binary instruction format that serves as a portable compilation target for programming languages, enabling deployment on the web and other environments.

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
     src="https://glossary.arweave.net/?hide-header=true&bg-color=%231F2129&text-color=%23e0e0e0&heading-color=%23ffffff&border-color=%23444444&hover-bg=%23222222&button-text=%23ffffff&section-bg=%23333333&section-color=%23ffffff&category-bg=%23333333&category-text=%23ffffff&tag-text=%23ffffff&secondary-text=%23a0a0a0&result-bg=%231e1e1e&result-hover=%23333333"
     >
    </iframe>
  </div>
</div> 