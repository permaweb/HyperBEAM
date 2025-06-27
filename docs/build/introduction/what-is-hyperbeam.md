# What is HyperBEAM?

<div style="width: 100%; max-width: 100vw; margin-bottom: 2em;">
  <video
    class="theme-invert-video"
    src="https://arweave.net/pc73dj9tZtj7AOeIKBGiiOm5ta13FYXzgsqWSePAxiM"
    style="width: 100%; height: auto; display: block;"
    autoplay
    muted
    playsinline
    loop
    controlslist="nodownload nofullscreen noremoteplayback"
    disablepictureinpicture
    preload="auto"
  ></video>
</div>

HyperBEAM is the primary, production-ready implementation of the [AO-Core protocol](./what-is-ao-core.md), built on the robust Erlang/OTP framework. It serves as a decentralized operating system, powering the [AO Computer](https://ao.arweave.net)—a scalable, trust-minimized, distributed supercomputer built on permanent storage of [Arweave](https://arweave.org).

## **Implementing AO-Core**
HyperBEAM transforms the abstract concepts of AO-Core—Messages, Devices, and Paths—into a concrete, operational system. It provides the runtime environment and essential services to execute these 
computations across a network of distributed nodes.

<div class='core-concepts-flex'>
<svg class='core-concepts-fig messages' width="100" height="100" viewBox="0 0 100 100" fill="none" xmlns="http://www.w3.org/2000/svg">
<path d="M89.8006 49.1685C89.8011 51.737 88.1042 54.3055 84.71 56.2651C77.9224 60.1839 66.9177 60.1839 60.1301 56.2651C56.7368 54.3059 55.0399 51.7382 55.0395 49.1705C55.0391 46.602 56.736 44.0336 60.1301 42.0739C66.9177 38.1551 77.9224 38.1551 84.71 42.0739C88.1033 44.0331 89.8002 46.6008 89.8006 49.1685Z" fill="#1E1E1B"/>
<path d="M84.71 35.8006C77.9225 31.8818 66.9177 31.8818 60.1302 35.8006V42.0739C66.9177 38.1551 77.9225 38.1551 84.71 42.0739V35.8006Z" fill="#1E1E1B"/>
<path d="M89.8006 42.8952C89.8002 40.3274 88.1033 37.7597 84.71 35.8005V42.0738C88.1033 44.0329 89.8002 46.6007 89.8006 49.1684V42.8952Z" fill="#1E1E1B"/>
<path d="M60.1301 35.8005C56.736 37.7602 55.0391 40.3287 55.0395 42.8971L55.0395 49.1684C55.0399 46.6006 56.7368 44.0329 60.1301 42.0738V35.8005Z" fill="#1E1E1B"/>
<path d="M84.71 49.9918C88.1033 48.0327 89.8002 45.465 89.8006 42.8972V49.1685C89.801 51.7369 88.1041 54.3054 84.71 56.2651V49.9918Z" fill="#1E1E1B"/>
<path d="M55.0395 42.8972C55.0399 45.465 56.7368 48.0327 60.1301 49.9919V56.2651C56.7368 54.306 55.0399 51.7383 55.0395 49.1705V42.8972Z" fill="#1E1E1B"/>
<circle cx="14.1912" cy="14.1912" r="14.1912" transform="matrix(0.866025 0.5 -0.866025 0.5 72.4201 28.7051)" fill="#E8E8E8"/>
<path d="M36.3738 60.2635L58.105 72.8101L36.3738 85.3566L14.6426 72.8101L36.3738 60.2635Z" fill="#1E1E1B"/>
<path d="M36.3738 55.245L14.6426 67.7915V72.8101L36.3738 60.2636V55.245Z" fill="#1E1E1B"/>
<path d="M58.105 67.7915L36.3738 55.245V60.2636L58.105 72.8101V67.7915Z" fill="#1E1E1B"/>
<path d="M14.6426 67.7914L36.3738 80.338V85.3566L14.6426 72.8101V67.7914Z" fill="#1E1E1B"/>
<path d="M36.3738 80.338L58.105 67.7914V72.8101L36.3738 85.3566V80.338Z" fill="#1E1E1B"/>
<rect width="25.093" height="25.093" transform="matrix(0.866025 0.5 -0.866025 0.5 36.3738 55.245)" fill="#E8E8E8"/>
<path d="M50.6585 30.217L40.3758 52.3733L12.2828 36.1538L50.6585 30.217Z" fill="#1E1E1B"/>
<path d="M50.6585 24.3643L12.2828 30.3011V36.1538L50.6585 30.217V24.3643Z" fill="#1E1E1B"/>
<path d="M40.3758 46.5206L50.6585 24.3643V30.217L40.3758 52.3733V46.5206Z" fill="#1E1E1B"/>
<path d="M12.2828 30.3011L40.3758 46.5206V52.3733L12.2828 36.1538V30.3011Z" fill="#1E1E1B"/>
<path d="M50.6585 24.3643L40.3757 46.5206L12.2827 30.3011L50.6585 24.3643Z" fill="#E8E8E8"/>
</svg>
<div class='core-concepts-column'>
<p class="core-concept-header-messages"><b>Messages</b></p>
<span class="core-concept-subtitle">Modular Data Packets</span>
<p>In HyperBEAM, every interaction within the AO Computer is handled as a <b>message</b>. A message is a binary item or a map of functions. These cryptographically-linked data units are the foundation for communication, allowing processes to trigger computations, query state, and transfer value. HyperBEAM nodes are responsible for routing and processing these messages according to the rules of the AO-Core protocol.</p>
</div>
</div>

<div class='core-concepts-flex'>
<svg class='core-concepts-fig devices' width="100" height="100" viewBox="0 0 100 100" fill="none" xmlns="http://www.w3.org/2000/svg">
<path d="M26.3709 37.516L45.4764 26.8021V48.2299L26.3496 58.9437L26.3709 37.516Z" fill="#1E1E1B"/>
<path d="M24.0487 17L4.92188 27.7139L26.3709 37.516L45.4764 26.8021L24.0487 17Z" fill="#E8E8E8"/>
<path d="M4.92188 27.7139V49.1416L26.3496 58.9437L26.3709 37.516L4.92188 27.7139Z" fill="#1E1E1B"/>
<path d="M13.1453 56.2997L5.52424 60.6997M13.1453 56.2997L10.0969 56.2997M13.1453 56.2997L13.1453 58.0597" stroke="#1E1E1B" stroke-linecap="square"/>
<path d="M51.3336 35.4171L43.7125 39.8171M51.3336 35.4171L48.2851 35.4171M51.3336 35.4171L51.3336 37.1771" stroke="#1E1E1B" stroke-linecap="square"/>
<path d="M69.5132 59.6402L88.6187 48.9263V70.3541L69.4919 81.0679L69.5132 59.6402Z" fill="#1E1E1B"/>
<path d="M67.191 39.1242L48.0642 49.8381L69.5132 59.6402L88.6187 48.9263L67.191 39.1242Z" fill="#E8E8E8"/>
<path d="M48.0642 49.8381V71.2658L69.4919 81.0679L69.5132 59.6402L48.0642 49.8381Z" fill="#1E1E1B"/>
<path d="M56.2876 78.424L48.6665 82.824M56.2876 78.424L53.2391 78.424M56.2876 78.424L56.2876 80.184" stroke="#1E1E1B" stroke-linecap="square"/>
<path d="M94.4758 57.5413L86.8547 61.9414M94.4758 57.5413L91.4273 57.5413M94.4758 57.5413L94.4758 59.3013" stroke="#1E1E1B" stroke-linecap="square"/>
</svg>
<div class='core-concepts-column'>
<p class="core-concept-header-devices"><b>Devices</b></p>
<span class="core-concept-subtitle">Extensible Execution Engines</span>
<p>HyperBEAM introduces a uniquely modular architecture centered around <b>Devices</b>. These pluggable components are Erlang modules that define specific computational logic—like running WASM, managing state, or relaying data—allowing for unprecedented flexibility. This design allows developers to extend the system by creating custom Devices to fit their specific computational needs.</p>
</div>
</div>

<div class='core-concepts-flex'>
<svg class='core-concepts-fig paths' width="100" height="100" viewBox="0 0 100 100" fill="none" xmlns="http://www.w3.org/2000/svg">
<path d="M24.9163 66.7032L17.5867 70.935M24.9163 66.7032H21.9844M24.9163 66.7032L24.9163 68.3959M17.5867 70.935L17.5867 69.2423M17.5867 70.935H20.5185" stroke="#1E1E1B" stroke-linecap="square"/>
<path d="M17.5867 33.6953L24.9163 37.9271M17.5867 33.6953L17.5867 35.388M17.5867 33.6953H20.5185M24.9163 37.9271H21.9845M24.9163 37.9271L24.9163 36.2344" stroke="#1E1E1B" stroke-linecap="square"/>
<path d="M3 76.0131L8.86372 79.3986L8.86374 75.1668L3.00001 71.7814L3 76.0131Z" fill="#1E1E1B"/>
<path d="M8.86372 79.3986L18.8851 73.6127L18.8851 69.3809L15.3704 71.4102L8.86374 75.1668L8.86372 79.3986Z" fill="#1E1E1B"/>
<path d="M3.00001 71.7814L8.86374 75.1668L15.3704 71.4102L18.8851 69.3809L13.2615 65.8569L3.00001 71.7814Z" fill="#E8E8E8"/>
<path d="M29.4829 67.4941L96.8196 28.6172L96.8196 24.3854L29.4829 63.2623L29.4829 67.4941Z" fill="#1E1E1B"/>
<path d="M90.9559 21L49.9098 44.698L29.4829 32.9045L23.6192 36.2899L27.284 38.4058L44.0461 48.0834L40.3813 50.1993L23.5231 59.9324L27.248 62.0136L29.4829 63.2623L96.8196 24.3854L90.9559 21Z" fill="#E8E8E8"/>
<path d="M40.3813 50.1993L44.0461 48.0834L27.284 38.4058L23.6192 36.2899L23.6192 40.5217L40.3813 50.1993Z" fill="#1E1E1B"/>
<path d="M8.86374 21L3.00221 24.3854L13.2615 30.3099L15.7047 28.8993L19.1253 26.9245L8.86374 21Z" fill="#E8E8E8"/>
<path d="M3.0022 28.6172L13.2615 34.5417L13.2615 30.3099L3.00221 24.3854L3.0022 28.6172Z" fill="#1E1E1B"/>
<path d="M23.5231 59.9324L23.523 64.1641L27.248 62.0136L23.5231 59.9324Z" fill="#4E4E48"/>
<path d="M23.523 64.1641L29.4829 67.4941L29.4829 63.2623L27.248 62.0136L23.523 64.1641Z" fill="black"/>
<path fill-rule="evenodd" clip-rule="evenodd" d="M19.1252 31.1563L13.2615 34.5417L13.2615 30.3099L15.7047 28.8993L19.1252 31.1563Z" fill="black"/>
<path d="M19.1252 31.1563L19.1253 26.9245L15.7047 28.8993L19.1252 31.1563Z" fill="#4E4E48"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 31.8615 64.6397)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 34.4408 61.6613)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 33.796 62.7782)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 33.1511 63.8951)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 32.5063 64.2674)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 35.0857 61.289)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 35.7305 60.9167)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 36.3754 60.5444)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 37.0202 61.6613)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 35.0857 62.0336)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 35.7305 62.4059)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 31.2166 63.5228)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 30.5718 65.3843)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 37.0202 60.9167)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
<rect x="0.161211" y="0.093075" width="0.3723" height="0.3723" transform="matrix(0.866025 -0.5 0 1 32.5063 63.5228)" fill="#F7F7F7" stroke="#1E1E1B" stroke-width="0.3723"/>
</svg>
<div class='core-concepts-column'>
<p class="core-concept-header-paths"><b>Paths</b></p>
<span class="core-concept-subtitle">Composable Pipelines</span>
<p>HyperBEAM exposes a powerful HTTP API that uses structured URL patterns to interact with processes and data. This <b>pathing mechanism</b> allows developers to create verifiable data pipelines, composing functionality from multiple devices into a single, atomic request. The URL bar effectively becomes a command-line interface for AO's trustless compute environment.</p>
</div>
</div>

## A Robust and Scalable Foundation

Built on the Erlang/OTP framework, HyperBEAM provides a robust and secure foundation that leverages the BEAM virtual machine for exceptional concurrency, fault tolerance, and scalability. This abstracts away underlying hardware, allowing diverse nodes to contribute resources without compatibility issues. The system governs how nodes coordinate and interact, forming a decentralized network that is resilient and permissionless.