// Renders the docs pages of a node in the browser. The node gives the same
// page for every device. This script gets the info of the page's device from
// the node, with the schema and the specification of the device in it, then
// the recipes and the implementations of the device, and renders them.
(function () {
  const jsonType = "application/json";
  const homeUrl = "/~docs@1.0";
  const assetUrl = `${homeUrl}/asset`;
  const esc = HBMarkdown.escape;
  const article = document.getElementById("main");
  const sidebarList = document.querySelector("#sidebar .sidebar-nav > ul");
  const homeLinks = ".site-header-home, .mobile-nav-home";
  const missingRecipeSummary =
    "Recipe body could not be loaded from the weave.";
  const communityPill =
    `<span class="pill pill-required">Community recipe</span>`;
  const communityCaution =
    `<p>${communityPill} Someone other than the signer of this device's ` +
    "specification made this recipe. Check each request before you run it.</p>";
  const runCaution =
    `<p>${communityPill} You are about to run a community created path. ` +
    "Click again to run it.</p>";
  const recipeQuery = `query($type: [String!]!, $specid: [String!]!) {
    transactions(
      tags: [
        { name: "type", values: $type },
        { name: "recipe-for-device", values: $specid }
      ],
      first: 20
    ) {
      edges { node { id owner { address } tags { name value } } }
    }
  }`;
  const guideSections = ["Introduction", "Processes", "Device Forge"];
  const nodeIndexItems = [
    ["schema", "Schema"],
    ["spec", "Spec"],
    ["recipes", "Recipes"],
    ["implementations", "Implementations"]
  ];
  const concepts = [
    ["ao-core", "The message execution model where each path segment " +
      "applies a request message to a base message."],
    ["devices", "Versioned interpreters that expose computable keys over " +
      "messages."],
    ["hyperbeam", "A node runtime for AO-Core messages, devices, and " +
      "path-addressed computation."],
    ["messages", "Typed key/value structures that carry data, commitments, " +
      "device names, and execution history."]
  ];
  const iconPaths = {
    package:
      "M223.68,66.15,135.68,18a15.88,15.88,0,0,0-15.36,0l-88,48.17a16,16,0,0,0-8.32,14v95.64a16,16,0,0,0,8.32,14l88,48.17a15.88,15.88,0,0,0,15.36,0l88-48.17a16,16,0,0,0,8.32-14V80.18A16,16,0,0,0,223.68,66.15ZM128,32l80.34,44-29.77,16.3-80.35-44ZM128,120,47.66,76l33.9-18.56,80.34,44ZM40,90l80,43.78v85.79L40,175.82Zm176,85.78h0l-80,43.79V133.82l32-17.51V152a8,8,0,0,0,16,0V107.55L216,90v85.77Z",
    upload:
      "M240,136v64a16,16,0,0,1-16,16H32a16,16,0,0,1-16-16V136a16,16,0,0,1,16-16H80a8,8,0,0,1,0,16H32v64H224V136H176a8,8,0,0,1,0-16h48A16,16,0,0,1,240,136ZM85.66,77.66,120,43.31V128a8,8,0,0,0,16,0V43.31l34.34,34.35a8,8,0,0,0,11.32-11.32l-48-48a8,8,0,0,0-11.32,0l-48,48A8,8,0,0,0,85.66,77.66ZM200,168a12,12,0,1,0-12,12A12,12,0,0,0,200,168Z",
    "book-open":
      "M232,48H160a40,40,0,0,0-32,16A40,40,0,0,0,96,48H24a8,8,0,0,0-8,8V200a8,8,0,0,0,8,8H96a24,24,0,0,1,24,24,8,8,0,0,0,16,0,24,24,0,0,1,24-24h72a8,8,0,0,0,8-8V56A8,8,0,0,0,232,48ZM96,192H32V64H96a24,24,0,0,1,24,24V200A39.81,39.81,0,0,0,96,192Zm128,0H160a39.81,39.81,0,0,0-24,8V88a24,24,0,0,1,24-24h64Z",
    stack:
      "M230.91,172A8,8,0,0,1,228,182.91l-96,56a8,8,0,0,1-8.06,0l-96-56A8,8,0,0,1,36,169.09l92,53.65,92-53.65A8,8,0,0,1,230.91,172ZM220,121.09l-92,53.65L36,121.09A8,8,0,0,0,28,134.91l96,56a8,8,0,0,0,8.06,0l96-56A8,8,0,1,0,220,121.09ZM24,80a8,8,0,0,1,4-6.91l96-56a8,8,0,0,1,8.06,0l96,56a8,8,0,0,1,0,13.82l-96,56a8,8,0,0,1-8.06,0l-96-56A8,8,0,0,1,24,80Zm23.88,0L128,126.74,208.12,80,128,33.26Z",
    crosshair:
      "M232,120h-8.34A96.14,96.14,0,0,0,136,32.34V24a8,8,0,0,0-16,0v8.34A96.14,96.14,0,0,0,32.34,120H24a8,8,0,0,0,0,16h8.34A96.14,96.14,0,0,0,120,223.66V232a8,8,0,0,0,16,0v-8.34A96.14,96.14,0,0,0,223.66,136H232a8,8,0,0,0,0-16Zm-96,87.6V200a8,8,0,0,0-16,0v7.6A80.15,80.15,0,0,1,48.4,136H56a8,8,0,0,0,0-16H48.4A80.15,80.15,0,0,1,120,48.4V56a8,8,0,0,0,16,0V48.4A80.15,80.15,0,0,1,207.6,120H200a8,8,0,0,0,0,16h7.6A80.15,80.15,0,0,1,136,207.6ZM128,88a40,40,0,1,0,40,40A40,40,0,0,0,128,88Zm0,64a24,24,0,1,1,24-24A24,24,0,0,1,128,152Z",
    "magnifying-glass":
      "M229.66,218.34l-50.07-50.06a88.11,88.11,0,1,0-11.31,11.31l50.06,50.07a8,8,0,0,0,11.32-11.32ZM40,112a72,72,0,1,1,72,72A72.08,72.08,0,0,1,40,112Z",
    export:
      "M216,112v96a16,16,0,0,1-16,16H56a16,16,0,0,1-16-16V112A16,16,0,0,1,56,96H80a8,8,0,0,1,0,16H56v96H200V112H176a8,8,0,0,1,0-16h24A16,16,0,0,1,216,112ZM93.66,69.66,120,43.31V136a8,8,0,0,0,16,0V43.31l26.34,26.35a8,8,0,0,0,11.32-11.32l-40-40a8,8,0,0,0-11.32,0l-40,40A8,8,0,0,0,93.66,69.66Z",
    key:
      "M216.57,39.43A80,80,0,0,0,83.91,120.78L28.69,176A15.86,15.86,0,0,0,24,187.31V216a16,16,0,0,0,16,16H72a8,8,0,0,0,8-8V208H96a8,8,0,0,0,8-8V184h16a8,8,0,0,0,5.66-2.34l9.56-9.57A79.73,79.73,0,0,0,160,176h.1A80,80,0,0,0,216.57,39.43ZM224,98.1c-1.09,34.09-29.75,61.86-63.89,61.9H160a63.7,63.7,0,0,1-23.65-4.51,8,8,0,0,0-8.84,1.68L116.69,168H96a8,8,0,0,0-8,8v16H72a8,8,0,0,0-8,8v16H40V187.31l58.83-58.82a8,8,0,0,0,1.68-8.84A63.72,63.72,0,0,1,96,95.92c0-34.14,27.81-62.8,61.9-63.89A64,64,0,0,1,224,98.1ZM192,76a12,12,0,1,1-12-12A12,12,0,0,1,192,76Z",
    "seal-check":
      "M225.86,102.82c-3.77-3.94-7.67-8-9.14-11.57-1.36-3.27-1.44-8.69-1.52-13.94-.15-9.76-.31-20.82-8-28.51s-18.75-7.85-28.51-8c-5.25-.08-10.67-.16-13.94-1.52-3.56-1.47-7.63-5.37-11.57-9.14C146.28,23.51,138.44,16,128,16s-18.27,7.51-25.18,14.14c-3.94,3.77-8,7.67-11.57,9.14C88,40.64,82.56,40.72,77.31,40.8c-9.76.15-20.82.31-28.51,8S41,67.55,40.8,77.31c-.08,5.25-.16,10.67-1.52,13.94-1.47,3.56-5.37,7.63-9.14,11.57C23.51,109.72,16,117.56,16,128s7.51,18.27,14.14,25.18c3.77,3.94,7.67,8,9.14,11.57,1.36,3.27,1.44,8.69,1.52,13.94.15,9.76.31,20.82,8,28.51s18.75,7.85,28.51,8c5.25.08,10.67.16,13.94,1.52,3.56,1.47,7.63,5.37,11.57,9.14C109.72,232.49,117.56,240,128,240s18.27-7.51,25.18-14.14c3.94-3.77,8-7.67,11.57-9.14,3.27-1.36,8.69-1.44,13.94-1.52,9.76-.15,20.82-.31,28.51-8s7.85-18.75,8-28.51c.08-5.25.16-10.67,1.52-13.94,1.47-3.56,5.37-7.63,9.14-11.57C232.49,146.28,240,138.44,240,128S232.49,109.73,225.86,102.82Zm-11.55,39.29c-4.79,5-9.75,10.17-12.38,16.52-2.52,6.1-2.63,13.07-2.73,19.82-.1,7-.21,14.33-3.32,17.43s-10.39,3.22-17.43,3.32c-6.75.1-13.72.21-19.82,2.73-6.35,2.63-11.52,7.59-16.52,12.38S132,224,128,224s-9.15-4.92-14.11-9.69-10.17-9.75-16.52-12.38c-6.1-2.52-13.07-2.63-19.82-2.73-7-.1-14.33-.21-17.43-3.32s-3.22-10.39-3.32-17.43c-.1-6.75-.21-13.72-2.73-19.82-2.63-6.35-7.59-11.52-12.38-16.52S32,132,32,128s4.92-9.15,9.69-14.11,9.75-10.17,12.38-16.52c2.52-6.1,2.63-13.07,2.73-19.82.1-7,.21-14.33,3.32-17.43S70.51,56.9,77.55,56.8c6.75-.1,13.72-.21,19.82-2.73,6.35-2.63,11.52-7.59,16.52-12.38S124,32,128,32s9.15,4.92,14.11,9.69,10.17,9.75,16.52,12.38c6.1,2.52,13.07,2.63,19.82,2.73,7,.1,14.33.21,17.43,3.32s3.22,10.39,3.32,17.43c.1,6.75.21,13.72,2.73,19.82,2.63,6.35,7.59,11.52,12.38,16.52S224,124,224,128,219.08,137.15,214.31,142.11ZM173.66,98.34a8,8,0,0,1,0,11.32l-56,56a8,8,0,0,1-11.32,0l-24-24a8,8,0,0,1,11.32-11.32L112,148.69l50.34-50.35A8,8,0,0,1,173.66,98.34Z",
    "list-bullets":
      "M80,64a8,8,0,0,1,8-8H216a8,8,0,0,1,0,16H88A8,8,0,0,1,80,64Zm136,56H88a8,8,0,0,0,0,16H216a8,8,0,0,0,0-16Zm0,64H88a8,8,0,0,0,0,16H216a8,8,0,0,0,0-16ZM44,52A12,12,0,1,0,56,64,12,12,0,0,0,44,52Zm0,64a12,12,0,1,0,12,12A12,12,0,0,0,44,116Zm0,64a12,12,0,1,0,12,12A12,12,0,0,0,44,180Z",
    database:
      "M128,24C74.17,24,32,48.6,32,80v96c0,31.4,42.17,56,96,56s96-24.6,96-56V80C224,48.6,181.83,24,128,24Zm80,104c0,9.62-7.88,19.43-21.61,26.92C170.93,163.35,150.19,168,128,168s-42.93-4.65-58.39-13.08C55.88,147.43,48,137.62,48,128V111.36c17.06,15,46.23,24.64,80,24.64s62.94-9.68,80-24.64ZM69.61,53.08C85.07,44.65,105.81,40,128,40s42.93,4.65,58.39,13.08C200.12,60.57,208,70.38,208,80s-7.88,19.43-21.61,26.92C170.93,115.35,150.19,120,128,120s-42.93-4.65-58.39-13.08C55.88,99.43,48,89.62,48,80S55.88,60.57,69.61,53.08ZM186.39,202.92C170.93,211.35,150.19,216,128,216s-42.93-4.65-58.39-13.08C55.88,195.43,48,185.62,48,176V159.36c17.06,15,46.23,24.64,80,24.64s62.94-9.68,80-24.64V176C208,185.62,200.12,195.43,186.39,202.92Z",
    "chat-dots":
      "M116,128a12,12,0,1,1,12,12A12,12,0,0,1,116,128ZM84,140a12,12,0,1,0-12-12A12,12,0,0,0,84,140Zm88,0a12,12,0,1,0-12-12A12,12,0,0,0,172,140Zm60-76V192a16,16,0,0,1-16,16H83l-32.6,28.16-.09.07A15.89,15.89,0,0,1,40,240a16.13,16.13,0,0,1-6.8-1.52A15.85,15.85,0,0,1,24,224V64A16,16,0,0,1,40,48H216A16,16,0,0,1,232,64ZM40,224h0ZM216,64H40V224l34.77-30A8,8,0,0,1,80,192H216Z",
    link:
      "M165.66,90.34a8,8,0,0,1,0,11.32l-64,64a8,8,0,0,1-11.32-11.32l64-64A8,8,0,0,1,165.66,90.34ZM215.6,40.4a56.08,56.08,0,0,0-79.2,0L112,64.8a8,8,0,0,0,11.31,11.31l24.4-24.4a40,40,0,1,1,56.57,56.57l-24.4,24.4A8,8,0,0,0,191.2,144l24.4-24.4A56.08,56.08,0,0,0,215.6,40.4ZM132.69,179.89l-24.4,24.4a40,40,0,1,1-56.57-56.57l24.4-24.4A8,8,0,0,0,64.8,112l-24.4,24.4a56,56,0,0,0,79.2,79.2L144,191.2a8,8,0,0,0-11.31-11.31Z",
    "puzzle-piece":
      "M220.27,158.54a8,8,0,0,0-7.7-.46,20,20,0,1,1,0-36.16A8,8,0,0,0,224,114.69V72a16,16,0,0,0-16-16H171.78a35.36,35.36,0,0,0,.22-4,36.11,36.11,0,0,0-11.36-26.24,36,36,0,0,0-60.55,23.62,36.56,36.56,0,0,0,.14,6.62H64A16,16,0,0,0,48,72v32.22a35.36,35.36,0,0,0-4-.22,36.12,36.12,0,0,0-26.24,11.36,35.7,35.7,0,0,0-9.69,27,36.08,36.08,0,0,0,33.31,33.6,35.68,35.68,0,0,0,6.62-.14V208a16,16,0,0,0,16,16H208a16,16,0,0,0,16-16V165.31A8,8,0,0,0,220.27,158.54ZM208,208H64V165.31a8,8,0,0,0-11.43-7.23,20,20,0,1,1,0-36.16A8,8,0,0,0,64,114.69V72h46.69a8,8,0,0,0,7.23-11.43,20,20,0,1,1,36.16,0A8,8,0,0,0,161.31,72H208v32.23a35.68,35.68,0,0,0-6.62-.14A36,36,0,0,0,204,176a35.36,35.36,0,0,0,4-.22Z",
    "cooking-pot":
      "M88,48V16a8,8,0,0,1,16,0V48a8,8,0,0,1-16,0Zm40,8a8,8,0,0,0,8-8V16a8,8,0,0,0-16,0V48A8,8,0,0,0,128,56Zm32,0a8,8,0,0,0,8-8V16a8,8,0,0,0-16,0V48A8,8,0,0,0,160,56Zm92.8,46.4L224,124v60a32,32,0,0,1-32,32H64a32,32,0,0,1-32-32V124L3.2,102.4a8,8,0,0,1,9.6-12.8L32,104V80a8,8,0,0,1,8-8H216a8,8,0,0,1,8,8v24l19.2-14.4a8,8,0,0,1,9.6,12.8ZM208,88H48v96a16,16,0,0,0,16,16H192a16,16,0,0,0,16-16Z"
  };
  const recipeIconKeywords = [
    ["bundle", "package"],
    ["reassembl", "puzzle-piece"],
    ["inspect", "magnifying-glass"],
    ["post", "upload"],
    ["upload", "upload"],
    ["chunk", "stack"],
    ["offset", "crosshair"],
    ["resolve", "crosshair"],
    ["verify", "seal-check"],
    ["commitment", "seal-check"],
    ["serialize", "export"],
    ["typed", "book-open"],
    ["key", "key"],
    ["list", "list-bullets"],
    ["message", "chat-dots"],
    ["transaction", "chat-dots"],
    ["raw", "database"],
    ["range", "database"],
    ["read", "book-open"]
  ];
  const guides = [
    ["Introduction", "introduction/index", "Introduction",
      "The conceptual start for this corpus: HyperBEAM, AO-Core, devices, " +
      "and pathing."],
    ["Introduction", "introduction/what-is-hyperbeam", "What is HyperBEAM?",
      "The production-ready AO-Core runtime that powers decentralized " +
      "compute on Erlang/OTP."],
    ["Introduction", "introduction/what-is-ao-core", "What is AO-Core?",
      "The HTTP-native protocol for decentralized computation on the " +
      "Arweave permaweb."],
    ["Introduction", "introduction/ao-devices", "AO Devices",
      "How AO devices modularize compute, codecs, storage, and node " +
      "services."],
    ["Introduction", "introduction/pathing-in-ao-core", "Pathing in AO-Core",
      "How HyperPATH URLs address messages, devices, and computation " +
      "results."],
    ["Processes", "processes/overview", "Process Overview",
      "Create and understand HyperBEAM process@1.0 processes, messages, and " +
      "authorities."],
    ["Processes", "processes/state-and-reads", "State And Reads",
      "Expose process state through patch@1.0 and read it over HTTP."],
    ["Processes", "processes/builder-templates", "Builder Templates",
      "Copy practical Lua process templates for tokens, chats, and public " +
      "state."],
    ["Processes", "processes/ao-connect-mainnet", "AO Connect Mainnet",
      "Spawn, message, and read HyperBEAM processes from JavaScript clients."],
    ["Processes", "processes/aos-lua-reference", "AOS Lua Reference",
      "Keep the AOS Lua commands, globals, handlers, and replies close at " +
      "hand."],
    ["Processes", "processes/migration-to-hyperbeam", "Migration To HyperBEAM",
      "Move legacy AO process patterns to HyperBEAM HTTP reads and patch " +
      "updates."],
    ["Processes", "processes/legacynet-appendix", "Legacynet Appendix",
      "Reference old Legacynet AO patterns only when supporting existing " +
      "processes."],
    ["Device Forge", "forge/index", "Device Forge",
      "The Device Forge turns Erlang dev_* modules into signed, loadable " +
      "HyperBEAM devices."],
    ["Device Forge", "forge/create-a-device", "Create A Device",
      "A device root module is named dev_<name>.erl."],
    ["Device Forge", "forge/install-template", "Install The Forge Template",
      "Install from a HyperBEAM checkout:"],
    ["Device Forge", "forge/operator-configuration", "Operator Configuration",
      "Device loading, cache writes, auth, payments, routes, and remote " +
      "fetches are operator decisions."],
    ["Device Forge", "forge/publish-and-load", "Publish And Load",
      "Publishing signs and uploads the spec and implementation messages:"],
    ["Device Forge", "forge/run-local", "Run Locally",
      "Start a node with your packaged device in a fresh preloaded store:"],
    ["Device Forge", "forge/runbook", "Device Forge Runbook",
      "The Device Forge is how a dev_* Erlang module becomes a loadable " +
      "HyperBEAM device."],
    ["Device Forge", "forge/test-package-verify", "Package, Verify, And Test",
      "From the device project root:"],
    ["Device Forge", "forge/trusted-signers-and-pins",
      "Trusted Signers And Pins",
      "Remote device loading is controlled by operator policy."]
  ].map(([section, path, title, summary]) =>
    ({ section, path, title, summary, href: `/docs/${path}` }));
  let page;
  let shownRoute;
  let communityRecipe = false;

  // Gets a path of the node as JSON, with the messages that it links to when
  // bundle is true, and returns the result of the reply.
  async function getJson(path, bundle) {
    const headers = { accept: jsonType };
    if (bundle) headers["accept-bundle"] = "true";
    const reply = await fetch(path, { headers, credentials: "omit" });
    if (!reply.ok) throw new Error(`${path}: ${reply.status}`);
    const json = await reply.json();
    return json["ao-result"] === "body" ? json.body : json;
  }

  // Gets a path of the node as text, and returns the text and the headers.
  async function getText(path) {
    const reply = await fetch(path, { credentials: "omit" });
    if (!reply.ok) throw new Error(`${path}: ${reply.status}`);
    return { text: await reply.text(), headers: reply.headers };
  }

  // Returns whether a value is an Arweave ID: 43 base64url characters.
  function isId(value) {
    return /^[\w-]{43}$/.test(value);
  }

  // Returns the entries of a map, sorted by their keys.
  function sorted(map) {
    return [...map].sort(([a], [b]) => (a < b ? -1 : a > b ? 1 : 0));
  }

  // Hashes text as `erlang:phash2/1` hashes the binary of its UTF-8 bytes.
  function phash2(text) {
    const k = new TextEncoder().encode(text);
    let [a, b, c] = [0x9e3779b9, 0x9e3779b9, 0x08d12e65];
    const mix = () => {
      a = (a - b - c) ^ (c >>> 13);
      b = (b - c - a) ^ (a << 8);
      c = (c - a - b) ^ (b >>> 13);
      a = (a - b - c) ^ (c >>> 12);
      b = (b - c - a) ^ (a << 16);
      c = (c - a - b) ^ (b >>> 5);
      a = (a - b - c) ^ (c >>> 3);
      b = (b - c - a) ^ (a << 10);
      c = (c - a - b) ^ (b >>> 15);
    };
    if (k.length === 0) return c & 0x7ffffff;
    const word = (at) =>
      k[at] + (k[at + 1] << 8) + (k[at + 2] << 16) + (k[at + 3] << 24);
    let at = 0;
    for (; k.length - at >= 12; at += 12) {
      [a, b, c] = [a + word(at), b + word(at + 4), c + word(at + 8)];
      mix();
    }
    const byte = (idx, shift) =>
      (at + idx < k.length ? k[at + idx] << shift : 0);
    c += k.length + byte(10, 24) + byte(9, 16) + byte(8, 8);
    b += byte(7, 24) + byte(6, 16) + byte(5, 8) + byte(4, 0);
    a += byte(3, 24) + byte(2, 16) + byte(1, 8) + byte(0, 0);
    mix();
    return c & 0x7ffffff;
  }

  // Returns the path of a page of a device's docs: `/~<device>/docs`, then
  // the parts of the page.
  function devicePath(id, ...parts) {
    return [`/~${id}/docs`, ...parts.map(encodeURIComponent)].join("/");
  }

  // Returns the URL of the page of a device, or of the node's home when there
  // is no device. A link to the shown page keeps the URL of the page.
  function pageUrl(device) {
    const here = `${location.pathname}${location.search}`;
    if (device === undefined) return page.home ? here : homeUrl;
    if (page.device && page.device.id === device) return here;
    return `/~message@1.0/info?for=${encodeURIComponent(device)
      .replace("%40", "@")}`;
  }

  // Returns the URL of the page that shows a path of the docs: `/docs` is the
  // node's home and `/~<device>/docs` the page of the device, and the rest of
  // the path goes in the URL's hash. Any other href is returned as is.
  function pageHref(href) {
    const match = /^(?:\/~([^/#]+))?\/docs(\/[^#]*)?(#.*)?$/.exec(href);
    if (match === null) return href;
    const [, device, tail, fragment = ""] = match;
    return `${pageUrl(device)}${tail === undefined ? "" : `#${tail}`}` +
      fragment;
  }

  // Decodes a part of a route, or returns it as is when it is not valid
  // percent-encoding.
  function decodePart(part) {
    try {
      return decodeURIComponent(part);
    } catch (_err) {
      return part;
    }
  }

  // Returns the route in the URL's hash as its parts, and the ID of the
  // element to scroll to:
  // `#/schema/match` -> ["schema", "match"], "";
  // `#/spec#scope` -> ["spec"], "scope";
  // `#devices` -> [], "devices".
  function hashRoute() {
    const hash = location.hash.slice(1);
    if (!hash.startsWith("/")) return { parts: [], anchor: decodePart(hash) };
    const [route, ...anchor] = hash.slice(1).split("#");
    return {
      parts: route.split("/").filter((part) => part !== "").map(decodePart),
      anchor: decodePart(anchor.join("#"))
    };
  }

  // Splits a device into its name and its version.
  function splitDeviceId(id) {
    const at = id.indexOf("@");
    if (at <= 0 || at === id.length - 1) return [id, ""];
    return [id.slice(0, at), id.slice(at + 1)];
  }

  // Gets the devices of the node, in the order of their names.
  async function nodeDevices() {
    const index = await getJson(`${homeUrl}/devices`, true);
    return Object.keys(index)
      .filter((id) => id.includes("@"))
      .sort()
      .map((id) => docsNodeDevice(id, index[id]));
  }

  // Returns a device of the node: its name, its version, its summary and the
  // paths of its pages.
  function docsNodeDevice(id, specId) {
    const [name, version] = splitDeviceId(id);
    return {
      name,
      version,
      href: devicePath(id),
      summary: `On-weave device docs discovered from spec ID ${specId}.`,
      schema: devicePath(id, "schema"),
      spec: devicePath(id, "spec"),
      recipes: devicePath(id, "recipes"),
      implementations: devicePath(id, "implementations")
    };
  }

  // Gets the data of the page's device: its info, from the URL of the page,
  // with its schema and specification, then its implementations and recipes.
  async function deviceData() {
    const info = await getJson(`${location.pathname}${location.search}`, true);
    if (typeof info.for !== "string") {
      throw new Error("not the info of a device");
    }
    const schema = schemaOf(info.schema);
    const spec = specOf(info.specification);
    const implementations = await getJson(
      `${location.pathname}/implementations${location.search}`,
      true
    ).catch(() => ({}));
    return {
      id: info.for,
      schema,
      schemaOrder: [...schema.keys()].sort(),
      spec,
      recipes: await deviceRecipes(spec.txid, spec.signer),
      implementations
    };
  }

  // Returns the schema of a device from the schemas of its functions: each
  // key, with the keys of the request of its first clause as its parameters.
  function schemaOf(functions) {
    const schema = new Map();
    for (const [name, clauses] of Object.entries(functions)) {
      const request = clauses[0].args[1];
      const keys = request && request.kind === "message" ? request.keys : [];
      const params = keys.map((key) => [
        key.name,
        { required: key.presence === "required", type: typeLabel(key.type) }
      ]);
      schema.set(name, { parameters: new Map(params) });
    }
    return schema;
  }

  // Returns the label of a type: `message(<keys>)`, `list<<item>>`,
  // `tuple(<items>)`, `union(<members>)`, `literal:<value>`, or its kind.
  function typeLabel(type) {
    const join = (types) => types.map(typeLabel).join(",");
    switch (type.kind) {
      case "message": {
        const names = type.keys.map((key) => key.name).sort();
        return names.length === 0 ? "message" : `message(${names.join(",")})`;
      }
      case "list":
        return `list<${typeLabel(type.item)}>`;
      case "tuple":
        return `tuple(${join(type.items)})`;
      case "union":
        return `union(${join(type.members)})`;
      case "literal":
        return type["ao-types"]
          ? `literal:${type.value}`
          : `literal:<<"${type.value}">>`;
      default:
        return type.kind;
    }
  }

  // Returns the specification of a device from its signed message: its
  // Markdown, the ID of its signature and its signer, the address at the end
  // of the `committer` of its signature.
  function specOf(message) {
    const [txid, commitment] = Object.entries(message.commitments)
      .find(([, item]) => item.committer);
    return {
      txid,
      markdown: message.body,
      signer: commitment.committer.split(":").pop()
    };
  }

  // Returns the sections of a specification: the ID and the title of each of
  // its level-2 headings.
  function specSections(spec) {
    return HBMarkdown.specSections(spec.markdown);
  }

  // Queries the node's GraphQL for the `Device-Recipe` items of a
  // specification, and returns their nodes.
  async function recipeNodes(specId) {
    const reply = await fetch("/~query@1.0/graphql", {
      method: "POST",
      headers: { accept: jsonType, "content-type": jsonType },
      body: JSON.stringify({
        query: recipeQuery,
        variables: { type: ["Device-Recipe"], specid: [specId] }
      }),
      credentials: "omit"
    });
    if (!reply.ok) throw new Error(`graphql: ${reply.status}`);
    const json = await reply.json();
    return json.data.transactions.edges.map((edge) => edge.node);
  }

  // Gets a recipe: its Markdown, slug, title and summary, and the number of
  // its runnable code blocks. A recipe is official when its owner is the
  // signer of the specification.
  async function recipeDoc(node, signer) {
    const tag = (name, fallback) => {
      const found = node.tags.find((item) => item.name === name);
      return found === undefined ? fallback : found.value;
    };
    const recipe = {
      txid: node.id,
      name: tag("slug", node.id),
      official: signer !== "" && node.owner.address === signer
    };
    try {
      const { text } = await getText(`/${node.id}`);
      return {
        ...recipe,
        markdown: text,
        title: tag("title", HBMarkdown.title(text, recipe.name)),
        summary: HBMarkdown.summary(text),
        runnable: HBMarkdown.codeBlocks(text).filter((block) => block.runnable)
          .length
      };
    } catch (_err) {
      return {
        ...recipe,
        markdown: "",
        title: tag("title", node.id),
        summary: missingRecipeSummary,
        runnable: 0
      };
    }
  }

  // Gets the recipes of a specification in two groups: those of its signer,
  // and those of the community. Each group keeps the first recipe of a slug.
  async function deviceRecipes(specId, signer) {
    const groups = new Map([
      ["recipes", new Map()],
      ["community-recipes", new Map()]
    ]);
    if (!isId(specId)) return groups;
    const nodes = await recipeNodes(specId).catch(() => []);
    const docs =
      await Promise.all(nodes.map((node) => recipeDoc(node, signer)));
    for (const doc of docs) {
      const group = groups.get(doc.official ? "recipes" : "community-recipes");
      if (!group.has(doc.name)) group.set(doc.name, doc);
    }
    return groups;
  }

  // Resolves a relative href of the Markdown at sourceRel to its path in the
  // docs, `docs/forge/runbook.md`. An href that starts with `/` is relative to
  // `docs`, as is any href when there is no sourceRel.
  function resolveDocRelpath(href, sourceRel) {
    const dir = /^docs\/(.+)\/[^/]+$/.exec(sourceRel);
    const base = dir ? `docs/${dir[1]}` : "docs";
    const joined =
      href.startsWith("/") ? `docs/${href.slice(1)}` : `${base}/${href}`;
    const parts = [];
    for (const part of joined.split("/")) {
      if (part === "..") parts.pop();
      else if (part !== "") parts.push(part);
    }
    return parts.join("/");
  }

  // Returns the function that resolves the links of Markdown at sourceRel on
  // the page at activePath. A fragment is a fragment of the page, an external
  // href and a path of the docs are kept, a device or a guide gets its page,
  // and any other link gets its label as code.
  function linkResolver(sourceRel, activePath) {
    return (url, label) => {
      const href = url.trim();
      if (href.startsWith("#")) return `${activePath}${href}`;
      if (/^(https?:\/\/|mailto:|\/~|\/docs)/.test(href)) return href;
      const [relPath, ...fragment] =
        resolveDocRelpath(href, sourceRel).split("#");
      const suffix = fragment.length === 0 ? "" : `#${fragment.join("#")}`;
      const device =
        /^docs\/devices\/(?:.*\/)?(.+?)-at-(.+)\.md$/.exec(relPath);
      if (device) {
        const id = `${device[1]}@${device[2].replaceAll("-", ".")}`;
        return `${devicePath(id)}${suffix}`;
      }
      const guide = guides.find((item) => `docs/${item.path}.md` === relPath);
      if (guide) return `${guide.href}${suffix}`;
      return { code: HBMarkdown.linkCodeLabel(label) };
    };
  }

  // Returns a page: its title, its active path, its sidebar and its content.
  function docsPageHtml(title, activePath, sidebar, content) {
    return { title, activePath, sidebar, content };
  }

  // Renders the node's home: its guides, its devices and the concepts.
  function renderNodeHtml(devices) {
    const content = [
      `<p class="eyebrow">Node</p><h1>HyperBEAM Docs</h1>`,
      "<p>HyperBEAM documentation for the spec-loaded devices this node can ",
      "resolve and run.</p>",
      `<p class="hb-docs-renderer-note">Rendered by <a href="${homeUrl}">`,
      "~docs@1.0</a>.</p>",
      `<h2>Guides</h2>${boilerplateStructuredIndex()}`,
      `<h2 id="devices">Devices</h2><div class="hb-docs-card-grid">`,
      devices.map(deviceRow).join(""),
      `</div><h2>Concepts</h2>${conceptRows()}`
    ];
    return docsPageHtml("HyperBEAM Node Info", "/docs",
      nodeSidebar(devices, "/docs"), content.join(""));
  }

  // Renders the page of a device: its schema, its specification and its
  // recipes.
  function renderDeviceHtml(data) {
    const content = [
      `<p class="eyebrow">Device</p><h1>${esc(deviceMarkedId(data.id))}</h1>`,
      `<h2 id="schema">Schema</h2>`,
      schemaTable(data.id, data.schema, data.schemaOrder),
      `<h2 id="spec">Spec</h2>`,
      renderSpecBody(data.spec, devicePath(data.id)),
      `<h2 id="recipes">Recipes</h2>${recipeGrid(data, "recipes")}`,
      data.recipes.get("community-recipes").size === 0
        ? ""
        : `<h2 id="community-recipes">Community recipes</h2>` +
          recipeGrid(data, "community-recipes")
    ];
    return docsPageHtml("HyperBEAM Device Info", devicePath(data.id),
      deviceSidebar(data, devicePath(data.id)), content.join(""));
  }

  // Renders the schema of a device.
  function renderSchemaIndexHtml(data) {
    const activePath = devicePath(data.id, "schema");
    const content = [
      `<p class="eyebrow">Schema</p><h1>${esc(deviceMarkedId(data.id))} `,
      "schema</h1>",
      schemaTable(data.id, data.schema, data.schemaOrder)
    ];
    return docsPageHtml("HyperBEAM Schema", activePath,
      deviceSidebar(data, activePath), content.join(""));
  }

  // Renders a key of the schema of a device, with its parameters.
  function renderSchemaKeyHtml(data, key) {
    const activePath = devicePath(data.id, "schema", key);
    const content = [
      `<p class="eyebrow">Schema Key</p>`,
      `<h1>${esc(deviceMarkedId(data.id))} / ${esc(key)}</h1><p></p>`,
      "<h2>Parameters</h2>",
      paramsTable(data.id, key, data.schema.get(key).parameters)
    ];
    return docsPageHtml("HyperBEAM Schema Key", activePath,
      deviceSidebar(data, activePath), content.join(""));
  }

  // Renders a parameter of a key of the schema of a device.
  function renderSchemaParameterHtml(data, key, name) {
    const param = data.schema.get(key).parameters.get(name);
    const activePath = devicePath(data.id, "schema", key);
    const content = [
      `<p class="eyebrow">Schema Parameter</p>`,
      `<h1>${esc(deviceMarkedId(data.id))} / ${esc(key)} / ${esc(name)}</h1>`,
      "<table><tbody>",
      `<tr><th>Name</th><td><code>${esc(name)}</code></td></tr>`,
      `<tr><th>Required</th><td>${param.required ? "yes" : "no"}</td></tr>`,
      `<tr><th>Type</th><td>${esc(param.type)}</td></tr>`,
      "<tr><th>Description</th><td></td></tr>",
      "<tr><th>Example</th><td><code></code></td></tr>",
      "</tbody></table>"
    ];
    return docsPageHtml("HyperBEAM Schema Parameter", activePath,
      deviceSidebar(data, activePath), content.join(""));
  }

  // Renders the specification of a device.
  function renderSpecHtml(data) {
    const activePath = devicePath(data.id, "spec");
    const content = [
      `<p class="eyebrow">Specification</p>`,
      `<h1>${esc(deviceMarkedId(data.id))} spec</h1>`,
      renderSpecBody(data.spec, activePath)
    ];
    return docsPageHtml("HyperBEAM Spec", activePath,
      deviceSidebar(data, activePath), content.join(""));
  }

  // Renders a section of the specification of a device.
  function renderSpecSectionPageHtml(data, sectionId, title) {
    const activePath = devicePath(data.id, "spec", sectionId);
    const markdown =
      HBMarkdown.specSectionMarkdown(data.spec.markdown, sectionId);
    const opts = {
      stripNumberedHeadings: true,
      resolveLink: linkResolver("", activePath)
    };
    const content = [
      `<p class="eyebrow">Specification</p>`,
      `<h1>${esc(data.id)} / `,
      `${esc(HBMarkdown.specSectionNavLabel(title))}</h1>`,
      onChainLinkParagraph(data.spec.txid, "View spec transaction"),
      HBMarkdown.renderWithHeadingIds(markdown, opts)
    ];
    return docsPageHtml("HyperBEAM Spec", activePath,
      deviceSidebar(data, activePath), content.join(""));
  }

  // Renders a group of the recipes of a device: `recipes`, by the signer of
  // its specification, or `community-recipes`, by anyone else.
  function renderRecipesHtml(data, group) {
    const activePath = devicePath(data.id, group);
    const [eyebrow, title] = group === "recipes"
      ? ["Recipes", "recipes"]
      : ["Community recipes", "community recipes"];
    const content = [
      `<p class="eyebrow">${eyebrow}</p>`,
      `<h1>${esc(deviceMarkedId(data.id))} ${title}</h1>`,
      recipeGrid(data, group)
    ];
    return docsPageHtml("HyperBEAM Recipes", activePath,
      deviceSidebar(data, activePath), content.join(""));
  }

  // Renders a recipe of a device: the link to its transaction, then its
  // Markdown. A community recipe has a caution, and its raw HTML is shown as
  // text.
  function renderRecipeHtml(data, group, slug) {
    const recipe = data.recipes.get(group).get(slug);
    const activePath = devicePath(data.id, group, slug);
    const opts = {
      resolveLink: linkResolver("", activePath),
      html: recipe.official
    };
    const content = [
      `<p class="eyebrow">Recipe</p>`,
      `<h1>${esc(data.id)} / ${esc(recipe.title)}</h1>`,
      onChainLinkParagraph(recipe.txid, "View recipe transaction"),
      recipe.official ? "" : communityCaution,
      HBMarkdown.render(HBMarkdown.dropFirstH1(recipe.markdown), opts)
    ];
    return {
      ...docsPageHtml("HyperBEAM Recipe", activePath,
        deviceSidebar(data, activePath), content.join("")),
      community: !recipe.official
    };
  }

  // Renders the implementations of a device, with the hashpath of the reply
  // that the node signed.
  function renderImplementationsHtml(data) {
    const reply = data.implementations;
    const activePath = devicePath(data.id, "implementations");
    const rows = (reply.implementations || []).map((id) => [
      `<tr><td>${esc(reply.specification)}</td>`,
      "<td><code>on-weave-beam-archive</code></td>",
      `<td>${arweaveTxLink(id, "View implementation transaction")}</td>`,
      "<td>discovered-by-implements-device</td></tr>"
    ].join(""));
    const content = [
      `<p class="eyebrow">Implementations</p>`,
      `<h1>${esc(deviceMarkedId(data.id))} implementations</h1>`,
      hashpathNote(reply),
      "<table><thead><tr><th>Name</th><th>Module</th><th>Source</th>",
      `<th>Status</th></tr></thead><tbody>${rows.join("")}</tbody></table>`
    ];
    return docsPageHtml("HyperBEAM Implementations", activePath,
      deviceSidebar(data, activePath), content.join(""));
  }

  // Renders the hashpath of a reply, with the address of the committer that
  // signed it.
  function hashpathNote(reply) {
    if (reply.hashpath === undefined) return "";
    const commitment = Object.values(reply.commitments || {}).find((item) =>
      item.committed.includes("hashpath") && item.committer !== undefined);
    const signer = commitment === undefined
      ? ""
      : ` signed by <code>${esc(commitment.committer.split(":").pop())}</code>`;
    return `<p class="hb-docs-renderer-note"><strong>Hashpath${signer}:` +
      `</strong> <code>${esc(reply.hashpath)}</code></p>`;
  }

  // Renders an index of the node: a card for each device, to its page of
  // that index.
  function renderNodeComponentHtml(devices, kind, title) {
    const entries = devices.map((device) =>
      ({ device: deviceCardLabel(device), href: device[kind] }));
    const content = [
      `<p class="eyebrow">Node</p><h1>${esc(title)} index</h1>`,
      `<div class="hb-docs-device-grid">`,
      `${entries.map(deviceRow).join("")}</div>`
    ];
    return docsPageHtml("HyperBEAM Node Index", `/docs/${kind}`,
      nodeSidebarFromComponent(entries, `/docs/${kind}`), content.join(""));
  }

  // Renders the guides: a section of cards for each group of guides.
  function renderNodeBoilerplateHtml() {
    const content = [
      `<p class="eyebrow">Node</p><h1>Guides</h1>`,
      "<p>Conceptual HyperBEAM, AO-Core, process, and Device Forge guides.</p>",
      guideSections.map(boilerplateSectionBlock).join("")
    ];
    return docsPageHtml("HyperBEAM Guides", "/docs/guides",
      nodeSidebar([], "/docs/guides"), content.join(""));
  }

  // Renders a guide: its Markdown without its title, with its images from the
  // package of `docs@1.0`.
  function renderNodeBoilerplatePageHtml(guide, markdown) {
    const sourceRel = `docs/${guide.path}.md`;
    const opts = { resolveLink: linkResolver(sourceRel, guide.href) };
    const body = HBMarkdown.render(HBMarkdown.dropFirstH1(markdown), opts)
      .replaceAll(`src="/docs/assets/images/`,
        `src="${assetUrl}/guides/assets/images/`);
    const content = `<p class="eyebrow">Guide</p><h1>${esc(guide.title)}</h1>` +
      body;
    return docsPageHtml("HyperBEAM Guide", guide.href,
      nodeSidebar([], guide.href), content);
  }

  // Renders the page of a device whose docs the node does not give.
  function renderUnsupportedDeviceHtml(id) {
    const content = [
      `<p class="eyebrow">Device</p>`,
      `<h1>${esc(deviceMarkedId(id))} docs not available</h1>`,
      "<p>No on-weave spec-loaded documentation is available for this ",
      `device.</p><p><a href="/docs">View All Node Info</a></p>`
    ];
    return docsPageHtml("HyperBEAM Device Docs Not Available", devicePath(id),
      nodeSidebar([], devicePath(id)), content.join(""));
  }

  // Renders a specification: the link to its transaction, then its Markdown
  // without its title.
  function renderSpecBody(spec, activePath) {
    const opts = {
      stripNumberedHeadings: true,
      resolveLink: linkResolver("", activePath)
    };
    const link = onChainLinkParagraph(spec.txid, "View spec transaction");
    const markdown = HBMarkdown.dropFirstH1(spec.markdown);
    return `${link}${HBMarkdown.renderWithHeadingIds(markdown, opts)}`;
  }

  // Renders the link to the transaction of an ID, if it is an Arweave ID.
  function onChainLinkParagraph(txid, label) {
    if (!isId(txid)) return "";
    const link = arweaveTxLink(txid, label);
    return `<p class="hb-docs-chain-link-row">${link}</p>`;
  }

  // Renders a link to a transaction on viewblock.io.
  function arweaveTxLink(txid, label) {
    return [
      `<a class="hb-docs-chain-link" `,
      `href="https://viewblock.io/arweave/tx/${esc(txid)}" target="_blank" `,
      `rel="noopener" title="${esc(txid)}">`,
      `<span class="hb-docs-chain-link-icon" aria-hidden="true">`,
      `${phosphorIconPathSvg("link")}</span><span>${esc(label)}</span></a>`
    ].join("");
  }

  // Renders the schema of a device: a row for each key, with its parameters.
  function schemaTable(id, schema, order) {
    const rows = order.map((name) => [
      `<tr><td><a href="${esc(devicePath(id, "schema", name))}">`,
      `${esc(name)}</a></td>`,
      `<td>${paramPills(schema.get(name).parameters)}</td></tr>`
    ].join(""));
    return "<table><thead><tr><th>Key</th><th>Parameters</th></tr></thead>" +
      `<tbody>${rows.join("")}</tbody></table>`;
  }

  // Returns whether a parameter is `required` or `optional`.
  function requiredLabel(param) {
    return param.required ? "required" : "optional";
  }

  // Renders the parameters of a key as pills, or `none` when it has none.
  function paramPills(params) {
    if (params.size === 0) return `<span class="param-pills-none">none</span>`;
    return sorted(params).map(([name, param]) => {
      const label = requiredLabel(param);
      return `<span class="pill pill-${label}">${esc(name)} ${label}</span>`;
    }).join("");
  }

  // Renders the parameters of a key as a table: the name, whether it is
  // required, and the type of each.
  function paramsTable(id, key, params) {
    if (params.size === 0) return "<p>No parameters.</p>";
    const rows = sorted(params).map(([name, param]) => [
      `<tr><td><a href="${esc(devicePath(id, "schema", key, name))}">`,
      `<code>${esc(name)}</code></a></td>`,
      `<td><span class="pill pill-${requiredLabel(param)}">`,
      `${requiredLabel(param)}</span></td>`,
      `<td>${esc(param.type)}</td><td></td><td><code></code></td></tr>`
    ].join(""));
    return [
      "<table><thead><tr><th>Name</th><th>Required</th><th>Type</th>",
      "<th>Description</th><th>Example</th></tr></thead>",
      `<tbody>${rows.join("")}</tbody></table>`
    ].join("");
  }

  // Renders the cards of a group of the recipes of a device, or `No recipes.`
  // when the group has none.
  function recipeGrid(data, group) {
    const recipes = sorted(data.recipes.get(group));
    if (recipes.length === 0) return "<p>No recipes.</p>";
    const cards = recipes.map(([slug, recipe]) =>
      recipeNav(data.id, group, slug, recipe));
    return `<div class="hb-docs-card-grid">${cards.join("")}</div>`;
  }

  // Renders the card of a recipe: its icon, its title, its summary and the
  // number of its runnable code blocks. A community recipe has a pill.
  function recipeNav(id, group, slug, recipe) {
    const meta = recipe.runnable === 0
      ? ""
      : `<small class="hb-docs-recipe-card-meta">${recipe.runnable} ` +
        "runnable</small>";
    const desc = (recipe.official ? "" : communityPill) +
      esc(HBMarkdown.recipeCardSummary(recipe));
    return recipeCard(devicePath(id, group, slug), slug, recipe.title, desc,
      meta);
  }

  // Renders a card with an icon and a title, a description, and `Open` with
  // its meta.
  function recipeCard(href, slug, title, desc, meta) {
    return [
      `<a class="hb-docs-card hb-docs-recipe-card" href="${esc(href)}">`,
      `<div class="hb-docs-recipe-card-header">`,
      `<div class="hb-docs-recipe-card-icon" aria-hidden="true">`,
      `${phosphorIconPathSvg(recipeIconName(slug, title))}</div>`,
      `<strong class="hb-docs-recipe-card-title">${esc(title)}</strong></div>`,
      `<div class="hb-docs-recipe-card-body">`,
      `<span class="hb-docs-recipe-card-desc">${desc}</span>`,
      `<div class="hb-docs-recipe-card-footer">`,
      `<span class="hb-docs-recipe-card-cta">Open &rarr;</span>`,
      `${meta}</div></div></a>`
    ].join("");
  }

  // Returns the icon of a recipe: the icon of the first keyword in its slug
  // or its title, or a cooking pot.
  function recipeIconName(slug, title) {
    const text = `${slug.toLowerCase()} ${title.toLowerCase()}`;
    const match =
      recipeIconKeywords.find(([keyword]) => text.includes(keyword));
    return match === undefined ? "cooking-pot" : match[1];
  }

  // Renders an icon of the Phosphor set.
  function phosphorIconPathSvg(icon) {
    return `<svg viewBox="0 0 256 256" fill="currentColor" ` +
      `focusable="false"><path d="${iconPaths[icon]}"></path></svg>`;
  }

  // Returns the label of the card of a device: `<name>@<version>`.
  function deviceCardLabel(device) {
    return device.device || `${device.name}@${device.version}`;
  }

  // Returns a label of a device with a leading `~`.
  function deviceMarkedId(label) {
    return label.startsWith("~") ? label : `~${label}`;
  }

  // Returns the title of a device: its name, with a capital first letter.
  function deviceDisplayTitle(id) {
    const [name] = splitDeviceId(id);
    return name.charAt(0).toUpperCase() + name.slice(1);
  }

  // Renders the card of a device: its title and its label, in the tint of its
  // label, and its summary, or else the URL of the page that it opens.
  function deviceRow(device) {
    const label = deviceCardLabel(device);
    const summary =
      device.summary === undefined ? pageHref(device.href) : device.summary;
    return [
      `<a class="hb-docs-device-card" href="${esc(device.href)}">`,
      `<div class="hb-docs-device-card-body">`,
      `<div class="hb-docs-device-card-title-row hb-docs-device-tint-`,
      `${phash2(label) % 8}">`,
      `<strong class="hb-docs-device-card-title">`,
      `${esc(deviceDisplayTitle(label))}</strong>`,
      `<span class="hb-docs-device-card-id">`,
      `${esc(deviceMarkedId(label))}</span></div>`,
      `<span class="hb-docs-device-card-desc">`,
      `${esc(HBMarkdown.cardSummary(summary))}</span></div></a>`
    ].join("");
  }

  // Renders the concepts, each with its description.
  function conceptRows() {
    return concepts.map(([key, value]) =>
      `<p><strong>${esc(key)}</strong><br>${esc(value)}</p>`).join("");
  }

  // Renders the guides of the node's home: a list of links for each group of
  // guides.
  function boilerplateStructuredIndex() {
    const groups = guideSections.map((section) => [
      `<section class="hb-docs-guide-group"><h3>${esc(section)}</h3><ul>`,
      guides.filter((guide) => guide.section === section)
        .map(boilerplateGuideLink).join(""),
      "</ul></section>"
    ].join(""));
    return `<nav class="hb-docs-guide-index" aria-label="Guides">` +
      `${groups.join("")}</nav>`;
  }

  // Renders the link to a guide.
  function boilerplateGuideLink(guide) {
    return `<li><a href="${esc(guide.href)}">${esc(guide.title)}</a></li>`;
  }

  // Renders a group of guides as cards: plain cards for the introduction, and
  // recipe cards for processes and the Device Forge.
  function boilerplateSectionBlock(section) {
    const cards = guides.filter((guide) => guide.section === section)
      .map((guide) => {
        const summary = esc(HBMarkdown.cardSummary(guide.summary));
        if (section !== "Introduction") {
          const slug = guide.href.split("/").pop();
          return recipeCard(guide.href, slug, guide.title, summary, "");
        }
        return `<a class="hb-docs-card" href="${esc(guide.href)}">` +
          `<strong>${esc(guide.title)}</strong><span>${summary}</span></a>`;
      });
    return `<section class="hb-docs-guide-section"><h2>${esc(section)}</h2>` +
      `<div class="hb-docs-card-grid">${cards.join("")}</div></section>`;
  }

  // Renders a link of the sidebar, active when its href is the active path.
  function sidebarLi(activePath, href, content) {
    const active = activePath === href ? ` class="active"` : "";
    return `<li${active}><a href="${esc(href)}">${content}</a></li>`;
  }

  // Renders a section of the sidebar: its label, the link to all of it, then
  // its items.
  function sidebarNavSection(activePath, label, allLabel, allHref, items) {
    const all = sidebarLi(activePath, allHref, allLabel);
    return `<li><p>${label}</p><ul>${all}${items.join("")}</ul></li>`;
  }

  // Returns the class of a link of the sidebar's context, with `is-active`
  // when its href is the active path.
  function contextLinkClass(activePath, href, base) {
    return activePath === href ? `${base} is-active` : base;
  }

  // Renders a link back from the sidebar's context, with its icon.
  function sidebarViewingBackLink(activePath, href, label, icon) {
    const className =
      contextLinkClass(activePath, href, "sidebar-viewing-back");
    return `<a class="${className}" href="${esc(href)}">` +
      `<span class="sidebar-viewing-back-icon" aria-hidden="true">` +
      `${phosphorIconPathSvg(icon)}</span>${esc(label)}</a>`;
  }

  // Renders the sidebar's context on the pages of the node.
  function sidebarNodeContext(activePath) {
    const className =
      contextLinkClass(activePath, "/docs", "sidebar-viewing-device");
    return `<li class="sidebar-viewing-context">` +
      `<p class="eyebrow">You are viewing</p>` +
      `<a class="${className}" href="/docs">Node</a></li>`;
  }

  // Renders the sidebar's context on the pages of a device: the device, and
  // the links back to the devices and to the node.
  function sidebarDeviceContext(activePath, id) {
    const href = devicePath(id);
    const className =
      contextLinkClass(activePath, href, "sidebar-viewing-device");
    return [
      `<li class="sidebar-viewing-context">`,
      `<p class="eyebrow">You are viewing</p>`,
      `<a class="${className}" href="${esc(href)}">`,
      `${esc(deviceMarkedId(id))}</a>`,
      sidebarViewingBackLink(activePath, "/docs#devices", "View All Devices",
        "stack"),
      sidebarViewingBackLink(activePath, "/docs", "View All Node Info",
        "database"),
      "</li>"
    ].join("");
  }

  // Renders the sidebar of the pages of the node: the context, the devices,
  // the indexes, then the guides of each group.
  function nodeSidebar(devices, activePath) {
    const deviceLis = devices.map((device) => sidebarLi(activePath,
      device.href, esc(deviceMarkedId(deviceCardLabel(device)))));
    const indexLis = nodeIndexItems.map(([kind, label]) =>
      sidebarLi(activePath, `/docs/${kind}`, label));
    const guideLis = guideSections.map((section) => [
      `<li><p>${esc(section)}</p><ul>`,
      guides.filter((guide) => guide.section === section)
        .map(boilerplateGuideLink).join(""),
      "</ul></li>"
    ].join(""));
    return [
      sidebarNodeContext(activePath),
      `<li class="sidebar-flat-links"><p>Devices</p>`,
      `<ul>${deviceLis.join("")}</ul></li>`,
      `<li class="sidebar-flat-links"><p>Indexes</p>`,
      `<ul>${indexLis.join("")}</ul></li>`,
      `<li><p>Guides</p><ul><li><a href="/docs/guides">All guides</a></li>`,
      `${guideLis.join("")}</ul></li>`
    ].join("");
  }

  // Renders the sidebar of an index of the node: the context, then the page
  // of that index of each device.
  function nodeSidebarFromComponent(entries, activePath) {
    const isIndex =
      nodeIndexItems.some(([kind]) => `/docs/${kind}` === activePath);
    const lis = entries.map((entry) =>
      sidebarLi(activePath, entry.href, esc(deviceMarkedId(entry.device))));
    return sidebarNodeContext(activePath) +
      `<li class="sidebar-flat-links${isIndex ? " active" : ""}">` +
      `<p>Index</p><ul>${lis.join("")}</ul></li>`;
  }

  // Renders the sidebar of the pages of a device: the context, then its
  // schema keys, the sections of its specification, and its recipes.
  function deviceSidebar(data, activePath) {
    const id = data.id;
    const keyLis = data.schemaOrder.map((key) =>
      sidebarLi(activePath, devicePath(id, "schema", key), esc(key)));
    const specLis = specSections(data.spec).map(([sectionId, title]) =>
      sidebarLi(activePath, devicePath(id, "spec", sectionId),
        esc(HBMarkdown.specSectionNavLabel(title))));
    const recipeLis = (group) => sorted(data.recipes.get(group))
      .map(([slug, recipe]) =>
        sidebarLi(activePath, devicePath(id, group, slug), esc(recipe.title)));
    return [
      sidebarDeviceContext(activePath, id),
      sidebarNavSection(activePath, "Schema", "All keys",
        devicePath(id, "schema"), keyLis),
      sidebarNavSection(activePath, "Spec", "All spec",
        devicePath(id, "spec"), specLis),
      sidebarNavSection(activePath, "Recipes", "All recipes",
        devicePath(id, "recipes"), recipeLis("recipes")),
      data.recipes.get("community-recipes").size === 0
        ? ""
        : sidebarNavSection(activePath, "Community recipes",
          "All community recipes", devicePath(id, "community-recipes"),
          recipeLis("community-recipes"))
    ].join("");
  }

  // Renders the page of a route of a device's docs, or else the page of the
  // device.
  function deviceRoute(data, [section, name, param]) {
    const keySchema = data.schema.get(name);
    const specSection =
      specSections(data.spec).find(([sectionId]) => sectionId === name);
    const group = data.recipes.get(section);
    if (section === "schema" && name === undefined) {
      return renderSchemaIndexHtml(data);
    }
    if (section === "schema" && keySchema && param === undefined) {
      return renderSchemaKeyHtml(data, name);
    }
    if (section === "schema" && keySchema && keySchema.parameters.has(param)) {
      return renderSchemaParameterHtml(data, name, param);
    }
    if (section === "spec" && name === undefined) return renderSpecHtml(data);
    if (section === "spec" && specSection) {
      return renderSpecSectionPageHtml(data, ...specSection);
    }
    if (group && name === undefined) return renderRecipesHtml(data, section);
    if (group && group.has(name)) return renderRecipeHtml(data, section, name);
    if (section === "implementations") return renderImplementationsHtml(data);
    return renderDeviceHtml(data);
  }

  // Renders the page of a route of the node's docs, or else the node's home.
  // A guide is rendered when its Markdown arrives.
  async function nodeRoute(devices, parts) {
    const path = ["/docs", ...parts].join("/");
    const item = nodeIndexItems.find(([kind]) => `/docs/${kind}` === path);
    if (item) return renderNodeComponentHtml(devices, ...item);
    if (path === "/docs/guides") return renderNodeBoilerplateHtml();
    const guide = guides.find((entry) =>
      [path, `${path}/index`, `${path}/overview`].includes(entry.href));
    if (guide === undefined) return renderNodeHtml(devices);
    const markdown = await getText(`${assetUrl}/guides/${guide.path}.md`)
      .then((reply) => reply.text, () => "");
    return renderNodeBoilerplatePageHtml(guide, markdown);
  }

  // Runs the scripts of the site on the shown page: code highlighting and
  // chrome, the example runner, the mobile navigation, the page's table of
  // contents and its footer navigation.
  function runPageScripts() {
    window.Prism.highlightAll();
    window.HBDocsCodeChrome.refresh();
    window.HBExampleRunner.refresh();
    // `HBDocsMobileNav.init` binds the menu button each time it runs, so it
    // gets a new button without the bindings of the page before.
    const toggle = document.getElementById("mobile-menu-toggle");
    toggle.replaceWith(toggle.cloneNode(true));
    window.HBDocsMobileNav.init();
    window.HBDocsPageToc.init();
    window.HBDocsFooterNav.init();
  }

  // Shows a page: its title, its active path, its sidebar and its content.
  // The scripts of the site work with the paths of the docs, so every link,
  // the links of the header and the menu to the node's home included, has its
  // path while the scripts run, then the URL of the page that shows it. A
  // fragment link of a page with a route keeps the route.
  function showPage(shown, route) {
    document.title = shown.title;
    document.body.setAttribute("data-active-path", shown.activePath);
    sidebarList.innerHTML = shown.sidebar;
    article.innerHTML = shown.content;
    for (const home of document.querySelectorAll(homeLinks)) {
      home.setAttribute("href", "/docs");
    }
    communityRecipe = shown.community === true;
    runPageScripts();
    for (const link of document.querySelectorAll("a[href]")) {
      const href = link.getAttribute("href");
      if (/^#(?!\/)/.test(href) && route !== "") {
        link.setAttribute("href", `#/${route}${href}`);
      } else {
        link.setAttribute("href", pageHref(href));
      }
    }
  }

  // Holds the first click on the run button of a code block of a community
  // recipe, and shows a caution above the code block. The next click runs it.
  function holdCommunityRun(event) {
    const button = event.target.closest(".hb-code-actions button");
    if (!communityRecipe || button === null) return;
    if (button.dataset.cautioned === "true") return;
    event.preventDefault();
    event.stopPropagation();
    button.dataset.cautioned = "true";
    button.closest("pre").insertAdjacentHTML("beforebegin", runCaution);
  }

  // Shows the route of the URL's hash when it changes, then scrolls to the
  // element of its fragment.
  async function show() {
    const { parts, anchor } = hashRoute();
    const route = parts.join("/");
    if (route !== shownRoute) {
      shownRoute = route;
      let shown;
      if (page.device) shown = deviceRoute(page.device, parts);
      else if (page.home) shown = await nodeRoute(page.devices, parts);
      else shown = renderUnsupportedDeviceHtml(page.missing);
      if (route !== shownRoute) return;
      showPage(shown, route);
      window.scrollTo(0, 0);
    }
    if (anchor !== "") document.getElementById(anchor)?.scrollIntoView();
  }

  // Gets the data of the page: the node's home, or the page of a device. Then
  // shows the route of the URL's hash, and again each time the hash changes.
  async function start() {
    article.addEventListener("click", holdCommunityRun, true);
    if ([homeUrl, `${homeUrl}/index`].includes(location.pathname)) {
      page = { home: true, devices: await nodeDevices().catch(() => []) };
    } else {
      page = await deviceData().then(
        (device) => ({ device }),
        () => ({
          missing: new URLSearchParams(location.search).get("for") ||
            (/^\/~([^/]+)\//.exec(location.pathname) || [])[1] || ""
        })
      );
    }
    window.addEventListener("hashchange", show);
    show();
  }

  start();
})();
