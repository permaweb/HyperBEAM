import { get, formatDisplayAmount, copyToClipboard } from '/~hyperbuddy@1.0/utils.js';

// Set up global message handler for console communications
window.addEventListener('message', (event) => {
  if (event.data && event.data.action) {
    if (event.data.action === 'expandConsole') {
      expandConsole();
    } else if (event.data.action === 'exitExpandedConsole') {
      exitExpandedConsole();
    }
  }
});

/**
 * Parse info data from the server response
 * @param {string} text - The raw info text
 * @returns {Array} - The parsed info objects
 */
function parseInfo(text) {
  const lines = text
    .split("\n")
    .map((line) => line.trim())
    .filter(Boolean);

  let boundary = "";
  for (const line of lines) {
    if (line.startsWith("--")) {
      boundary = line;
      break;
    }
  }
  if (!boundary) return [];

  const parts = text.split(boundary);
  const results = [];

  parts.forEach((part) => {
    part = part.trim();
    if (!part || part === "--") return;
    const sections = part.split("\r\n\r\n");
    if (sections.length < 2) return;
    const header = sections[0].trim();
    const content = sections
      .slice(1)
      .join("\r\n\r\n")
      .trim()
      .replaceAll(`"`, "");
    if (
      content.toLowerCase().startsWith("atom") ||
      !content.toLowerCase().startsWith("dev")
    )
      return;
    const nameMatch = header.match(/name="([^"]+)"/);
    if (!nameMatch) return;
    const module = nameMatch[1];
    results.push({
      name: content.replaceAll(`"`, `''`),
      content: module,
    });
  });

  return results;
}

/**
 * Render the info groups in the UI
 * @param {Array} groups - The info groups
 * @param {string} devicesStr - The devices string in JSON format
 */
function renderInfoGroups(groups, devicesStr) {
  const devices = JSON.parse(devicesStr);
  const container = document.getElementById("info-section-lines");
  
  // Clear previous content
  container.innerHTML = "";
  
  // Make sure it has the right class
  container.className = "device-cards-container";
  
  for (const [key, device] of Object.entries(devices)) {
    if(key == "device") continue;
    const [name, variant] = device.name.split("@");
    
    // Create a card for each device
    const card = document.createElement("div");
    card.classList.add("device-card");
    
    // Create device name element
    const deviceName = document.createElement("div");
    deviceName.classList.add("device-name");
    deviceName.textContent = name;
    
    // Create variant element with color based on version number
    const deviceVariant = document.createElement("div");
    deviceVariant.classList.add("device-variant");
    
    // Add color class based on version number
    const versionNum = parseFloat(variant);
    if (versionNum >= 1.0) {
      deviceVariant.classList.add("device-variant-high");
    } else if (versionNum >= 0.5) {
      deviceVariant.classList.add("device-variant-medium");
    } else {
      deviceVariant.classList.add("device-variant-low");
    }
    
    deviceVariant.textContent = variant;
    
    // Add elements to card
    card.appendChild(deviceName);
    card.appendChild(deviceVariant);
    
    // Add card to container
    container.appendChild(card);
  }
}

/**
 * Fetch info data from the server
 */
async function fetchInfo() {
  try {
    const operatorAddress = await get("/~meta@1.0/info/address");
    const operatorAction = document.getElementById("operator-action");
    operatorAction.innerHTML = formatAddress(operatorAddress);
    operatorAction.value = operatorAddress;
    operatorAction.disabled = false;
    
    // Add click handler for the operator button
    operatorAction.addEventListener("click", function() {
      copyToClipboard(this);
    });

    const info = await get("/~meta@1.0/info");
    const deviceInfo =
      await get("/~meta@1.0/info/preloaded_devices/serialize~json@1.0");
    const infoGroups = parseInfo(info);
    renderInfoGroups(infoGroups, deviceInfo);
  } catch (error) {
    console.error("Error fetching or parsing info:", error);
  }
}

/**
 * Format an address for display
 * @param {string} address - The address to format
 * @returns {string} - The formatted address
 */
function formatAddress(address) {
  if (!address) return "";
  return address.substring(0, 6) + "..." + address.substring(36, address.length);
}

/**
 * Add the console iframe to the UI
 */
function addConsole() {
  const container = document.getElementById("console-section");
  const consoleContainer = document.createElement("iframe");
  consoleContainer.style.width = "100%";
  consoleContainer.style.minHeight = "500px";
  consoleContainer.style.border = "none";
  consoleContainer.src = "/~hyperbuddy@1.0/console";
  consoleContainer.id = "console-iframe";
  container.appendChild(consoleContainer);
}

/**
 * Expand the console to take over the entire screen
 */
function expandConsole() {
  const consoleIframe = document.getElementById("console-iframe");
  if (!consoleIframe) {
    return;
  }
  
  // Save original position and size
  if (!consoleIframe.dataset.originalStyles) {
    consoleIframe.dataset.originalStyles = JSON.stringify({
      width: consoleIframe.style.width,
      height: consoleIframe.style.height,
      minHeight: consoleIframe.style.minHeight,
      position: consoleIframe.style.position,
      top: consoleIframe.style.top,
      left: consoleIframe.style.left,
      zIndex: consoleIframe.style.zIndex
    });
  }
  
  // Apply fullscreen styles directly to the iframe
  consoleIframe.style.position = "fixed";
  consoleIframe.style.top = "0";
  consoleIframe.style.left = "0";
  consoleIframe.style.width = "100%";
  consoleIframe.style.height = "100%";
  consoleIframe.style.minHeight = "100%";
  consoleIframe.style.zIndex = "9999";
  
  // Notify the iframe that it's now expanded
  setTimeout(() => {
    if (consoleIframe.contentWindow) {
      consoleIframe.contentWindow.postMessage({
        consoleState: 'expanded'
      }, '*');
    }
  }, 100); // Short delay to ensure iframe is ready
}

/**
 * Exit the expanded console view and return to normal layout
 */
function exitExpandedConsole() {
  const consoleIframe = document.getElementById("console-iframe");
  
  if (!consoleIframe) {
    return;
  }
  
  // Restore original position and size
  if (consoleIframe.dataset.originalStyles) {
    const originalStyles = JSON.parse(consoleIframe.dataset.originalStyles);
    consoleIframe.style.position = originalStyles.position;
    consoleIframe.style.top = originalStyles.top;
    consoleIframe.style.left = originalStyles.left;
    consoleIframe.style.width = originalStyles.width;
    consoleIframe.style.height = originalStyles.height;
    consoleIframe.style.minHeight = originalStyles.minHeight || "500px";
    consoleIframe.style.zIndex = originalStyles.zIndex;
  } else {
    // Fallback if original styles weren't saved
    consoleIframe.style.position = "";
    consoleIframe.style.top = "";
    consoleIframe.style.left = "";
    consoleIframe.style.width = "100%";
    consoleIframe.style.height = "";
    consoleIframe.style.minHeight = "500px";
    consoleIframe.style.zIndex = "";
  }
  
  // Notify the iframe that it's now in normal mode
  setTimeout(() => {
    if (consoleIframe.contentWindow) {
      consoleIframe.contentWindow.postMessage({
        consoleState: 'normal'
      }, '*');
    }
  }, 100); // Short delay to ensure iframe is ready
}

// Export functions for use in other modules
export {
  parseInfo,
  renderInfoGroups,
  fetchInfo,
  addConsole,
  expandConsole,
  exitExpandedConsole
}; 