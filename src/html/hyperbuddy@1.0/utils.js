/**
 * Base route for all API calls
 */
const BASE_ROUTE = window.location.origin;

/**
 * Format an address for display, shortening it if needed
 * @param {string} address - The full address to format
 * @param {boolean} wrap - Whether to wrap the address in parentheses
 * @returns {string} - The formatted address
 */
function formatAddress(address, wrap) {
  if (!address) return "";
  const formattedAddress =
    address.substring(0, 6) + "..." + address.substring(36, address.length);
  return wrap ? `(${formattedAddress})` : formattedAddress;
}

/**
 * Format a number for display, adding commas and handling decimals
 * @param {number|string} amount - The amount to format
 * @returns {string} - The formatted amount
 */
function formatDisplayAmount(amount) {
  if (amount === null) return "-";
  if (amount.toString().includes(".")) {
    let parts = amount.toString().split(".");
    parts[0] = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, ",");

    let firstTwoDecimals = parts[1].substring(0, 2);

    if (firstTwoDecimals === "00") {
      parts[1] = parts[1].substring(0, 6);
    } else {
      parts[1] = parts[1].substring(0, 4);
    }

    return parts.join(".");
  } else {
    return amount.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
  }
}

/**
 * Make a GET request to the server
 * @param {string} endpoint - The endpoint to request
 * @returns {Promise<string>} - The response text
 */
async function get(endpoint) {
  return await (await fetch(`${BASE_ROUTE}${endpoint}`)).text();
}

/**
 * Copy text to clipboard and update button text
 * @param {HTMLButtonElement} button - The button that was clicked
 */
function copyToClipboard(button) {
  const text = button.innerHTML;

  navigator.clipboard
    .writeText(button.value)
    .then(() => {
      button.textContent = "Copied!";
      button.disabled = true;

      setTimeout(() => {
        button.textContent = text;
        button.disabled = false;
      }, 2000);
    })
    .catch((err) => console.error("Error copying text: ", err));
}

function enableTooltipAndCopy(selector) {
  const el = document.querySelector(selector);
  if (!el) return;

  // Create tooltip
  const tooltip = document.createElement("div");
  tooltip.className = "custom-tooltip";
  tooltip.style.position = "absolute";
  tooltip.style.backgroundColor = "#333";
  tooltip.style.color = "#fff";
  tooltip.style.padding = "5px 10px";
  tooltip.style.borderRadius = "4px";
  tooltip.style.fontSize = "12px";
  tooltip.style.whiteSpace = "nowrap";
  tooltip.style.display = "none";
  tooltip.style.zIndex = "9999";
  tooltip.style.pointerEvents = "none";
  document.body.appendChild(tooltip);

  el.style.cursor = "pointer";

  el.addEventListener("mouseenter", (e) => {
    tooltip.textContent = "Click to copy";
    tooltip.style.display = "block";
  });

  el.addEventListener("mousemove", (e) => {
    const tooltipRect = tooltip.getBoundingClientRect();
    tooltip.style.left = `${e.clientX - tooltipRect.width / 2}px`;
    tooltip.style.top = `${e.clientY - tooltipRect.height - 10}px`;
  });

  el.addEventListener("mouseleave", () => {
    tooltip.style.display = "none";
  });

  el.addEventListener("click", async () => {
    try {
      await navigator.clipboard.writeText(el.innerText);
      tooltip.textContent = "Copied!";
      setTimeout(() => {
        tooltip.style.display = "none";
      }, 1000);
    } catch (err) {
      console.error("Copy failed", err);
    }
  });
}

/**
 * Show a specific tab content and update tab button styles
 * @param {string} tabId - The ID of the tab to show
 */
function showTab(tabId) {
  // Hide all tab content
  document.querySelectorAll(".tab-content").forEach((content) => {
    content.classList.remove("active");
  });

  // Deactivate all tab buttons
  document.querySelectorAll(".tab-button").forEach((button) => {
    button.classList.remove("active");
  });

  // Show the selected tab content
  document.getElementById(tabId).classList.add("active");

  // Activate the matching tab button
  document
    .querySelector(`.tab-button[data-tab="${tabId}"]`)
    .classList.add("active");
}

/**
 * Initialize tab event listeners
 */
function initTabListeners() {
  document.querySelectorAll(".tab-button").forEach((button) => {
    button.addEventListener("click", () => {
      const tabId = button.getAttribute("data-tab");
      showTab(tabId);
    });
  });
}

/**
 * SimpleJsonViewer - A lightweight JSON visualization library
 */
class SimpleJsonViewer {
  /**
   * Create a new JSON viewer
   * @param {Object} options - Configuration options
   * @param {HTMLElement} options.container - DOM element to render in
   * @param {string|Object} options.data - JSON string or object to display
   * @param {string} [options.theme="light"] - Display theme (light/dark)
   * @param {boolean} [options.expand=false] - Expand all nodes by default
   */
  constructor(options) {
    this.options = Object.assign(
      {
        theme: "light",
        container: null,
        data: "{}",
        expand: false,
      },
      options
    );

    if (!this.options.container) {
      throw new Error("Container: DOM element is required");
    }

    this.render();
  }

  /**
   * Render the JSON viewer
   */
  render() {
    const container = this.options.container;
    container.innerHTML = "";
    container.className = `json-viewer ${this.options.theme}`;

    let data;
    try {
      if (typeof this.options.data === "string") {
        data = JSON.parse(this.options.data);
      } else {
        data = this.options.data;
      }
    } catch (e) {
      container.innerHTML = `<div class="json-error">Invalid JSON: ${e.message}</div>`;
      return;
    }

    const rootElement = this.createNode(data, null, 0);
    container.appendChild(rootElement);
  }

  /**
   * Create a DOM node for a JSON value
   * @param {*} value - The value to display
   * @param {string|null} key - The property key (or null for root/array elements)
   * @param {number} level - Nesting level
   * @returns {HTMLElement} The created DOM element
   */
  createNode(value, key, level) {
    const item = document.createElement("div");
    item.className = "json-item";
    item.style.marginLeft = level === 0 ? "0" : "20px";

    const valueType = this.getType(value);

    // For objects and arrays, create expandable sections
    if (valueType === "object" || valueType === "array") {
      // Create the item label part
      const itemHead = document.createElement("div");
      const toggle = document.createElement("span");
      toggle.className = `json-toggle ${this.options.expand ? "expanded" : ""}`;
      itemHead.appendChild(toggle);

      // Add the key part if this isn't the root
      if (key !== null) {
        const keyEl = document.createElement("span");
        keyEl.className = "json-key";
        keyEl.textContent = key;
        itemHead.appendChild(keyEl);

        const separator = document.createElement("span");
        separator.className = "json-separator";
        separator.textContent = " : ";
        itemHead.appendChild(separator);
      }

      // Add type label (object/array) with count
      const typeLabel = document.createElement("span");
      typeLabel.className = "json-type-label";
      const count =
        valueType === "object" ? Object.keys(value).length : value.length;
      typeLabel.textContent = `${valueType} {${count}}`;
      itemHead.appendChild(typeLabel);

      // Add the full item to the parent
      item.appendChild(itemHead);

      // Create the container for child items
      const container = document.createElement("div");
      container.className = `json-container ${
        this.options.expand ? "" : "collapsed"
      }`;

      // For objects
      if (valueType === "object") {
        const keys = Object.keys(value);
        keys.forEach((objKey) => {
          const childNode = this.createNode(value[objKey], objKey, level + 1);
          container.appendChild(childNode);
        });
      }
      // For arrays
      else if (valueType === "array") {
        value.forEach((arrItem, index) => {
          const childNode = this.createNode(
            arrItem,
            index.toString(),
            level + 1
          );
          container.appendChild(childNode);
        });
      }

      // Add the container (no closing bracket/brace element)
      item.appendChild(container);

      // Add click handler
      toggle.addEventListener("click", () => {
        toggle.classList.toggle("expanded");
        container.classList.toggle("collapsed");
      });
    }
    // For primitive values, just show the key-value pair
    else {
      // Create the row with key and value
      if (key !== null) {
        const keyEl = document.createElement("span");
        keyEl.className = "json-key";
        keyEl.textContent = key;
        item.appendChild(keyEl);

        const separator = document.createElement("span");
        separator.className = "json-separator";
        separator.textContent = " : ";
        item.appendChild(separator);
      }

      // Add the value with appropriate styling
      const valueEl = document.createElement("span");
      valueEl.className = `json-${valueType}`;

      if (valueType === "string") {
        valueEl.textContent = `"${this.escapeString(value)}"`;
      } else {
        valueEl.textContent = String(value);
      }

      item.appendChild(valueEl);
    }

    return item;
  }

  /**
   * Get the type of a value
   * @param {*} value - The value to check
   * @returns {string} The type name
   */
  getType(value) {
    if (value === null) return "null";
    if (Array.isArray(value)) return "array";
    if (typeof value === "object") return "object";
    if (typeof value === "string") return "string";
    if (typeof value === "number") return "number";
    if (typeof value === "boolean") return "boolean";
    return "unknown";
  }

  /**
   * Escape a string for display
   * @param {string} str - The string to escape
   * @returns {string} The escaped string
   */
  escapeString(str) {
    return str
      .replace(/\\/g, "\\\\")
      .replace(/"/g, '\\"')
      .replace(/\n/g, "\\n")
      .replace(/\r/g, "\\r")
      .replace(/\t/g, "\\t");
  }

  /**
   * Update the theme
   * @param {string} theme - The theme to use ('light' or 'dark')
   */
  setTheme(theme) {
    this.options.container.className = `json-viewer ${theme}`;
    this.options.theme = theme;
  }

  /**
   * Expand all nodes
   */
  expandAll() {
    const toggles = this.options.container.querySelectorAll(".json-toggle");
    const containers =
      this.options.container.querySelectorAll(".json-container");

    toggles.forEach((toggle) => toggle.classList.add("expanded"));
    containers.forEach((container) => container.classList.remove("collapsed"));
  }

  /**
   * Collapse all nodes
   */
  collapseAll() {
    const toggles = this.options.container.querySelectorAll(".json-toggle");
    const containers =
      this.options.container.querySelectorAll(".json-container");

    toggles.forEach((toggle) => toggle.classList.remove("expanded"));
    containers.forEach((container) => container.classList.add("collapsed"));
  }
}

// Export functions for use in other modules
export {
  BASE_ROUTE,
  formatAddress,
  formatDisplayAmount,
  get,
  copyToClipboard,
  showTab,
  initTabListeners,
  SimpleJsonViewer,
  enableTooltipAndCopy,
};
