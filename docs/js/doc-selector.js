/*
 * DocSelector Component Loader
 * Minimal implementation for loading DocSelector with theme detection
 */

// Simple MkDocs Material theme detection
function detectTheme() {
  const scheme = document.body?.getAttribute('data-md-color-scheme');
  return scheme === 'slate' ? 'dark' : 'light';
}

// Configuration
window.DocSelectorConfig = {
  currentCookbook: "HYPERBEAM",
  links: {
    AO: "https://cookbook_ao.arweave.net/welcome/ao-core-introduction.html",
    HYPERBEAM: "https://hyperbeam.arweave.net/build/introduction/what-is-hyperbeam.html",
    ARWEAVE: "https://cookbook.arweave.net/getting-started/index.html",
  },
  theme: detectTheme()
};

// Load DocSelector script
function loadDocSelector() {
  // Remove existing script and component
  document.querySelector('script[src*="localhost:8081/doc-selector.js"]')?.remove();
  document.querySelector('[data-doc-selector="true"]')?.remove();
  
  // Create new script
  const script = document.createElement("script");
  script.src = "https://arweave.net/uUdfnAHLxvRswVdGTiLg4_RXYUIb_4BvyTxVQ8m1X28";
  script.async = true;
  document.head.appendChild(script);
}

// Update theme and reload component
function updateTheme() {
  const newTheme = detectTheme();
  if (newTheme !== window.DocSelectorConfig.theme) {
    window.DocSelectorConfig.theme = newTheme;
    loadDocSelector();
  }
}

// Initialize
(function() {
  // Skip on homepage
  if (window.location.pathname === '/' || window.location.pathname === '/index.html') {
    return;
  }
  
  // Load initial component
  loadDocSelector();
  
  // Watch for theme changes
  if (document.body) {
    new MutationObserver(() => updateTheme()).observe(document.body, {
      attributes: true,
      attributeFilter: ['data-md-color-scheme']
    });
  }
})(); 