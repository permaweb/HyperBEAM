import { showTab, copyToClipboard, initTabListeners, SimpleJsonViewer } from '/~hyperbuddy@1.0/utils.js';
import { fetchInfo, addConsole, addGraph } from '/~hyperbuddy@1.0/devices.js';
import { startFetchingMetrics } from '/~hyperbuddy@1.0/metrics.js';

/**
 * Fetch and display ledger data
 */
async function loadLedger() {
  const ledgerSection = document.getElementById('ledger-section');
  
  try {
    ledgerSection.innerHTML = '<div class="loading-info-line"><p>Loading ledger data...</p></div>';
    
    const response = await fetch(`${window.location.origin}/ledger~node-process@1.0/now/balance/serialize~json@1.0`);
    const data = await response.json();
    
    ledgerSection.innerHTML = '';
    
    const viewer = new SimpleJsonViewer({
      container: ledgerSection,
      data: data,
      theme: 'dark',
      expand: true
    });
    
  } catch (error) {
    // Remove the ledger tab button and content if the request fails
    const ledgerTabButton = document.querySelector('.tab-button[data-tab="ledger-tab"]');
    const ledgerTabContent = document.getElementById('ledger-tab');
    
    if (ledgerTabButton) {
      ledgerTabButton.remove();
    }
    if (ledgerTabContent) {
      ledgerTabContent.remove();
    }
    
    console.error('Failed to load ledger data:', error);
  }
}

/**
 * Initialize the application
 */
function init() {
  // Set up global event handlers
  window.copyToClipboard = copyToClipboard;
  
  // Initialize tab switching
  initTabListeners();
  
  // Fetch initial data
  fetchInfo();
  
  // Start fetching metrics periodically
  startFetchingMetrics();
  
  // Add console iframe
  addConsole();
  
  // Add graph iframe
  addGraph();
  
  // Load ledger data
  loadLedger();
}

// Initialize when DOM is fully loaded
document.addEventListener('DOMContentLoaded', init); 