import { showTab, copyToClipboard, initTabListeners } from '/~hyperbuddy@1.0/utils.js';
import { fetchInfo, addConsole } from '/~hyperbuddy@1.0/devices.js';
import { startFetchingMetrics } from '/~hyperbuddy@1.0/metrics.js';

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
}

// Initialize when DOM is fully loaded
document.addEventListener('DOMContentLoaded', init); 