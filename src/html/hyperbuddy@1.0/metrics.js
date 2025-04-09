// Import the required functions from utils.js
import { get, formatDisplayAmount } from '/~hyperbuddy@1.0/utils.js';

/**
 * Parse metrics data from Prometheus format
 * @param {string} text - The raw metrics text in Prometheus format
 * @returns {Object} - Organized metrics data
 */
function parseMetrics(text) {
  const lines = text.split("\n");
  const groups = {};
  let currentMetric = null;

  lines.forEach((line) => {
    line = line.trim();
    if (!line) return;

    if (line.startsWith("# TYPE")) {
      const parts = line.split(/\s+/);
      const metricName = parts[2];
      const metricType = parts[3];

      if (!groups[metricType]) {
        groups[metricType] = [];
      }

      currentMetric = {
        name: metricName,
        help: "",
        values: [],
      };

      groups[metricType].push(currentMetric);
    } else if (line.startsWith("# HELP")) {
      const parts = line.split(/\s+/);
      const metricName = parts[2];
      const helpText = parts.slice(3).join(" ");
      if (currentMetric && currentMetric.name === metricName) {
        currentMetric.help = helpText;
      }
    } else if (line.startsWith("#")) {
      // Skip other comments
    } else {
      if (currentMetric) {
        const match = line.match(/(-?\d+(\.\d+)?)(\s*)$/);
        if (match) {
          let label = currentMetric.name;
          const data = parseFloat(match[1]);

          const inputMatch = line.match(/\{([^}]*)\}/);
          if (inputMatch) {
            label = inputMatch[1];
          }

          if (label === `process_uptime_seconds`) {
            document.getElementById("uptime-value").innerHTML =
              formatDisplayAmount(data);
            startUpdatingUptime();
          }
          else if (label === `system_load`) {
            document.getElementById("system-value").innerHTML = formatDisplayAmount(data);
          }
          else if (currentMetric.name === 'event' && label.includes('topic="ao_result",event="ao_result"')) {
            document.getElementById("executions-value").innerHTML = formatDisplayAmount(data);
          }

          currentMetric.values.push({ label, data });
        }
      }
    }
  });

  if (groups.counter) {
    const spawnedProcesse = groups.counter.find(
      (item) => item.name === "cowboy_spawned_processes_total"
    );
    
    if (spawnedProcesse?.values) {
      const readsHandled = spawnedProcesse.values.find(
        (value) =>
          value.label.includes('method="GET",reason="normal",status_class="success"')
      )?.data || 0;
      
      const writesHandled = spawnedProcesse.values.find(
        (value) =>
          value.label.includes('method="POST",reason="normal",status_class="success"')
      )?.data || 0;
      
      document.getElementById("read-value").innerHTML =
        formatDisplayAmount(readsHandled);
      document.getElementById("write-value").innerHTML =
        formatDisplayAmount(writesHandled);
    }
  }

  return groups;
}

/**
 * Renders the metrics data in the UI
 * @param {Object} groups - The organized metrics data
 */
function renderMetricGroups(groups) {
  // Save the currently active category or default to "AO Events"
  let activeCategory = localStorage.getItem('activeMetricsCategory') || "AO Events";
  
  const container = document.getElementById("metrics-section");
  container.innerHTML = "";
  
  // Create category sections to organize metrics
  const categories = {
    "AO Events": ["event"],
    "System Stats": ["process_uptime_seconds", "system_load", "outbound_connections"],
    "HTTP & Requests": ["cowboy_requests_total", "cowboy_protocol_upgrades_total", "cowboy_spawned_processes_total", "cowboy_errors_total", "cowboy_early_errors_total", "cowboy_request_duration_seconds", "http_request_duration_seconds", "cowboy_receive_body_duration_seconds"],
    "Memory": ["erlang_vm_memory_atom_bytes_total", "erlang_vm_memory_bytes_total", "erlang_vm_memory_dets_tables", "erlang_vm_memory_ets_tables", "erlang_vm_memory_processes_bytes_total", "erlang_vm_memory_system_bytes_total"],
    "Network Stats": ["http_client_uploaded_bytes_total", "http_client_downloaded_bytes_total", "gun_requests_total"],
    "Telemetry": ["telemetry_scrape_size_bytes", "telemetry_scrape_duration_seconds", "telemetry_scrape_encoded_size_bytes"],
    "VM Stats": ["erlang_vm_msacc_aux_seconds_total", "erlang_vm_msacc_check_io_seconds_total", "erlang_vm_msacc_emulator_seconds_total", "erlang_vm_msacc_gc_seconds_total", "erlang_vm_msacc_other_seconds_total"]
  };
  
  // Build an index to quickly find metrics
  const metricsIndex = {};
  for (const type in groups) {
    groups[type].forEach(metric => {
      metricsIndex[metric.name] = { metric, type };
    });
  }
  
  // Create the metrics navbar
  const navbar = document.createElement("div");
  navbar.classList.add("metrics-navbar");
  
  // Create content container to hold the metrics content
  const metricsContent = document.createElement("div");
  metricsContent.classList.add("metrics-content");
  
  // Create and add all category sections first (hidden initially)
  const categoryContainers = {};
  
  // Function to show the selected category
  const showCategory = (categoryName) => {
    // Hide all categories
    Object.values(categoryContainers).forEach(container => {
      container.style.display = "none";
    });
    
    // Show selected category
    if (categoryContainers[categoryName]) {
      categoryContainers[categoryName].style.display = "block";
    }
    
    // Update active tab in navbar
    document.querySelectorAll('.metrics-nav-item').forEach(item => {
      if (item.textContent === categoryName) {
        item.classList.add('active');
      } else {
        item.classList.remove('active');
      }
    });
    
    // Save active category to localStorage
    localStorage.setItem('activeMetricsCategory', categoryName);
    activeCategory = categoryName;
  };
  
  // Create "Other Metrics" category for uncategorized metrics
  const otherMetrics = [];
  for (const type in groups) {
    groups[type].forEach(metric => {
      let isCategorized = false;
      for (const metricNames of Object.values(categories)) {
        if (metricNames.includes(metric.name)) {
          isCategorized = true;
          break;
        }
      }
      if (!isCategorized) {
        otherMetrics.push({ metric, type });
      }
    });
  }
  
  if (otherMetrics.length > 0) {
    categories["Other Metrics"] = [];
  }
  
  // Create navbar items
  for (const categoryName in categories) {
    // Create navbar item
    const navItem = document.createElement("div");
    navItem.classList.add("metrics-nav-item");
    navItem.textContent = categoryName;
    
    if (categoryName === activeCategory) {
      navItem.classList.add('active');
    }
    
    navItem.addEventListener('click', () => {
      showCategory(categoryName);
    });
    
    navbar.appendChild(navItem);
    
    // Create section container for this category
    const categoryContainer = document.createElement("div");
    categoryContainer.classList.add("metrics-category");
    categoryContainer.style.display = categoryName === activeCategory ? "block" : "none";
    categoryContainers[categoryName] = categoryContainer;
    
    // Build the category's content
    if (categoryName === "Other Metrics") {
      otherMetrics.forEach(({ metric, type }) => {
        const metricContainer = createMetricDisplay(metric, type, categoryName);
        categoryContainer.appendChild(metricContainer);
      });
    } else {
      let hasMetrics = false;
      for (const metricName of categories[categoryName]) {
        if (!metricsIndex[metricName]) continue;
        
        hasMetrics = true;
        const { metric, type } = metricsIndex[metricName];
        
        const metricContainer = createMetricDisplay(metric, type, categoryName);
        categoryContainer.appendChild(metricContainer);
      }
      
      if (!hasMetrics) {
        const noDataMsg = document.createElement("div");
        noDataMsg.textContent = "No data available for this category";
        noDataMsg.style.padding = "20px";
        noDataMsg.style.textAlign = "center";
        categoryContainer.appendChild(noDataMsg);
      }
    }
    
    metricsContent.appendChild(categoryContainer);
  }
  
  // Add navbar and content to container
  container.appendChild(navbar);
  container.appendChild(metricsContent);
  
  // Helper function to create metric display
  function createMetricDisplay(metric, type, categoryName) {
    const metricContainer = document.createElement("div");
    metricContainer.classList.add("metric-container");
    
    // Skip the metric header for the AO Events section
    if (!(categoryName === "AO Events" && metric.name === "event")) {
      const subheader = document.createElement("div");
      subheader.classList.add("metrics-section-lines-header");
      subheader.classList.add("section-lines-header");
      
      const metricTitle = document.createElement("p");
      metricTitle.textContent = metric.name + " (" + type + ")";
      subheader.appendChild(metricTitle);
      
      metricContainer.appendChild(subheader);
    }
    
    // Add help text if available
    if (metric.help && !(categoryName === "AO Events" && metric.name === "event")) {
      const helpDiv = document.createElement("div");
      helpDiv.classList.add("section-line");
      helpDiv.style.fontStyle = "italic";
      helpDiv.style.fontSize = "12px";
      helpDiv.style.color = "#666";
      helpDiv.textContent = metric.help;
      metricContainer.appendChild(helpDiv);
    }
    
    const metricBody = document.createElement("div");
    metricBody.classList.add("section-lines");
    
    // Special handling for event metrics - group by topic
    if (metric.name === "event") {
      const eventsByTopic = {};
      
      metric.values.forEach(value => {
        const match = value.label.match(/topic="([^"]+)"/);
        if (match) {
          const topic = match[1];
          if (!eventsByTopic[topic]) {
            eventsByTopic[topic] = [];
          }
          eventsByTopic[topic].push(value);
        }
      });
      
      for (const [topic, events] of Object.entries(eventsByTopic)) {
        const topicHeader = document.createElement("div");
        topicHeader.classList.add("section-line");
        topicHeader.style.fontWeight = "bold";
        topicHeader.textContent = `Topic: ${topic}`;
        metricBody.appendChild(topicHeader);
        
        events.forEach(event => {
          const eventMatch = event.label.match(/event="([^"]+)"/);
          if (eventMatch) {
            const eventLine = document.createElement("div");
            eventLine.classList.add("section-line");
            
            const eventName = document.createElement("p");
            eventName.textContent = eventMatch[1];
            eventName.style.paddingLeft = "20px";
            
            const eventValue = document.createElement("p");
            eventValue.textContent = formatDisplayAmount(event.data);
            
            eventLine.appendChild(eventName);
            eventLine.appendChild(eventValue);
            metricBody.appendChild(eventLine);
          }
        });
      }
    } else if (metric.values.length > 0) {
      // Sort values for better readability
      const sortedValues = [...metric.values].sort((a, b) => {
        return a.label.localeCompare(b.label);
      });
      
      // Handle histogram metrics - group by method
      if (metric.name.includes('duration_seconds') && metric.name.includes('bucket')) {
        const valuesByMethod = {};
        
        sortedValues.forEach(value => {
          const methodMatch = value.label.match(/method="([^"]+)"/);
          const method = methodMatch ? methodMatch[1] : 'unknown';
          
          if (!valuesByMethod[method]) {
            valuesByMethod[method] = [];
          }
          valuesByMethod[method].push(value);
        });
        
        for (const method in valuesByMethod) {
          const methodHeader = document.createElement("div");
          methodHeader.classList.add("section-line");
          methodHeader.style.fontWeight = "bold";
          methodHeader.textContent = `Method: ${method}`;
          metricBody.appendChild(methodHeader);
          
          // Only show summary stats for histograms to avoid clutter
          const count = valuesByMethod[method].find(v => v.label.includes('count'))?.data || 0;
          const sum = valuesByMethod[method].find(v => v.label.includes('sum'))?.data || 0;
          const avg = count > 0 ? sum / count : 0;
          
          const statsLine = document.createElement("div");
          statsLine.classList.add("section-line");
          statsLine.style.paddingLeft = "20px";
          
          statsLine.innerHTML = `
            <p>Count: ${formatDisplayAmount(count)}</p>
            <p>Sum: ${formatDisplayAmount(sum)}</p>
            <p>Avg: ${formatDisplayAmount(avg.toFixed(4))}</p>
          `;
          metricBody.appendChild(statsLine);
        }
      } else {
        // Group HTTP metrics by method if they contain method in their labels
        if (categoryName === "HTTP & Requests" && 
            (metric.name.includes('cowboy') || metric.name.includes('http')) && 
            sortedValues.some(v => v.label.includes('method='))) {
          
          // Group values by HTTP method
          const valuesByMethod = {};
          
          sortedValues.forEach(value => {
            const methodMatch = value.label.match(/method="([^"]+)"/);
            const method = methodMatch ? methodMatch[1] : 'unknown';
            
            if (!valuesByMethod[method]) {
              valuesByMethod[method] = [];
            }
            valuesByMethod[method].push(value);
          });
          
          // Display metrics grouped by method
          for (const [method, values] of Object.entries(valuesByMethod)) {
            const methodHeader = document.createElement("div");
            methodHeader.classList.add("section-line");
            methodHeader.style.fontWeight = "bold";
            methodHeader.style.backgroundColor = "#f0f0f0";
            methodHeader.textContent = `Method: ${method}`;
            metricBody.appendChild(methodHeader);
            
            values.forEach(valueMap => {
              const metricLine = document.createElement("div");
              metricLine.classList.add("section-line");
              metricLine.style.paddingLeft = "20px";
              
              const labelEl = document.createElement("p");
              // Extract just the reason and status for cleaner display
              let displayLabel = "(default)";
              
              const reasonMatch = valueMap.label.match(/reason="([^"]+)"/);
              const statusMatch = valueMap.label.match(/status_class="([^"]+)"/);
              const errorMatch = valueMap.label.match(/error="([^"]+)"/);
              
              if (reasonMatch || statusMatch) {
                displayLabel = [];
                if (reasonMatch) displayLabel.push(`reason: ${reasonMatch[1]}`);
                if (statusMatch) displayLabel.push(`status: ${statusMatch[1]}`);
                if (errorMatch) displayLabel.push(`error: ${errorMatch[1]}`);
                displayLabel = displayLabel.join(', ');
              } else {
                displayLabel = valueMap.label.replace(/method="[^"]+",\s*/, '');
                displayLabel = displayLabel === metric.name ? "(default)" : displayLabel;
              }
              
              labelEl.textContent = displayLabel;
              
              const valueEl = document.createElement("p");
              valueEl.textContent = formatDisplayAmount(valueMap.data);
              
              metricLine.appendChild(labelEl);
              metricLine.appendChild(valueEl);
              metricBody.appendChild(metricLine);
            });
          }
        } else {
          // Limit number of displayed values if too many
          const maxValues = 15;
          const valuesToShow = sortedValues.length > maxValues ? 
            sortedValues.slice(0, maxValues) : sortedValues;
          
          if (sortedValues.length > maxValues) {
            const noticeDiv = document.createElement("div");
            noticeDiv.classList.add("section-line");
            noticeDiv.style.fontStyle = "italic";
            noticeDiv.textContent = `Showing ${maxValues} of ${sortedValues.length} values`;
            metricBody.appendChild(noticeDiv);
          }
          
          // Default rendering for non-HTTP metrics or metrics without method labels
          valuesToShow.forEach(valueMap => {
            const metricLine = document.createElement("div");
            metricLine.classList.add("section-line");
            
            const labelEl = document.createElement("p");
            // For better readability, format complex labels
            const displayLabel = valueMap.label === metric.name ? 
              "(default)" : valueMap.label.replace(/,/g, ', ');
            labelEl.textContent = displayLabel;
            
            const valueEl = document.createElement("p");
            valueEl.textContent = formatDisplayAmount(valueMap.data);
            
            metricLine.appendChild(labelEl);
            metricLine.appendChild(valueEl);
            metricBody.appendChild(metricLine);
          });
        }
      }
    } else {
      const metricLine = document.createElement("div");
      metricLine.classList.add("section-line");
      const metricValueLabel = document.createElement("p");
      metricValueLabel.textContent = "No data";
      metricLine.appendChild(metricValueLabel);
      const metricValueData = document.createElement("p");
      metricValueData.textContent = "-";
      metricLine.appendChild(metricValueData);
      metricBody.appendChild(metricLine);
    }
    
    metricContainer.appendChild(metricBody);
    return metricContainer;
  }
}

/**
 * Fetches metrics data from the server
 */
async function fetchMetrics() {
  try {
    const metrics = await get("/~hyperbuddy@1.0/metrics");
    const metricGroups = parseMetrics(metrics);
    renderMetricGroups(metricGroups);
  } catch (error) {
    console.error("Error fetching or parsing metrics:", error);
  }
}

/**
 * Updates the uptime value on a regular basis
 */
function startUpdatingUptime() {
  const uptimeElement = document.getElementById("uptime-value");
  if (!uptimeElement) return;

  const initialUptime =
    parseFloat(uptimeElement.innerHTML.replaceAll(",", "")) || 0;
  const startTime = Date.now();

  function updateUptime() {
    const elapsedSeconds = (Date.now() - startTime) / 1000;
    const currentUptime = initialUptime + elapsedSeconds;

    uptimeElement.innerHTML = formatDisplayAmount(
      currentUptime.toFixed(2)
    );
    requestAnimationFrame(updateUptime);
  }

  updateUptime();
}

/**
 * Start fetching metrics periodically
 */
async function startFetchingMetrics() {
  await fetchMetrics();
  
  // Initialize metrics display
  if (!localStorage.getItem('activeMetricsCategory')) {
    localStorage.setItem('activeMetricsCategory', 'AO Events');
  }

  setInterval(fetchMetrics, 50000);
}

// Export functions for use in other modules
export {
  parseMetrics,
  renderMetricGroups,
  fetchMetrics,
  startUpdatingUptime,
  startFetchingMetrics
}; 