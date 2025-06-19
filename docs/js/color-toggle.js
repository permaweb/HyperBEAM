window.addEventListener("DOMContentLoaded", () => {
  // Create button
  const button = document.createElement("button");
  button.className = "VPSwitch";
  button.type = "button";
  button.role = "switch";
  button.title = "Toggle theme";
  button.ariaChecked = "false";
  button.style.cssText = `
    position: relative;
    border-radius: 11px;
    display: block;
    width: 40px;
    height: 22px;
    border: 1px solid #ccc;
    background-color: #eee;
    cursor: pointer;
    margin-left: 1rem;
  `;

  // Create check span
  const check = document.createElement("span");
  check.className = "check";
  check.style.cssText = `
    position: absolute;
    top: 1px;
    left: 1px;
    width: 18px;
    height: 18px;
    border-radius: 50%;
    background-color: #fff;
    box-shadow: 0 1px 2px rgba(0,0,0,0.3);
    transition: transform 0.25s;
  `;

  // Create icon span
  const icon = document.createElement("span");
  icon.className = "icon";
  icon.style.cssText = `
    position: relative;
    display: block;
    width: 18px;
    height: 18px;
    border-radius: 50%;
    overflow: hidden;
  `;

  // Sun span
  const sun = document.createElement("span");
  sun.className = "vpi-sun";
  sun.style.cssText = `
    background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' fill='none' stroke='currentColor' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' viewBox='0 0 24 24'%3E%3Ccircle cx='12' cy='12' r='4'/%3E%3Cpath d='M12 2v2M12 20v2M4.93 4.93l1.41 1.41M17.66 17.66l1.41 1.41M2 12h2M20 12h2M6.34 17.66l-1.41 1.41M19.07 4.93l-1.41 1.41'/%3E%3C/svg%3E");
    background-repeat: no-repeat;
    background-position: center;
    background-size: contain;
    position: absolute;
    top: 3px;
    left: 3px;
    width: 12px;
    height: 12px;
    opacity: 1;
    transition: opacity 0.25s;
  `;

  // Moon span
  const moon = document.createElement("span");
  moon.className = "vpi-moon";
  moon.style.cssText = `
    background-image: url("data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' fill='none' stroke='white' stroke-linecap='round' stroke-linejoin='round' stroke-width='2' viewBox='0 0 24 24'%3E%3Cpath d='M12 3a6 6 0 0 0 9 9 9 9 0 1 1-9-9Z'/%3E%3C/svg%3E");
    background-repeat: no-repeat;
    background-position: center;
    background-size: contain;
    position: absolute;
    top: 3px;
    left: 3px;
    width: 12px;
    height: 12px;
    opacity: 0;
    transition: opacity 0.25s;
  `;

  // Build the structure
  icon.appendChild(sun);
  icon.appendChild(moon);
  check.appendChild(icon);
  button.appendChild(check);

  // if isDark is true, toggle to dark, otherwise toggle to light
  const toggleButtonToDark = (isDark, saveToLocalStorage = true) => {
    button.ariaChecked = isDark.toString();
    button.title = isDark ? "Switch to light theme" : "Switch to dark theme";

    // Move switch knob
    check.style.transform = isDark ? "translateX(18px)" : "translateX(0)";

    // Icon opacity
    sun.style.opacity = isDark ? "0" : "1";
    moon.style.opacity = isDark ? "1" : "0";

    // Update background + border
    button.style.backgroundColor = isDark ? "#333" : "#eee";
    button.style.borderColor = isDark ? "#666" : "#ccc";
    check.style.backgroundColor = isDark ? "#000" : "#fff";

    // Set variables based on theme
    const scheme = isDark ? "slate" : "default";
    const primary = isDark ? "black" : "white";

    // Store in localStorage
    if (saveToLocalStorage) {
      localStorage.setItem("mdColorScheme", scheme);
      localStorage.setItem("mdColorPrimary", primary);
    }

    // Set data attributes
    document.body.dataset.mdColorScheme = scheme;
    document.body.dataset.mdColorPrimary = primary;
  };

  // Handle toggle
  button.addEventListener("click", () => {
    // data-md-color-scheme="default" for light
    // data-md-color-scheme="slate" for dark
    if (document.body.dataset.mdColorScheme === "default") {
      toggleButtonToDark(true);
    } else {
      toggleButtonToDark(false);
    }
  });

  // Initialize from localStorage
  const colorScheme = localStorage.getItem("mdColorScheme");

  if (colorScheme) {
    const isDark = colorScheme !== "default";
    toggleButtonToDark(isDark);
  } else {
    // Set default values if nothing in localStorage
    document.body.dataset.mdColorScheme = "default";
    document.body.dataset.mdColorPrimary = "white";
  }

  // do not show the button on the landing page and ensure it is in light mode
  const isLandingPage = window.location.pathname === "/";
  if (isLandingPage) {
    // toggle to light mode and do not save this preference to localStorage
    toggleButtonToDark(false, false);
  } else {
    // Insert into DOM
    const headerOptions = document.querySelector(".md-header__source");
    if (headerOptions) {
      headerOptions.parentNode.insertBefore(button, headerOptions);
    }
  }
});
