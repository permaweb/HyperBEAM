(function () {
  let currentPath = window.location.pathname;
  let savedTheme = null; // Store user's preferred theme

  function updateHeaderAndMainClass() {
    const header = document.querySelector(".md-header");
    const main = document.querySelector("main");
    const body = document.querySelector("body");
    const tabs = document.querySelector(".md-tabs");
    const drawerIcon = document.querySelector(".md-icon");
    console.log(drawerIcon, "drawericon");
    if (!header || !main) {
      // console.warn("Header or main element not found for class update.");
      return;
    }

    const segments = window.location.pathname.split("/").filter(Boolean);
    const isArweaveIdPath = segments.length === 1 && segments[0].length === 43;
    const isRootPath = segments.length === 0;
    const isHomepage = isRootPath || isArweaveIdPath;

    if (isHomepage) {
      // Always save current theme before switching to default (unless already default)
      const currentScheme = document.body.dataset.mdColorScheme || localStorage.getItem("mdColorScheme") || "default";
      const currentPrimary = document.body.dataset.mdColorPrimary || localStorage.getItem("mdColorPrimary") || "white";
      
      // Only save if it's not already the default theme
      if (currentScheme !== "default" || currentPrimary !== "white") {
        savedTheme = {
          colorScheme: currentScheme,
          colorPrimary: currentPrimary
        };
      }
      
      // Step 1: Force default theme FIRST
      document.body.dataset.mdColorScheme = "default";
      document.body.dataset.mdColorPrimary = "white";
      localStorage.setItem("mdColorScheme", "default");
      localStorage.setItem("mdColorPrimary", "white");
      
      // Step 2: Update VPSwitch button state to match default theme
      const vpSwitchButton = document.querySelector(".VPSwitch");
      if (vpSwitchButton) {
        const checkElement = vpSwitchButton.querySelector(".check");
        const sunIcon = vpSwitchButton.querySelector(".vpi-sun");
        const moonIcon = vpSwitchButton.querySelector(".vpi-moon");
        
        // Set to light mode state
        vpSwitchButton.ariaChecked = "false";
        vpSwitchButton.title = "Switch to dark theme";
        if (checkElement) checkElement.style.transform = "translateX(0)";
        if (sunIcon) sunIcon.style.opacity = "1";
        if (moonIcon) moonIcon.style.opacity = "0";
        vpSwitchButton.style.backgroundColor = "#eee";
        vpSwitchButton.style.borderColor = "#ccc";
        if (checkElement) checkElement.style.backgroundColor = "#fff";
      }
      
      // Step 3: Force CSS re-evaluation with small delay
      setTimeout(() => {
        header.classList.add("custom-homepage-header");
        main.classList.add("custom-homepage-main");
        main.classList.remove("md-main");
        body.style.background = "#000";
        drawerIcon.style.display = "none";
        if (tabs) tabs.style.display = "none";
      }, 10);
      
    } else {
      // Restore user's preferred theme when leaving homepage
      if (savedTheme) {
        // Step 1: Restore theme attributes and localStorage
        document.body.dataset.mdColorScheme = savedTheme.colorScheme;
        document.body.dataset.mdColorPrimary = savedTheme.colorPrimary;
        localStorage.setItem("mdColorScheme", savedTheme.colorScheme);
        localStorage.setItem("mdColorPrimary", savedTheme.colorPrimary);
        
        // Step 2: Restore VPSwitch button state
        const vpSwitchButton = document.querySelector(".VPSwitch");
        if (vpSwitchButton) {
          const checkElement = vpSwitchButton.querySelector(".check");
          const sunIcon = vpSwitchButton.querySelector(".vpi-sun");
          const moonIcon = vpSwitchButton.querySelector(".vpi-moon");
          const isDark = savedTheme.colorScheme === "slate";
          
          // Set button state based on saved theme
          vpSwitchButton.ariaChecked = isDark.toString();
          vpSwitchButton.title = isDark ? "Switch to light theme" : "Switch to dark theme";
          if (checkElement) checkElement.style.transform = isDark ? "translateX(18px)" : "translateX(0)";
          if (sunIcon) sunIcon.style.opacity = isDark ? "0" : "1";
          if (moonIcon) moonIcon.style.opacity = isDark ? "1" : "0";
          vpSwitchButton.style.backgroundColor = isDark ? "#333" : "#eee";
          vpSwitchButton.style.borderColor = isDark ? "#666" : "#ccc";
          if (checkElement) checkElement.style.backgroundColor = isDark ? "#000" : "#fff";
        }
        
        savedTheme = null; // Clear saved theme
      }
      
      // Step 3: Remove homepage classes
      header.classList.remove("custom-homepage-header");
      main.classList.remove("custom-homepage-main");
      main.classList.add("md-main");
      body.style.background = "";
      drawerIcon.style.display = "";
      if (tabs) tabs.style.display = "";
    }
  }

  // Initial run
  updateHeaderAndMainClass();

  // Function to handle path changes
  function handlePathChange() {
    if (window.location.pathname !== currentPath) {
      currentPath = window.location.pathname;
      updateHeaderAndMainClass();
    }
  }

  // Watch for URL changes via MutationObserver (for client-side navigation)
  const observer = new MutationObserver(handlePathChange);
  observer.observe(document.body, { childList: true, subtree: true });

  // Also listen for popstate (browser back/forward)
  window.addEventListener("popstate", updateHeaderAndMainClass);
})();

document.addEventListener("DOMContentLoaded", function () {
  function updateMainClass() {
    const mainElement = document.querySelector("main");
    const isHomepage = window.location.pathname === "/";

    // Apply the homepage class if on the homepage, else remove it
    if (isHomepage) {
      mainElement.classList.add("custom-homepage-main");
      mainElement.classList.remove("md-main");

      if (tabs) tabs.style.display = "none";
    } else {
      if (tabs) tabs.style.display = "";

      mainElement.classList.add("md-main");
      mainElement.classList.remove("custom-homepage-main");
    }
  }

  // Initial update on page load
  updateMainClass();

  // Listen for link clicks and update the class after navigation
  const links = document.querySelectorAll("a");
  links.forEach((link) => {
    link.addEventListener("click", function (event) {
      // Small delay to ensure the page has started loading
      setTimeout(updateMainClass, 0);
    });
  });

  // Listen for popstate events (back/forward navigation)
  window.addEventListener("popstate", function () {
    setTimeout(updateMainClass, 500);
  });
});
