(function () {
  let currentPath = window.location.pathname;

  function updateHeaderAndMainClass() {
    const header = document.querySelector(".md-header");
    const main = document.querySelector("main");
    const tabs = document.querySelector(".md-tabs");

    if (!header || !main) {
      // console.warn("Header or main element not found for class update.");
      return;
    }

    const segments = window.location.pathname.split("/").filter(Boolean);
    const isArweaveIdPath = segments.length === 1 && segments[0].length === 43;
    const isRootPath = segments.length === 0;
    const isHomepage = isRootPath || isArweaveIdPath;

    if (isHomepage) {
      header.classList.add("custom-homepage-header");
      main.classList.add("custom-homepage-main");
      main.classList.remove("md-main");
      if (tabs) tabs.style.display = "none";
    } else {
      header.classList.remove("custom-homepage-header");
      main.classList.remove("custom-homepage-main");
      main.classList.add("md-main");
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
