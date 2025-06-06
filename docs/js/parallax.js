document.addEventListener("DOMContentLoaded", () => {
  // 1) Make sure we’re on the homepage (either by URL or by checking for .custom-homepage-header)
  const isHomepage =
    window.location.pathname === "/" ||
    window.location.pathname === "/index.html";

  if (!isHomepage) {
    // On any other page, do nothing: the logo stays as-is.
    return;
  }

  // 2) Grab only the homepage header and its logo
  const header = document.querySelector(".custom-homepage-header");
  const scrollContainer = document.querySelector(".custom-homepage-main");
  const logo = header?.querySelector(".md-logo");

  if (!header || !scrollContainer || !logo) {
    // If something is missing, bail out quietly
    return console.log(
      "Homepage-header or scroll-container or .md-logo not found."
    );
  }

  let needsUpdate = false;

  function updateHeaderFade() {
    needsUpdate = false;
    const scrollTop = scrollContainer.scrollTop;
    const vh = window.innerHeight;

    const fadeStart = vh * 1.35; // header BG starts at 135% of viewport height
    const fadeEnd = vh * 1.45; // fully opaque at 145%

    let opacity;
    if (scrollTop <= fadeStart) {
      opacity = 0;
    } else if (scrollTop >= fadeEnd) {
      opacity = 1;
    } else {
      opacity = (scrollTop - fadeStart) / (fadeEnd - fadeStart);
    }

    // 3) Apply fade to header background + invert filter
    header.style.backgroundColor = `rgba(255, 255, 255, ${opacity})`;
    header.style.filter = `invert(${1 - opacity})`;

    // 4) Sync the logo’s opacity
    logo.style.opacity = opacity;
  }

  scrollContainer.addEventListener("scroll", () => {
    if (!needsUpdate) {
      needsUpdate = true;
      requestAnimationFrame(updateHeaderFade);
    }
  });

  window.addEventListener("resize", updateHeaderFade);
  requestAnimationFrame(updateHeaderFade); // initialize on load
});

// (Optional) Your existing “fade-in for landing page elements” logic can remain untouched,
// because it already gated itself on isHomepage inside its own load handler.
window.addEventListener("DOMContentLoaded", () => {
  const isHomepage =
    window.location.pathname === "/" ||
    window.location.pathname === "/index.html";

  if (isHomepage) {
    const preloaderHTML = `<div id="preloader">Loading<span></span></div>`;
    document.body.insertAdjacentHTML("afterbegin", preloaderHTML);
  }

  window.addEventListener("load", () => {
    if (isHomepage) {
      const preloader = document.getElementById("preloader");
      if (preloader) {
        preloader.style.transition = "opacity 0.5s ease-out";
        preloader.style.opacity = "0";
        setTimeout(() => (preloader.style.display = "none"), 500);
      }

      document.querySelectorAll(".fade-in").forEach((el, index) => {
        const delay = el.classList.contains("nav")
          ? 2000
          : el.classList.contains("hero-text")
          ? 1500
          : 500 + index * 100;
        setTimeout(() => el.classList.add("visible"), delay);
      });
    }
  });
});
