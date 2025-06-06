document.addEventListener("DOMContentLoaded", () => {
  // 1) Make sure we’re on the homepage (either by URL or by checking for .custom-homepage-header)
  const isHomepage =
    window.location.pathname === "/" ||
    window.location.pathname === "/index.html";

  if (!isHomepage) {
    // On any other page, do nothing: the logo stays as-is.
    return;
  }

  // 2) Grab only the homepage header, its logo, and the hero wrapper
  const header = document.querySelector(".custom-homepage-header");
  const scrollContainer = document.querySelector(".custom-homepage-main");
  const logo = header?.querySelector(".md-logo");
  const heroFloating = document.querySelector(".hero-floating-wrapper");

  if (!header || !scrollContainer || !logo || !heroFloating) {
    return console.log(
      "Homepage-header, scroll-container, .md-logo, or .hero-floating-wrapper not found."
    );
  }

  let needsUpdate = false;

  function updateHeaderFade() {
    needsUpdate = false;
    const scrollTop = scrollContainer.scrollTop;
    const vh = window.innerHeight;

    const headerFadeStart = vh * 1.35; // header BG starts at 135% of viewport height
    const headerFadeEnd = vh * 1.45; // fully opaque at 145%

    const logoFadeStart = vh * 1.1;
    const logoFadeEnd = vh * 1.2;

    // Calculate header opacity (0 → 1 between headerFadeStart and headerFadeEnd)
    let headerOpacity;
    if (scrollTop <= headerFadeStart) {
      headerOpacity = 0;
    } else if (scrollTop >= headerFadeEnd) {
      headerOpacity = 1;
    } else {
      headerOpacity =
        (scrollTop - headerFadeStart) / (headerFadeEnd - headerFadeStart);
    }

    // Calculate logo opacity (0 → 1 between logoFadeStart and logoFadeEnd)
    let logoOpacity;
    if (scrollTop <= logoFadeStart) {
      logoOpacity = 0;
    } else if (scrollTop >= logoFadeEnd) {
      logoOpacity = 1;
    } else {
      logoOpacity = (scrollTop - logoFadeStart) / (logoFadeEnd - logoFadeStart);
    }

    // 3) Apply fade to header background + invert filter
    header.style.backgroundColor = `rgba(255, 255, 255, ${headerOpacity})`;
    header.style.filter = `invert(${1 - headerOpacity})`;

    // 4) Sync the logo’s opacity
    logo.style.opacity = logoOpacity;

    // 5) Fade out .hero-floating-wrapper in the same window (inverse of headerOpacity)
    heroFloating.style.opacity = 1 - headerOpacity;
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
