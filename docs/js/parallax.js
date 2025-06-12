document.addEventListener("DOMContentLoaded", () => {
  // 1) Verify we’re on the homepage
  const isHomepage =
    window.location.pathname === "/" ||
    window.location.pathname === "/index.html";

  if (!isHomepage) return;

  // 2) Grab header, scroll container, logo, and hero wrapper
  const header = document.querySelector(".custom-homepage-header");
  const scrollContainer = document.querySelector(".custom-homepage-main");
  const logo = header?.querySelector(".md-logo");
  const heroFloating = document.querySelector(".hero-floating-wrapper");

  if (!header || !scrollContainer || !logo || !heroFloating) {
    return console.log(
      "Missing .custom-homepage-header, .custom-homepage-main, .md-logo, or .hero-floating-wrapper."
    );
  }

  let needsUpdate = false;

  function updateHeaderFade() {
    needsUpdate = false;
    const scrollTop = scrollContainer.scrollTop;
    const vh = window.innerHeight;

    // HEADER fade window (unchanged)
    const headerFadeStart = vh * 1.35;
    const headerFadeEnd = vh * 1.45;

    // LOGO fade window (stay at 0.7vh → 0.8vh)
    const logoFadeStart = vh * 0.7;
    const logoFadeEnd = vh * 0.8;

    // HERO fade window moved up to 0.5vh → 0.6vh
    const heroFadeStart = vh * 0.6;
    const heroFadeEnd = vh * 0.8;

    // 1) Header opacity: 0 → 1 between 1.35vh and 1.45vh
    let headerOpacity;
    if (scrollTop <= headerFadeStart) {
      headerOpacity = 0;
    } else if (scrollTop >= headerFadeEnd) {
      headerOpacity = 1;
    } else {
      headerOpacity =
        (scrollTop - headerFadeStart) / (headerFadeEnd - headerFadeStart);
    }

    // 2) Logo opacity: 0 → 1 between 0.7vh and 0.8vh
    let logoOpacity;
    if (scrollTop <= logoFadeStart) {
      logoOpacity = 0;
    } else if (scrollTop >= logoFadeEnd) {
      logoOpacity = 1;
    } else {
      logoOpacity = (scrollTop - logoFadeStart) / (logoFadeEnd - logoFadeStart);
    }

    // 3) Hero opacity: 1 → 0 between 0.5vh and 0.6vh
    let heroOpacity;
    if (scrollTop <= heroFadeStart) {
      heroOpacity = 1;
    } else if (scrollTop >= heroFadeEnd) {
      heroOpacity = 0;
    } else {
      heroOpacity =
        1 - (scrollTop - heroFadeStart) / (heroFadeEnd - heroFadeStart);
    }

    // 4) Apply all fades
    header.style.backgroundColor = `rgba(255, 255, 255, ${headerOpacity})`;
    header.style.filter = `invert(${1 - headerOpacity})`;
    logo.style.opacity = logoOpacity;
    heroFloating.style.opacity = heroOpacity;
  }

  scrollContainer.addEventListener("scroll", () => {
    if (!needsUpdate) {
      needsUpdate = true;
      requestAnimationFrame(updateHeaderFade);
    }
  });

  window.addEventListener("resize", updateHeaderFade);
  requestAnimationFrame(updateHeaderFade);
});

// (Optional) Keep your “fade-in” logic for landing-page elements as-is
window.addEventListener("DOMContentLoaded", () => {
  const isHomepage =
    window.location.pathname === "/" ||
    window.location.pathname === "/index.html";

  if (isHomepage) {
    const preloaderHTML = `<div id="preloader">Loading<span></span></div>`;
    document.body.insertAdjacentHTML("afterbegin", preloaderHTML);
  }

  window.addEventListener("load", () => {
    if (!isHomepage) return;

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
  });
});
