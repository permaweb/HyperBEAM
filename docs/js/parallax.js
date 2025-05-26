document.addEventListener("DOMContentLoaded", () => {
  const header = document.querySelector(".custom-homepage-header");
  const scrollContainer = document.querySelector(".custom-homepage-main");

  if (!header || !scrollContainer)
    return console.log("Missing header or scroll container");

  let needsUpdate = false;

  function updateHeaderFade() {
    needsUpdate = false;
    const scrollTop = scrollContainer.scrollTop;

    const fadeStart = window.innerHeight * 1.35; // fade starts 80% into hero
    const fadeEnd = window.innerHeight * 1.45; // fade finishes at 120%

    let opacity;
    if (scrollTop <= fadeStart) {
      opacity = 0;
    } else if (scrollTop >= fadeEnd) {
      opacity = 1;
    } else {
      opacity = (scrollTop - fadeStart) / (fadeEnd - fadeStart);
    }

    header.style.backgroundColor = `rgba(255, 255, 255, ${opacity})`;
    header.style.filter = `invert(${1 - opacity})`;
  }

  scrollContainer.addEventListener("scroll", () => {
    if (!needsUpdate) {
      needsUpdate = true;
      requestAnimationFrame(updateHeaderFade);
    }
  });

  window.addEventListener("resize", updateHeaderFade);
  requestAnimationFrame(updateHeaderFade); // run on load
});

// Fade in slowly all landing page elements

window.addEventListener("load", () => {
  const isHomepage =
    window.location.pathname === "/" ||
    window.location.pathname === "/index.html";

  if (isHomepage) {
    // Only run preloader fade out on the homepage
    const preloader = document.getElementById("preloader");
    if (preloader) {
      preloader.style.transition = "opacity 0.5s ease-out";
      preloader.style.opacity = "0";
      setTimeout(() => (preloader.style.display = "none"), 500);
    }

    // Fade in elements on the homepage
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
