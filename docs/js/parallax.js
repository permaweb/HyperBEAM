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
