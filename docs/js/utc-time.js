window.addEventListener("DOMContentLoaded", () => {
  const timeDiv = document.createElement("div");
  timeDiv.id = "utc-time";
  timeDiv.style.cssText = `
	  font-size: clamp(0.4rem, 1.5vw, 0.5rem);
	  user-select: none;
	`;

  const updateTime = () => {
    const now = new Date();
    timeDiv.textContent = "UTC " + now.toISOString().substring(11, 19); // HH:MM:SS
  };

  updateTime();
  setInterval(updateTime, 1000); // update every second

  const headerOptions = document.querySelector(".md-header__option");
  if (headerOptions) {
    headerOptions.replaceWith(timeDiv); // Replace existing element
  }
});
