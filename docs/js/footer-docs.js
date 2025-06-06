document.addEventListener("DOMContentLoaded", () => {
  // 1. Select the footer
  const footer = document.querySelector("footer.md-footer");
  if (!footer) {
    console.error("Footer not found!");
    return;
  }

  // 2. Find the first element inside it (the <nav>)
  const firstElement = footer.querySelector("nav");
  if (!firstElement) {
    console.error("No <nav> inside footer.");
    return;
  }

  // 3. Define your banner HTML (exactly as you provided)
  const bannerHTML = `
    <a href="https://github.com/permaweb/HyperBEAM">
      <div class="join-us-banner">
        <p>
          Made with 
          <svg width="12" height="12" viewBox="0 0 32 32" fill="none" xmlns="http://www.w3.org/2000/svg">
            <g clip-path="url(#clip0_658_3)">
              <rect x="-1" y="-2" width="34" height="34" fill="#DBDBDB"/>
              <path d="M16 26C16 26 4 19.6364 4 12.1364C4 10.5089 4.65645 8.94809 5.82495 7.7973C6.99345 6.64651 8.57827 6 10.2308 6C12.8373 6 15.07 7.39886 16 9.63636C16.93 7.39886 19.1627 6 21.7692 6C23.4217 6 25.0066 6.64651 26.175 7.7973C27.3435 8.94809 28 10.5089 28 12.1364C28 19.6364 16 26 16 26Z" fill="#EB4D4D"/>
            </g>
            <defs>
              <clipPath id="clip0_658_3">
                <rect width="32" height="32" fill="white"/>
              </clipPath>
            </defs>
          </svg>
           in <s>San Francisco</s> lol jk in cyberspace. Hungry to eat glass all day, <u>Join Us.</u> 
          <svg width="75.22" height="23" viewBox="0 0 122 39" fill="none" xmlns="http://www.w3.org/2000/svg">
            <!-- (long SVG path data omitted for brevity; use your full SVG here) -->
          </svg>
        </p>
      </div>
    </a>
  `;

  // 4. Inject it immediately after the <nav>
  firstElement.insertAdjacentHTML("afterend", bannerHTML);
});
