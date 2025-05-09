document.addEventListener('DOMContentLoaded', function() {
  const header = document.querySelector('.md-header');
  const mainContent = document.querySelector('.md-main');
  const mdContent = document.querySelector('.md-content');

  if (!header || !mainContent || !mdContent) {
    // console.error('Required elements (.md-header, .md-main, or .md-content) not found for header scroll behavior.');
    return;
  }

  // Function to add CSS transition to .md-content for smooth padding changes
  function addCssTransitionToMdContent() {
    const styleId = 'md-content-padding-transition-style';
    if (document.getElementById(styleId)) {
      return; // Style already added
    }
    const styleElement = document.createElement('style');
    styleElement.id = styleId;
    // Using a common transition timing, similar to what Material for MkDocs might use
    styleElement.textContent = `
      .md-content {
        transition: padding-top 0.25s cubic-bezier(0.4, 0, 0.2, 1); /* Faster transition */
      }
    `;
    document.head.appendChild(styleElement);
  }

  let headerHeight = header.offsetHeight;

  // Function to update paddings based on header state
  function updatePaddings() {
    // It's important to use the height of the header when it's visible
    // If header.classList.contains('header-hidden'), its offsetHeight might be its original height or 0 depending on CSS
    // For simplicity, we'll grab a fresh measurement if it's visible, or use the last known good height
    // This specific header's height doesn't change when 'header-hidden' is applied, only its transform.
    // So, header.offsetHeight should remain consistent.
    headerHeight = header.offsetHeight;

    if (header.classList.contains('header-hidden')) {
      if (mdContent) mdContent.style.paddingTop = '75px'; // Set to 100px when header is hidden
      document.documentElement.style.scrollPaddingTop = '0'; // scrollPaddingTop might still need to be 0 for anchors
    } else {
      if (mdContent) mdContent.style.paddingTop = headerHeight + 'px'; // Should animate
      document.documentElement.style.scrollPaddingTop = headerHeight + 'px';
    }
  }

  // Set initial state first, without transition
  updatePaddings();

  // Now add the transition style so subsequent changes are animated
  addCssTransitionToMdContent();

  window.addEventListener('scroll', function() {
    const scrollTop = window.scrollY || document.documentElement.scrollTop;

    if (scrollTop > 0) { // When scrolling down / header should be hidden
      if (!header.classList.contains('header-hidden')) {
        header.classList.add('header-hidden');
        updatePaddings(); // Update paddings after class change
      }
    } else { // When at the top / header should be visible
      if (header.classList.contains('header-hidden')) {
        header.classList.remove('header-hidden');
        updatePaddings(); // Update paddings after class change
      }
    }
  });

  window.addEventListener('resize', function() {
    // Recalculate and update paddings on resize
    updatePaddings();
  });
}); 