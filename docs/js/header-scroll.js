document.addEventListener('DOMContentLoaded', function() {
  const header = document.querySelector('.md-header');
  const paddingTargetElement = document.querySelector('.md-content'); // Element for padding adjustments
  const contentVisibilityTargetElement = document.querySelector('.md-main__inner.md-grid'); // Element to hide/show with transition

  if (!header || !paddingTargetElement || !contentVisibilityTargetElement) {
    console.error('Header scroll: Required elements (.md-header, .md-content, or .md-main__inner.md-grid) not found.');
    return;
  }

  const HIDING_CLASS = 'content--initializing';

  // Function to inject CSS for transition and initial hiding
  function injectTransitionStyles() {
    const styleId = 'md-content-transition-style';
    if (document.getElementById(styleId)) {
      return; // Style already added
    }
    const styleElement = document.createElement('style');
    styleElement.id = styleId;
    styleElement.textContent = `
      .md-main__inner.md-grid { /* Style for the element to be shown with transition */
        opacity: 0;
        transition: opacity 200ms ease-in-out; /* Tiny transition */
      }
      .${HIDING_CLASS} { /* Class to initially hide the content */
        display: none !important;
        opacity: 0 !important; /* Ensure opacity is 0 when hidden */
      }
    `;
    document.head.appendChild(styleElement);
  }

  // Initially hide the content and set up for transition
  injectTransitionStyles();
  contentVisibilityTargetElement.classList.add(HIDING_CLASS);

  let headerHeight = 0;

  // Function to update paddings based on header state
  function updatePaddings() {
    const currentHeaderHeight = header.offsetHeight;
    if (currentHeaderHeight > 0) {
        headerHeight = currentHeaderHeight;
    }

    if (header.classList.contains('header-hidden')) {
      if (paddingTargetElement) paddingTargetElement.style.paddingTop = '75px';
      document.documentElement.style.scrollPaddingTop = '0';
    } else {
      if (paddingTargetElement) paddingTargetElement.style.paddingTop = headerHeight + 'px';
      document.documentElement.style.scrollPaddingTop = headerHeight + 'px';
    }
  }

  // Function to initialize header state and reveal content
  function initializeHeaderState() {
    headerHeight = header.offsetHeight;
    updatePaddings(); // Apply padding to paddingTargetElement

    // Make content displayable (it's still opacity 0 due to injected styles)
    contentVisibilityTargetElement.classList.remove(HIDING_CLASS);

    // Trigger the opacity transition to fade in the content
    requestAnimationFrame(() => {
      contentVisibilityTargetElement.style.opacity = 1;
    });
  }

  window.addEventListener('load', initializeHeaderState);

  window.addEventListener('scroll', function() {
    const scrollTop = window.scrollY || document.documentElement.scrollTop;

    if (scrollTop > 0) { // When scrolling down / header should be hidden
      if (!header.classList.contains('header-hidden')) {
        header.classList.add('header-hidden');
        updatePaddings();
      }
    } else { // When at the top / header should be visible
      if (header.classList.contains('header-hidden')) {
        header.classList.remove('header-hidden');
        updatePaddings();
      }
    }
  });

  window.addEventListener('resize', function() {
    updatePaddings();
  });
}); 