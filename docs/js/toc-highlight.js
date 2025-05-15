document.addEventListener("DOMContentLoaded", function () {
  /**
   * Fixes navigation highlighting in MkDocs Material Theme:
   * 1. If a list item has both an active label and an active link, remove active from label
   * 2. If a parent item has active children, remove active from the parent's links
   * 3. When scroll position is at the top, reactivate the parent navigation item
   */
  function fixNavigationHighlighting() {
    // First fix case where both label and anchor in same item are active
    document.querySelectorAll(".md-nav__item").forEach(function (item) {
      const label = item.querySelector("label.md-nav__link--active");
      const link = item.querySelector("a.md-nav__link--active");

      // If both exist in the same item, keep only the link active
      if (label && link) {
        label.classList.remove("md-nav__link--active");
      }
    });

    // Check if scroll position is at the top
    const atTop = window.scrollY === 0;

    // Now fix nested navigation (parent sections shouldn't be active when children are, unless at top)
    document
      .querySelectorAll(".md-nav__item--active")
      .forEach(function (activeItem) {
        // Check if this active item contains other active items
        const hasActiveChildren = activeItem.querySelector(
          ".md-nav__link--active",
        );

        // console.log("Has Active Children:", hasActiveChildren);

        if (hasActiveChildren && !atTop) {
          // Remove active class from parent's links
          const parentLinks = activeItem.querySelectorAll(
            ":scope > a.md-nav__link--active, :scope > label.md-nav__link--active",
          );
          parentLinks.forEach(function (link) {
            link.classList.remove("md-nav__link--active");
          });
        } else if (!hasActiveChildren && atTop) {
          // Reactivate parent link if at top and no active children
          const parentLinks = activeItem.querySelectorAll(
            ":scope > a.md-nav__link, :scope > label.md-nav__link",
          );
          parentLinks.forEach(function (link) {
            link.classList.add("md-nav__link--active");
          });
        }
      });
  }

  // Initial run
  fixNavigationHighlighting();

  // Set up a mutation observer to detect changes
  const observer = new MutationObserver(function (mutations) {
    let shouldUpdate = false;

    for (const mutation of mutations) {
      if (
        mutation.type === "attributes" &&
        mutation.attributeName === "class" &&
        (mutation.target.classList.contains("md-nav__link--active") ||
          mutation.target.classList.contains("md-nav__item--active"))
      ) {
        shouldUpdate = true;
        break;
      }
    }

    if (shouldUpdate) {
      fixNavigationHighlighting();
    }
  });

  // Observe all navigation elements
  document
    .querySelectorAll(".md-nav__item, .md-nav__link")
    .forEach(function (el) {
      observer.observe(el, { attributes: true });
    });

  // Update on navigation events
  window.addEventListener("popstate", function () {
    setTimeout(fixNavigationHighlighting, 100);
  });

  window.addEventListener("load", fixNavigationHighlighting);

  // Update on scroll with throttling
  let scrollTimeout;
  window.addEventListener("scroll", function () {
    if (!scrollTimeout) {
      scrollTimeout = setTimeout(function () {
        fixNavigationHighlighting();
        scrollTimeout = null;
      }, 50);
    }
  });

  document.addEventListener("click", function (e) {
    if (e.target.closest(".md-nav__link")) {
      setTimeout(fixNavigationHighlighting, 50);
    }
  });

  // Add click event handling for navigation tabs
  const tabLinks = document.querySelectorAll('nav.md-tabs .md-tabs__list .md-tabs__item a');
  tabLinks.forEach(link => {
    link.addEventListener('click', function(event) {
      // Basic check if it's an internal link
      if (link.hostname === window.location.hostname || !link.hostname.length) {
        console.log('Tab clicked, forcing full reload for:', link.href);
        console.log('Updating navigation highlighting before navigation');
        fixNavigationHighlighting();
        event.preventDefault();
        event.stopPropagation();
        window.location.href = link.href;
      }
    }, true); // Use capture phase to catch the event before the theme's handler
  });
});
