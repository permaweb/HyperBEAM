
(function(){
  function addLineNumbers(pre, code) {
    if (pre.dataset.lineNumbers) return;
    var lines = (code.textContent || '').split('\n');
    if (lines.length && lines[lines.length - 1] === '') lines.pop();
    if (!lines.length) return;
    var gutter = document.createElement('div');
    gutter.className = 'code-line-numbers';
    gutter.setAttribute('aria-hidden', 'true');
    lines.forEach(function (_, i) {
      var span = document.createElement('span');
      span.textContent = String(i + 1);
      gutter.appendChild(span);
    });
    pre.classList.add('has-line-numbers');
    pre.insertBefore(gutter, code);
    pre.dataset.lineNumbers = '1';
  }

	  function addCopyButtons() {
	    document.querySelectorAll('.markdown-section pre').forEach(function (pre) {
      var code = pre.querySelector('code');
      if (!code || pre.dataset.codeChrome) return;
      addLineNumbers(pre, code);
      var lang = (pre.getAttribute('data-lang') || '').trim();
      var header = document.createElement('div');
      header.className = 'code-header';
      var langLabel = document.createElement('span');
      langLabel.className = 'code-header-lang';
      langLabel.textContent = lang || 'code';
      var btn = document.createElement('button');
      btn.type = 'button';
      btn.className = 'copy-code-btn';
      btn.setAttribute('aria-label', 'Copy code');
      btn.innerHTML =
        '<span class="copy-code-btn-stage" aria-hidden="true">' +
        '<span class="copy-code-btn-icon copy-code-btn-icon-copy">' +
        '<svg viewBox="0 0 256 256" fill="currentColor"><path d="M216,32H88a8,8,0,0,0-8,8V80H40a8,8,0,0,0-8,8V216a8,8,0,0,0,8,8H168a8,8,0,0,0,8-8V176h40a8,8,0,0,0,8-8V40A8,8,0,0,0,216,32ZM160,208H48V96H160Zm48-48H176V88a8,8,0,0,0-8-8H96V48H208Z"/></svg>' +
        '</span><span class="copy-code-btn-icon copy-code-btn-icon-check">' +
        '<svg viewBox="0 0 256 256" fill="currentColor"><path d="M232.49,80.49l-128,128a12,12,0,0,1-17,0l-56-56a12,12,0,1,1,17-17L96,183,215.51,63.51a12,12,0,0,1,17,17Z"/></svg>' +
        '</span></span>';
      btn.addEventListener('click', function () {
        var text = code.textContent || '';
        if (navigator.clipboard && navigator.clipboard.writeText) {
          navigator.clipboard.writeText(text).then(function () {
            btn.classList.add('copied');
            btn.setAttribute('aria-label', 'Copied');
            setTimeout(function () {
              btn.classList.remove('copied');
              btn.setAttribute('aria-label', 'Copy code');
            }, 1800);
          });
        }
      });
      header.appendChild(langLabel);
      header.appendChild(btn);
      pre.insertBefore(header, pre.firstChild);
      pre.classList.add('has-code-header');
      pre.dataset.codeChrome = '1';
	    });
	  }

	  window.HBDocsCodeChrome = {
	    refresh: function () {
	      addCopyButtons();
	    }
	  };
	})();

(function () {
  var tocScrollRoot = null;
  var tocScrollHandler = null;
  var tocScrollEndHandler = null;
  var tocResizeHandler = null;
  var tocObserver = null;
  var tocScrollRaf = null;

  function teardownPageTocScroll() {
    if (tocObserver) {
      tocObserver.disconnect();
      tocObserver = null;
    }
    if (tocScrollRaf) {
      window.cancelAnimationFrame(tocScrollRaf);
      tocScrollRaf = null;
    }
    if (tocScrollRoot && tocScrollHandler) {
      tocScrollRoot.removeEventListener('scroll', tocScrollHandler);
    }
    if (tocScrollRoot && tocScrollEndHandler) {
      tocScrollRoot.removeEventListener('scrollend', tocScrollEndHandler);
    }
    if (tocResizeHandler) {
      window.removeEventListener('resize', tocResizeHandler);
    }
    tocScrollRoot = null;
    tocScrollHandler = null;
    tocScrollEndHandler = null;
    tocResizeHandler = null;
  }

  function getScrollEl() {
    var content = document.querySelector('.content');
    if (content) {
      var overflowY = window.getComputedStyle(content).overflowY;
      if (/(auto|scroll|overlay)/.test(overflowY) && content.scrollHeight > content.clientHeight + 1) {
        return content;
      }
    }
    return window;
  }

  function getHeaderHeight() {
    var header = document.getElementById('site-header');
    if (!header || window.getComputedStyle(header).display === 'none') return 0;
    return header.getBoundingClientRect().height;
  }

  function getHeaderScrollOffset() {
    return getHeaderHeight() + 12;
  }

  function getAnchorIdFromHref(href) {
    if (!href) return '';
    var idMatch = href.match(/[?&]id=([^&]+)/);
    if (idMatch) return decodeURIComponent(idMatch[1]);
    var hash = href.split('#').pop() || '';
    return hash.replace(/^\//, '');
  }

  function slugify(text) {
    return String(text || '')
      .trim()
      .toLowerCase()
      .replace(/[^\w\s-]/g, '')
      .replace(/\s+/g, '-')
      .replace(/-+/g, '-');
  }

  function resolveHeadingLink(heading, usedIds) {
    var anchor = heading.querySelector('a.anchor');
    if (anchor) {
      var anchorId = getAnchorIdFromHref(anchor.getAttribute('href') || '');
      if (anchorId && !heading.id) heading.id = anchorId;
      if (heading.id) {
        usedIds[heading.id] = true;
        return { href: anchor.getAttribute('href') || ('#' + heading.id), id: heading.id };
      }
    }
    if (heading.id) {
      usedIds[heading.id] = true;
      return { href: '#' + heading.id, id: heading.id };
    }
    var base = slugify(heading.textContent || '');
    if (!base) return null;
    var id = base;
    var suffix = 2;
    while (usedIds[id]) {
      id = base + '-' + suffix;
      suffix += 1;
    }
    heading.id = id;
    usedIds[id] = true;
    return { href: '#' + id, id: id };
  }

  function addPageToc() {
    if (!document.body.classList.contains('hb-docs-protocol')) return;
    teardownPageTocScroll();
    document.body.classList.remove('page-toc-active');
    document.querySelector('.page-toc')?.remove();

    var markdownSection = document.querySelector('#main.markdown-section') ||
      document.querySelector('.markdown-section');
    var headings = markdownSection
      ? markdownSection.querySelectorAll('h2, h3')
      : document.querySelectorAll('.markdown-section h2, .markdown-section h3');
    if (!headings.length) return;

    var toc = document.createElement('aside');
    toc.className = 'page-toc';
    toc.setAttribute('aria-label', 'On this page');

    var tocNav = document.createElement('nav');
    tocNav.className = 'page-toc-nav';
    tocNav.setAttribute('aria-label', 'On this page sections');

    var tocProgress = document.createElement('div');
    tocProgress.className = 'page-toc-progress';
    tocProgress.setAttribute('aria-hidden', 'true');
    tocProgress.innerHTML =
      '<span class="page-toc-progress-rail"></span>' +
      '<span class="page-toc-progress-indicator"></span>';

    var tocLinksWrap = document.createElement('div');
    tocLinksWrap.className = 'page-toc-links';

    tocNav.appendChild(tocProgress);
    tocNav.appendChild(tocLinksWrap);
    toc.appendChild(tocNav);

    var tocLinks = [];
    var usedIds = {};
    headings.forEach(function (heading) {
      var linkInfo = resolveHeadingLink(heading, usedIds);
      if (!linkInfo) return;
      var link = document.createElement('a');
      link.href = linkInfo.href;
      link.textContent = (heading.textContent || '').trim();
      link.dataset.level = heading.tagName.toLowerCase();
      tocLinksWrap.appendChild(link);
      tocLinks.push({ link: link, heading: heading });
    });

    if (tocLinks.length <= 1) {
      return;
    }

    document.body.classList.add('page-toc-active');
    document.body.appendChild(toc);

    var scrollRoot = getScrollEl();
    var tocSpySuppressUntil = 0;

    function getScrollTop() {
      return scrollRoot === window ? window.pageYOffset : scrollRoot.scrollTop;
    }

    function getScrollHeight() {
      return scrollRoot === window
        ? Math.max(document.documentElement.scrollHeight, document.body.scrollHeight)
        : scrollRoot.scrollHeight;
    }

    function getViewportHeight() {
      return scrollRoot === window ? window.innerHeight : scrollRoot.clientHeight;
    }

    function getHeadingDocumentY(heading) {
      if (scrollRoot === window) {
        return heading.getBoundingClientRect().top + window.pageYOffset;
      }
      var rootRect = scrollRoot.getBoundingClientRect();
      return heading.getBoundingClientRect().top - rootRect.top + scrollRoot.scrollTop;
    }

    function scrollToY(targetY) {
      var top = Math.max(0, targetY);
      if (scrollRoot === window) {
        window.scrollTo({ top: top, behavior: 'smooth' });
      } else {
        scrollRoot.scrollTo({ top: top, behavior: 'smooth' });
      }
    }

    function scrollToHeading(heading, linkHref) {
      var offset = getHeaderScrollOffset();
      scrollToY(getHeadingDocumentY(heading) - offset);
      tocSpySuppressUntil = Date.now() + 900;
      if (linkHref) {
        history.replaceState(null, '', linkHref);
      }
      window.setTimeout(function () {
        setActiveTocLink(heading.id);
      }, 0);
    }

    function updatePageTocProgress() {
      var indicator = toc.querySelector('.page-toc-progress-indicator');
      var progress = toc.querySelector('.page-toc-progress');
      var active = tocLinksWrap.querySelector('a.active');
      if (!indicator || !progress || !active) {
        if (indicator) indicator.style.opacity = '0';
        return;
      }

      var progressRect = progress.getBoundingClientRect();
      var activeRect = active.getBoundingClientRect();
      var top = activeRect.top - progressRect.top;

      indicator.style.opacity = '1';
      indicator.style.height = activeRect.height + 'px';
      indicator.style.transform = 'translateY(' + top + 'px)';
    }

    function setActiveTocLink(targetId) {
      tocLinks.forEach(function (item) {
        var active = item.heading.id === targetId;
        item.link.classList.toggle('active', active);
      });
      updatePageTocProgress();
    }

    function resolveActiveTocIndex() {
      if (!tocLinks.length) return 0;

      var offset = getHeaderScrollOffset();
      var scrollTop = getScrollTop();
      var maxScroll = Math.max(0, getScrollHeight() - getViewportHeight());

      if (scrollTop <= 1) {
        return 0;
      }
      if (maxScroll > 0 && scrollTop >= maxScroll - 1) {
        return tocLinks.length - 1;
      }

      var anchorY = scrollTop + offset;
      var activeIndex = 0;
      for (var i = 0; i < tocLinks.length; i++) {
        if (getHeadingDocumentY(tocLinks[i].heading) <= anchorY + 1) {
          activeIndex = i;
        }
      }
      return activeIndex;
    }

    function syncActiveTocFromScroll() {
      if (!markdownSection || !tocLinks.length) return;
      if (Date.now() < tocSpySuppressUntil) return;
      var activeIndex = resolveActiveTocIndex();
      setActiveTocLink(tocLinks[activeIndex].heading.id);
    }

    function scheduleTocSync() {
      if (tocScrollRaf) return;
      tocScrollRaf = window.requestAnimationFrame(function () {
        tocScrollRaf = null;
        syncActiveTocFromScroll();
      });
    }

    tocLinksWrap.addEventListener('click', function (event) {
      var link = event.target.closest('a');
      if (!link || !tocLinksWrap.contains(link)) return;
      var linkHref = link.getAttribute('href') || '';
      var targetId = getAnchorIdFromHref(linkHref);
      var item = tocLinks.find(function (entry) {
        return entry.heading.id === targetId;
      });
      if (!item) return;
      event.preventDefault();
      scrollToHeading(item.heading, linkHref);
    });

    tocScrollHandler = scheduleTocSync;
    tocScrollEndHandler = syncActiveTocFromScroll;
    tocScrollRoot = scrollRoot === window ? window : scrollRoot;
    tocScrollRoot.addEventListener('scroll', tocScrollHandler, { passive: true });
    if ('onscrollend' in tocScrollRoot) {
      tocScrollRoot.addEventListener('scrollend', tocScrollEndHandler, { passive: true });
    }
    tocResizeHandler = scheduleTocSync;
    window.addEventListener('resize', tocResizeHandler, { passive: true });

    tocObserver = new IntersectionObserver(
      function () {
        scheduleTocSync();
      },
      {
        root: scrollRoot === window ? null : scrollRoot,
        rootMargin: '-' + getHeaderScrollOffset() + 'px 0px -70% 0px',
        threshold: [0, 1]
      }
    );
    tocLinks.forEach(function (item) {
      tocObserver.observe(item.heading);
    });

    window.requestAnimationFrame(function () {
      window.requestAnimationFrame(syncActiveTocFromScroll);
    });

    toc.__updatePageTocProgress = updatePageTocProgress;
    toc.__syncActiveTocFromScroll = syncActiveTocFromScroll;
  }

  if (!window.__pageTocProgressResizeBound) {
    window.__pageTocProgressResizeBound = true;
    window.addEventListener('resize', function () {
      var tocEl = document.querySelector('.page-toc');
      if (!tocEl) return;
      if (typeof tocEl.__syncActiveTocFromScroll === 'function') {
        tocEl.__syncActiveTocFromScroll();
      } else if (typeof tocEl.__updatePageTocProgress === 'function') {
        tocEl.__updatePageTocProgress();
      }
    }, { passive: true });
  }

  window.HBDocsPageToc = {
    init: addPageToc,
    refresh: addPageToc
  };
})();

(function () {
  function normalizePath(path) {
    return String(path || '/')
      .split('?')[0]
      .split('#')[0]
      .replace(/\/$/, '') || '/';
  }

  function getSidebarLinks() {
    return Array.from(document.querySelectorAll('.sidebar-nav a'))
      .map(function (a) {
        return {
          href: a.getAttribute('href') || '',
          text: (a.textContent || '').trim()
        };
      })
      .filter(function (item) {
        return item.href.charAt(0) === '/' && item.text;
      });
  }

  function getCurrentLinkIndex(links) {
    var current = normalizePath(
      document.body.getAttribute('data-active-path') || window.location.pathname
    );
    return links.findIndex(function (item) {
      return normalizePath(item.href) === current;
    });
  }

  function addFooterNav() {
    var section = document.querySelector('.markdown-section');
    if (!section) return;
    section.querySelector('.docs-footer-nav')?.remove();

    var links = getSidebarLinks();
    var index = getCurrentLinkIndex(links);
    if (index < 0) return;

    var nav = document.createElement('nav');
    nav.className = 'docs-footer-nav';
    nav.setAttribute('aria-label', 'Page navigation');

    if (index > 0) {
      var prev = links[index - 1];
      var prevLink = document.createElement('a');
      prevLink.className = 'docs-footer-link prev';
      prevLink.href = prev.href;
      prevLink.innerHTML =
        '<span class="docs-footer-label">Previous</span>' +
        '<span class="docs-footer-title"></span>';
      prevLink.querySelector('.docs-footer-title').textContent = prev.text;
      nav.appendChild(prevLink);
    } else {
      nav.appendChild(document.createElement('span'));
    }

    if (index < links.length - 1) {
      var next = links[index + 1];
      var nextLink = document.createElement('a');
      nextLink.className = 'docs-footer-link next';
      nextLink.href = next.href;
      nextLink.innerHTML =
        '<span class="docs-footer-label">Next</span>' +
        '<span class="docs-footer-title"></span>';
      nextLink.querySelector('.docs-footer-title').textContent = next.text;
      nav.appendChild(nextLink);
    }

    if (nav.children.length) section.appendChild(nav);
  }

  window.HBDocsFooterNav = {
    init: addFooterNav,
    refresh: addFooterNav
  };
})();

(function () {
  var chevronSvg =
    '<svg class="nav-megamenu-chevron" viewBox="0 0 12 12" fill="none" ' +
    'stroke="currentColor" stroke-width="1.5" stroke-linecap="round" ' +
    'stroke-linejoin="round" aria-hidden="true"><path d="M4.5 2.5 8 6 4.5 9.5"/></svg>';

  function normalizePath(path) {
    return String(path || '/')
      .split('?')[0]
      .split('#')[0]
      .replace(/\/$/, '') || '/';
  }

  function isMobileNavHomeDuplicate(href, label) {
    var home = document.querySelector('.mobile-nav-home');
    if (!home) return false;
    var homeHref = normalizePath(home.getAttribute('href') || '');
    var homeLabel = (home.textContent || '').trim();
    return normalizePath(href) === homeHref || (!!label && label === homeLabel);
  }

  function closeMobileNav() {
    var drawer = document.getElementById('mobile-nav-drawer');
    var toggle = document.getElementById('mobile-menu-toggle');
    if (!drawer) return;
    drawer.classList.remove('open');
    drawer.setAttribute('aria-hidden', 'true');
    if (toggle) toggle.setAttribute('aria-expanded', 'false');
    document.body.classList.remove('mobile-nav-open');
    var searchInput = document.getElementById('mobile-nav-search-input');
    if (searchInput) {
      searchInput.value = '';
      filterMobileNav('');
    }
  }

  function openMobileNav() {
    var drawer = document.getElementById('mobile-nav-drawer');
    var toggle = document.getElementById('mobile-menu-toggle');
    if (!drawer) return;
    drawer.classList.add('open');
    drawer.setAttribute('aria-hidden', 'false');
    if (toggle) toggle.setAttribute('aria-expanded', 'true');
    document.body.classList.add('mobile-nav-open');
    drawer.querySelectorAll('.mobile-nav-section.active').forEach(function (sectionEl) {
      sectionEl.classList.add('open');
      var trigger = sectionEl.querySelector('.mobile-nav-section-trigger');
      if (trigger) trigger.setAttribute('aria-expanded', 'true');
    });
  }

  function syncLayoutForViewport() {
    if (window.matchMedia('(max-width: 1000px)').matches) {
      document.body.classList.add('close');
    } else {
      document.body.classList.remove('close');
      closeMobileNav();
    }
  }

  function setActiveNav(path) {
    var normalized = normalizePath(path);
    var home = document.querySelector('.mobile-nav-home');
    if (home) {
      home.classList.toggle('active', normalized === '/docs');
    }
    document.querySelectorAll('.mobile-nav-tab').forEach(function (tab) {
      var href = tab.getAttribute('href') || '';
      tab.classList.toggle('active', normalizePath(href) === normalized);
    });
    document.querySelectorAll('.mobile-nav-section').forEach(function (sectionEl) {
      var active = false;
      sectionEl.querySelectorAll('a[href]').forEach(function (link) {
        if (normalizePath(link.getAttribute('href')) === normalized) active = true;
      });
      sectionEl.classList.toggle('active', active);
    });
    document.querySelectorAll('.sidebar-nav a[href]').forEach(function (link) {
      var li = link.parentElement;
      if (!li || li.classList.contains('sidebar-viewing-context')) return;
      li.classList.toggle('active', normalizePath(link.getAttribute('href')) === normalized);
    });
  }

  function textMatchesQuery(text, query) {
    return String(text || '').trim().toLowerCase().indexOf(query) !== -1;
  }

  function filterMobileNav(query) {
    var q = String(query || '').trim().toLowerCase();
    var mobileNavList = document.getElementById('mobile-nav-list');
    var mobileNavTabs = document.getElementById('mobile-nav-tabs');
    if (!mobileNavList) return;

    function setHidden(el, hidden) {
      if (!el) return;
      el.classList.toggle('mobile-nav-filter-hidden', hidden);
    }

    if (mobileNavTabs) {
      mobileNavTabs.querySelectorAll('.mobile-nav-tab').forEach(function (tab) {
        var label = (tab.textContent || '').trim();
        setHidden(tab, q && !textMatchesQuery(label, q));
      });
      setHidden(
        mobileNavTabs.querySelector('.mobile-nav-home'),
        q && !textMatchesQuery((mobileNavTabs.querySelector('.mobile-nav-home')?.textContent || ''), q)
      );
    }

    mobileNavList.querySelectorAll('.mobile-nav-section').forEach(function (sectionEl) {
      var sectionLabel = (
        sectionEl.querySelector('.mobile-nav-section-trigger span')?.textContent || ''
      ).trim();
      var sectionMatches = q && textMatchesQuery(sectionLabel, q);
      var sectionVisible = !q;

      sectionEl.querySelectorAll('.mobile-nav-link').forEach(function (link) {
        var linkMatches = !q || sectionMatches || textMatchesQuery(link.textContent, q);
        var sublinks = link.nextElementSibling;
        var hasVisibleSublink = false;

        if (sublinks && sublinks.classList.contains('mobile-nav-sublinks')) {
          sublinks.querySelectorAll('.mobile-nav-sublink').forEach(function (sublink) {
            var sublinkMatches = !q || sectionMatches || textMatchesQuery(sublink.textContent, q);
            setHidden(sublink, !sublinkMatches);
            if (sublinkMatches) hasVisibleSublink = true;
          });
          setHidden(sublinks, q && !sectionMatches && !hasVisibleSublink && !linkMatches);
        }

        var visible = linkMatches || hasVisibleSublink;
        setHidden(link, q && !visible);
        if (visible) sectionVisible = true;
      });

      setHidden(sectionEl, q && !sectionVisible);

      if (q && sectionVisible) {
        sectionEl.classList.add('open');
        var trigger = sectionEl.querySelector('.mobile-nav-section-trigger');
        if (trigger) trigger.setAttribute('aria-expanded', 'true');
      } else if (!q) {
        var isActive = sectionEl.classList.contains('active');
        sectionEl.classList.toggle('open', isActive);
        var sectionTrigger = sectionEl.querySelector('.mobile-nav-section-trigger');
        if (sectionTrigger) sectionTrigger.setAttribute('aria-expanded', String(isActive));
      }
    });
  }

  function buildMobileNavFromSidebar() {
    var sidebarNav = document.querySelector('.sidebar-nav > ul');
    var mobileNavList = document.getElementById('mobile-nav-list');
    var mobileNavTabs = document.getElementById('mobile-nav-tabs');
    if (!sidebarNav || !mobileNavList || !mobileNavTabs) return;

    mobileNavList.innerHTML = '';
    mobileNavTabs.querySelectorAll('.mobile-nav-tab').forEach(function (el) {
      el.remove();
    });

    Array.prototype.forEach.call(sidebarNav.children, function (li, index) {
      if (li.tagName !== 'LI') return;
      if (li.classList.contains('sidebar-viewing-context')) {
        var deviceLink = li.querySelector('.sidebar-viewing-device');
        if (deviceLink) {
          var deviceHref = deviceLink.getAttribute('href') || '#';
          var deviceLabel = (deviceLink.textContent || '').trim();
          var deviceTab = document.createElement('a');
          deviceTab.className = 'mobile-nav-tab';
          deviceTab.href = deviceHref;
          deviceTab.setAttribute('data-section', 'viewing-context');
          deviceTab.textContent = deviceLabel;
          deviceTab.addEventListener('click', closeMobileNav);
          mobileNavTabs.appendChild(deviceTab);
        }
        return;
      }
      var sectionLabelEl = li.querySelector(':scope > p');
      var directLink = li.querySelector(':scope > a');
      var subUl = li.querySelector(':scope > ul');

      if (sectionLabelEl && subUl) {
        var label = (sectionLabelEl.textContent || '').trim();
        var sectionId = label.toLowerCase().replace(/[^a-z0-9]+/g, '-');
        var firstLink = subUl.querySelector('a[href]');
        var sectionHome = firstLink ? firstLink.getAttribute('href') : '/docs';

        if (!isMobileNavHomeDuplicate(sectionHome, label)) {
          var tab = document.createElement('a');
          tab.className = 'mobile-nav-tab';
          tab.href = sectionHome;
          tab.setAttribute('data-section', sectionId);
          tab.textContent = label;
          tab.addEventListener('click', closeMobileNav);
          mobileNavTabs.appendChild(tab);
        }

        var block = document.createElement('div');
        block.className = 'mobile-nav-section';
        if (li.classList.contains('sidebar-flat-links')) {
          block.classList.add('mobile-nav-flat-links');
        }
        block.setAttribute('data-section', sectionId);

        var trigger = document.createElement('button');
        trigger.type = 'button';
        trigger.className = 'mobile-nav-section-trigger';
        trigger.setAttribute('aria-expanded', 'false');
        trigger.innerHTML = '<span>' + label + '</span>' + chevronSvg;

        var panel = document.createElement('div');
        panel.className = 'mobile-nav-section-panel';

        subUl.querySelectorAll(':scope > li').forEach(function (item, itemIndex) {
          var link = item.querySelector(':scope > a');
          if (!link) return;
          var mobileLink = document.createElement('a');
          mobileLink.className = (itemIndex === 0 || li.classList.contains('sidebar-flat-links'))
            ? 'mobile-nav-link'
            : 'mobile-nav-link mobile-nav-link-nested';
          mobileLink.href = link.getAttribute('href') || '#';
          mobileLink.textContent = (link.textContent || '').trim();
          mobileLink.addEventListener('click', closeMobileNav);
          panel.appendChild(mobileLink);

          var nested = item.querySelector(':scope > ul');
          if (nested) {
            var sublinks = document.createElement('div');
            sublinks.className = 'mobile-nav-sublinks';
            nested.querySelectorAll('a[href]').forEach(function (childLink) {
              var child = document.createElement('a');
              child.className = 'mobile-nav-sublink';
              child.href = childLink.getAttribute('href') || '#';
              child.textContent = (childLink.textContent || '').trim();
              child.addEventListener('click', closeMobileNav);
              sublinks.appendChild(child);
            });
            panel.appendChild(sublinks);
          }
        });

        trigger.addEventListener('click', function () {
          var isOpen = block.classList.contains('open');
          mobileNavList.querySelectorAll('.mobile-nav-section.open').forEach(function (other) {
            if (other !== block) {
              other.classList.remove('open');
              var otherTrigger = other.querySelector('.mobile-nav-section-trigger');
              if (otherTrigger) otherTrigger.setAttribute('aria-expanded', 'false');
            }
          });
          block.classList.toggle('open', !isOpen);
          trigger.setAttribute('aria-expanded', String(!isOpen));
        });

        block.appendChild(trigger);
        block.appendChild(panel);
        mobileNavList.appendChild(block);
        return;
      }

      if (directLink) {
        var linkHref = directLink.getAttribute('href') || '#';
        var linkLabel = (directLink.textContent || '').trim();
        if (!isMobileNavHomeDuplicate(linkHref, linkLabel)) {
          var tabLink = document.createElement('a');
          tabLink.className = 'mobile-nav-tab';
          tabLink.href = linkHref;
          tabLink.setAttribute('data-section', 'link-' + index);
          tabLink.textContent = linkLabel;
          tabLink.addEventListener('click', closeMobileNav);
          mobileNavTabs.appendChild(tabLink);
        }
      }
    });
    var searchInput = document.getElementById('mobile-nav-search-input');
    filterMobileNav(searchInput ? searchInput.value : '');
  }

  function bindMobileNavSearch() {
    var searchInput = document.getElementById('mobile-nav-search-input');
    if (!searchInput || searchInput.__bound) return;
    searchInput.__bound = true;
    searchInput.addEventListener('input', function () {
      filterMobileNav(searchInput.value);
    });
  }

  function bindMobileChrome() {
    var mobileMenuToggle = document.getElementById('mobile-menu-toggle');
    var mobileNavDrawer = document.getElementById('mobile-nav-drawer');
    var navMedia = window.matchMedia('(max-width: 1000px)');

    mobileMenuToggle?.addEventListener('click', function () {
      if (mobileNavDrawer?.classList.contains('open')) closeMobileNav();
      else openMobileNav();
    });

    mobileNavDrawer?.querySelectorAll('[data-mobile-nav-close]').forEach(function (el) {
      el.addEventListener('click', closeMobileNav);
    });

    document.addEventListener('keydown', function (e) {
      if (e.key !== 'Escape') return;
      closeMobileNav();
    });

    navMedia.addEventListener('change', syncLayoutForViewport);
    window.addEventListener('resize', syncLayoutForViewport, { passive: true });
  }

  window.HBDocsMobileNav = {
    init: function () {
      buildMobileNavFromSidebar();
      bindMobileNavSearch();
      bindMobileChrome();
      syncLayoutForViewport();
      setActiveNav(document.body.getAttribute('data-active-path') || window.location.pathname);
      document.querySelector('.mobile-nav-home')?.addEventListener('click', closeMobileNav);
      document.querySelector('.site-header-home')?.addEventListener('click', closeMobileNav);
    }
  };
})();
