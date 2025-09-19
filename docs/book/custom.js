// HyperBEAM mdBook Enhancement
// Following mdBook best practices: minimal JavaScript additions

document.addEventListener('DOMContentLoaded', function() {
    // Set default theme to Rust if no theme is stored
    setDefaultTheme();

    // Initialize theme change detection
    initThemeDetection();

    // Add copy buttons to code blocks (disabled - using mdbook's built-in)
    // addCopyButtons();

    // Replace edit icon with page copy functionality
    replaceEditWithCopy();
});

function setDefaultTheme() {
    // Check if there's already a stored theme preference
    const storedTheme = localStorage.getItem('mdbook-theme');

    // If no theme is stored, set default to rust
    if (!storedTheme) {
        localStorage.setItem('mdbook-theme', 'rust');
        document.documentElement.className = document.documentElement.className.replace(/\brust\b|\blight\b|\bnavy\b|\bayu\b/g, '').trim();
        document.documentElement.classList.add('rust');
    }
}

function initThemeDetection() {
    // Watch for theme changes via class changes on html element
    const observer = new MutationObserver(function(mutations) {
        mutations.forEach(function(mutation) {
            if (mutation.type === 'attributes' && mutation.attributeName === 'class') {
                handleThemeChange();
            }
        });
    });

    // Start observing
    observer.observe(document.documentElement, {
        attributes: true,
        attributeFilter: ['class']
    });

    // Also listen for storage changes (theme switching)
    window.addEventListener('storage', function(e) {
        if (e.key === 'mdbook-theme') {
            handleThemeChange();
        }
    });

    // Initial setup
    handleThemeChange();
}

function handleThemeChange() {
    // Re-apply theme-aware styling to any dynamic elements
    const copyButtons = document.querySelectorAll('.copy-code-btn');
    copyButtons.forEach(function(btn) {
        // Update button colors to match current theme
        btn.style.background = 'var(--theme-popup-bg, #333)';
        btn.style.color = 'var(--fg, #fff)';
        btn.style.borderColor = 'var(--theme-popup-border, #555)';
    });

    // Log theme change for debugging
    const currentTheme = document.documentElement.className || 'default';
    console.log('HyperBEAM: Theme changed to', currentTheme);
}

function addCopyButtons() {
    const codeBlocks = document.querySelectorAll('pre code');

    codeBlocks.forEach(function(codeBlock) {
        const pre = codeBlock.parentElement;

        // Skip if copy button already exists
        if (pre.querySelector('.copy-code-btn')) return;

        const copyBtn = document.createElement('button');
        copyBtn.className = 'copy-code-btn';
        copyBtn.textContent = '📋';
        copyBtn.setAttribute('aria-label', 'Copy code to clipboard');
        copyBtn.style.cssText = `
            position: absolute;
            top: 8px;
            right: 8px;
            background: var(--theme-popup-bg, #333);
            color: var(--fg, #fff);
            border: 1px solid var(--theme-popup-border, #555);
            padding: 4px 6px;
            border-radius: 4px;
            font-size: 11px;
            cursor: pointer;
            opacity: 0;
            transition: all 0.2s ease;
            z-index: 10;
            line-height: 1;
            min-width: 24px;
            height: 24px;
            display: flex;
            align-items: center;
            justify-content: center;
        `;

        pre.style.position = 'relative';
        pre.appendChild(copyBtn);

        // Show/hide on hover
        pre.addEventListener('mouseenter', function() {
            copyBtn.style.opacity = '0.8';
        });

        pre.addEventListener('mouseleave', function() {
            copyBtn.style.opacity = '0';
        });

        // Hover effect
        copyBtn.addEventListener('mouseenter', function() {
            copyBtn.style.opacity = '1';
            copyBtn.style.background = 'var(--hyperbeam-accent-primary, #00ff94)';
            copyBtn.style.color = '#000';
        });

        copyBtn.addEventListener('mouseleave', function() {
            copyBtn.style.background = 'var(--theme-popup-bg, #333)';
            copyBtn.style.color = 'var(--fg, #fff)';
        });

        // Copy functionality
        copyBtn.addEventListener('click', function() {
            navigator.clipboard.writeText(codeBlock.textContent).then(function() {
                const originalText = copyBtn.textContent;
                copyBtn.textContent = '✓';
                copyBtn.style.background = 'var(--hyperbeam-accent-primary, #00ff94)';
                copyBtn.style.color = '#000';

                setTimeout(function() {
                    copyBtn.textContent = originalText;
                    copyBtn.style.background = 'var(--theme-popup-bg, #333)';
                    copyBtn.style.color = 'var(--fg, #fff)';
                }, 1500);
            }).catch(function() {
                copyBtn.textContent = '✗';
                setTimeout(function() {
                    copyBtn.textContent = '📋';
                }, 1500);
            });
        });
    });
}

function replaceEditWithCopy() {
    // Remove the edit button and replace with copy page button
    const editButton = document.querySelector('a[title="Suggest an edit"], a[href*="edit"]');
    if (editButton) {
        // Create new copy button
        const copyPageBtn = document.createElement('button');
        copyPageBtn.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="9" y="9" width="13" height="13" rx="2" ry="2"></rect><path d="m5 15-1-1v-9a2 2 0 0 1 2-2h9a2 2 0 0 1 2 2v1"></path></svg>`;
        copyPageBtn.title = 'Copy page content for LLM';
        copyPageBtn.setAttribute('aria-label', 'Copy page content for LLM use');
        copyPageBtn.className = editButton.className;
        copyPageBtn.style.cssText = `
            background: none;
            border: none;
            color: inherit;
            font-size: inherit;
            cursor: pointer;
            padding: 8px;
            border-radius: 4px;
            transition: background-color 0.2s ease;
            display: inline-flex;
            align-items: center;
            justify-content: center;
            vertical-align: middle;
        `;

        // Add hover effect
        copyPageBtn.addEventListener('mouseenter', function() {
            copyPageBtn.style.backgroundColor = 'var(--theme-hover, rgba(255,255,255,0.1))';
        });

        copyPageBtn.addEventListener('mouseleave', function() {
            copyPageBtn.style.backgroundColor = 'transparent';
        });

        // Copy functionality
        copyPageBtn.addEventListener('click', function() {
            // Get the current page path and construct the markdown file URL
            const currentPath = window.location.pathname;
            const pathParts = currentPath.split('/');
            const fileName = pathParts[pathParts.length - 1] || pathParts[pathParts.length - 2];

            // Construct the path to the original markdown file
            let markdownPath = '';
            if (fileName && fileName.endsWith('.html')) {
                markdownPath = fileName.replace('.html', '.md');
            } else {
                // Handle index pages or other cases
                markdownPath = 'index.md';
            }

            // Try to fetch the original markdown file
            fetch(`src/${markdownPath}`)
                .then(response => {
                    if (response.ok) {
                        return response.text();
                    }
                    throw new Error('Could not fetch markdown file');
                })
                .then(markdownContent => {
                    navigator.clipboard.writeText(markdownContent).then(function() {
                        const originalContent = copyPageBtn.innerHTML;
                        copyPageBtn.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="20,6 9,17 4,12"></polyline></svg>`;
                        copyPageBtn.style.color = 'var(--hyperbeam-accent-primary, #00ff94)';

                        setTimeout(function() {
                            copyPageBtn.innerHTML = originalContent;
                            copyPageBtn.style.color = 'inherit';
                        }, 2000);
                    }).catch(function() {
                        showCopyError();
                    });
                })
                .catch(function() {
                    showCopyError();
                });

            function showCopyError() {
                const originalContent = copyPageBtn.innerHTML;
                copyPageBtn.innerHTML = `<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="18" y1="6" x2="6" y2="18"></line><line x1="6" y1="6" x2="18" y2="18"></line></svg>`;
                copyPageBtn.style.color = '#ff6b6b';

                setTimeout(function() {
                    copyPageBtn.innerHTML = originalContent;
                    copyPageBtn.style.color = 'inherit';
                }, 2000);
            }
        });

        // Replace the edit button
        editButton.parentNode.replaceChild(copyPageBtn, editButton);
    }
}

