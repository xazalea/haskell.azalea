// Auto-launch browser - Opens Chrome or Firefox automatically
// User can exit if they want

class AutoLauncher {
    constructor() {
        this.hasLaunched = false;
    }

    async launch() {
        // Only launch once
        if (this.hasLaunched) return;
        this.hasLaunched = true;

        // Check if we're in a browser already
        if (typeof window !== 'undefined' && window.navigator) {
            console.log('Already in browser:', window.navigator.userAgent);
            return; // Already in browser, don't launch
        }

        // Try to detect and launch browser
        // Note: This requires a backend service or native app
        // For web deployment, we'll show instructions instead
        
        // Check if we can detect the OS
        const platform = navigator.platform || '';
        const isWindows = platform.includes('Win');
        const isMac = platform.includes('Mac');
        const isLinux = platform.includes('Linux');

        // Show browser recommendation
        this.showBrowserRecommendation();
    }

    showBrowserRecommendation() {
        // Create a modal for browser recommendation
        const modal = document.createElement('div');
        modal.className = 'browser-launch-modal';
        modal.innerHTML = `
            <div class="browser-launch-content">
                <div class="browser-launch-header">
                    <h2>🚀 Azalea VM</h2>
                    <button class="browser-launch-close">&times;</button>
                </div>
                <div class="browser-launch-body">
                    <p>For the best experience, we recommend using:</p>
                    <div class="browser-options">
                        <a href="#" class="browser-option" data-browser="chrome">
                            <i class="fab fa-chrome"></i>
                            <span>Google Chrome</span>
                        </a>
                        <a href="#" class="browser-option" data-browser="firefox">
                            <i class="fab fa-firefox"></i>
                            <span>Mozilla Firefox</span>
                        </a>
                    </div>
                    <p class="browser-note">You can continue with your current browser, but Chrome or Firefox will provide the best performance.</p>
                </div>
                <div class="browser-launch-footer">
                    <button class="browser-launch-continue">Continue Anyway</button>
                </div>
            </div>
        `;

        document.body.appendChild(modal);

        // Close handlers
        const closeBtn = modal.querySelector('.browser-launch-close');
        const continueBtn = modal.querySelector('.browser-launch-continue');
        const browserOptions = modal.querySelectorAll('.browser-option');

        closeBtn.addEventListener('click', () => this.closeModal(modal));
        continueBtn.addEventListener('click', () => this.closeModal(modal));

        browserOptions.forEach(option => {
            option.addEventListener('click', (e) => {
                e.preventDefault();
                const browser = option.dataset.browser;
                this.openBrowser(browser);
                this.closeModal(modal);
            });
        });

        // Auto-close after 5 seconds
        setTimeout(() => {
            if (modal.parentNode) {
                this.closeModal(modal);
            }
        }, 5000);
    }

    openBrowser(browser) {
        const currentUrl = window.location.href;
        
        if (browser === 'chrome') {
            // Try to open in Chrome
            window.open(`googlechrome://${currentUrl}`, '_blank');
            // Fallback: show instructions
            setTimeout(() => {
                alert('To open in Chrome:\n1. Copy this URL\n2. Open Chrome\n3. Paste the URL\n\nOr install Chrome: https://www.google.com/chrome/');
            }, 1000);
        } else if (browser === 'firefox') {
            // Try to open in Firefox
            window.open(`firefox://${currentUrl}`, '_blank');
            // Fallback: show instructions
            setTimeout(() => {
                alert('To open in Firefox:\n1. Copy this URL\n2. Open Firefox\n3. Paste the URL\n\nOr install Firefox: https://www.mozilla.org/firefox/');
            }, 1000);
        }
    }

    closeModal(modal) {
        modal.style.opacity = '0';
        setTimeout(() => {
            if (modal.parentNode) {
                modal.parentNode.removeChild(modal);
            }
        }, 300);
    }
}

// Auto-launch on page load
if (typeof window !== 'undefined') {
    window.addEventListener('DOMContentLoaded', () => {
        const launcher = new AutoLauncher();
        launcher.launch();
    });
}

