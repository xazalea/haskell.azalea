// Device Detection and Performance Scaling
// Adapts VM performance and UI based on device capabilities

class DeviceDetector {
    constructor() {
        this.deviceInfo = this.detectDevice();
        this.performanceProfile = this.calculatePerformanceProfile();
    }

    detectDevice() {
        const ua = navigator.userAgent;
        const platform = navigator.platform;
        const screen = window.screen;
        const hardwareConcurrency = navigator.hardwareConcurrency || 2;
        const deviceMemory = navigator.deviceMemory || 4; // GB
        const connection = navigator.connection || navigator.mozConnection || navigator.webkitConnection;

        // Detect device type
        const isMobile = /Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(ua);
        const isTablet = /iPad|Android/i.test(ua) && !/Mobile/i.test(ua);
        const isDesktop = !isMobile && !isTablet;
        const isTouch = 'ontouchstart' in window || navigator.maxTouchPoints > 0;

        // Detect OS
        const isIOS = /iPad|iPhone|iPod/.test(ua);
        const isAndroid = /Android/.test(ua);
        const isWindows = /Windows/.test(ua);
        const isMac = /Mac/.test(ua);
        const isLinux = /Linux/.test(ua);

        // Screen info
        const pixelRatio = window.devicePixelRatio || 1;
        const screenWidth = screen.width;
        const screenHeight = screen.height;
        const viewportWidth = window.innerWidth;
        const viewportHeight = window.innerHeight;

        // Network info
        const networkType = connection?.effectiveType || 'unknown';
        const downlink = connection?.downlink || 10; // Mbps

        // GPU detection
        const canvas = document.createElement('canvas');
        const gl = canvas.getContext('webgl') || canvas.getContext('experimental-webgl');
        const gpuInfo = gl ? this.getGPUInfo(gl) : { vendor: 'unknown', renderer: 'unknown' };

        return {
            type: isMobile ? 'mobile' : isTablet ? 'tablet' : 'desktop',
            isMobile,
            isTablet,
            isDesktop,
            isTouch,
            os: isIOS ? 'ios' : isAndroid ? 'android' : isWindows ? 'windows' : isMac ? 'mac' : isLinux ? 'linux' : 'unknown',
            platform,
            hardwareConcurrency,
            deviceMemory,
            pixelRatio,
            screenWidth,
            screenHeight,
            viewportWidth,
            viewportHeight,
            networkType,
            downlink,
            gpuInfo,
            userAgent: ua
        };
    }

    getGPUInfo(gl) {
        const debugInfo = gl.getExtension('WEBGL_debug_renderer_info');
        if (debugInfo) {
            return {
                vendor: gl.getParameter(debugInfo.UNMASKED_VENDOR_WEBGL),
                renderer: gl.getParameter(debugInfo.UNMASKED_RENDERER_WEBGL)
            };
        }
        return { vendor: 'unknown', renderer: 'unknown' };
    }

    calculatePerformanceProfile() {
        const { deviceInfo } = this;
        
        // Base performance score (0-100)
        let score = 50;

        // CPU cores (max 8 cores = +40 points)
        score += Math.min(deviceInfo.hardwareConcurrency * 5, 40);

        // Device memory (max 16GB = +30 points)
        score += Math.min(deviceInfo.deviceMemory * 2, 30);

        // Network speed (max 100Mbps = +20 points)
        score += Math.min(deviceInfo.downlink * 2, 20);

        // Device type bonus
        if (deviceInfo.isDesktop) score += 10;
        else if (deviceInfo.isTablet) score += 5;

        // GPU detection (basic)
        if (deviceInfo.gpuInfo.renderer !== 'unknown') {
            const gpu = deviceInfo.gpuInfo.renderer.toLowerCase();
            if (gpu.includes('nvidia') || gpu.includes('amd') || gpu.includes('radeon')) {
                score += 10; // Dedicated GPU
            } else if (gpu.includes('intel') || gpu.includes('apple')) {
                score += 5; // Integrated GPU
            }
        }

        // Determine performance tier
        let tier;
        let maxResolution;
        let targetFPS;
        let memoryLimit;

        if (score >= 90) {
            tier = 'ultra';
            maxResolution = { width: 2560, height: 1440 };
            targetFPS = 60;
            memoryLimit = 512; // MB
        } else if (score >= 70) {
            tier = 'high';
            maxResolution = { width: 1920, height: 1080 };
            targetFPS = 60;
            memoryLimit = 256; // MB
        } else if (score >= 50) {
            tier = 'medium';
            maxResolution = { width: 1280, height: 720 };
            targetFPS = 45;
            memoryLimit = 128; // MB
        } else {
            tier = 'low';
            maxResolution = { width: 960, height: 540 };
            targetFPS = 30;
            memoryLimit = 64; // MB
        }

        // Adjust for mobile devices
        if (deviceInfo.isMobile) {
            maxResolution.width = Math.min(maxResolution.width, deviceInfo.viewportWidth);
            maxResolution.height = Math.min(maxResolution.height, deviceInfo.viewportHeight);
            targetFPS = Math.min(targetFPS, 30); // Battery optimization
        }

        // Adjust for pixel ratio (retina/high-DPI)
        if (deviceInfo.pixelRatio > 1) {
            // Scale down resolution but maintain quality
            maxResolution.width = Math.floor(maxResolution.width / deviceInfo.pixelRatio);
            maxResolution.height = Math.floor(maxResolution.height / deviceInfo.pixelRatio);
        }

        return {
            score,
            tier,
            maxResolution,
            targetFPS,
            memoryLimit,
            enableAdvancedFeatures: score >= 70,
            enableAnimations: score >= 50,
            enableShadows: score >= 60,
            enableFilters: score >= 80
        };
    }

    getOptimalSettings() {
        return {
            width: this.performanceProfile.maxResolution.width,
            height: this.performanceProfile.maxResolution.height,
            targetFPS: this.performanceProfile.targetFPS,
            pixelRatio: this.deviceInfo.pixelRatio,
            enableTouch: this.deviceInfo.isTouch,
            deviceType: this.deviceInfo.type
        };
    }
}

// Export
if (typeof window !== 'undefined') {
    window.DeviceDetector = DeviceDetector;
}

