// Enhanced UI Effects and Animations

// Particle background effect
class ParticleSystem {
    constructor() {
        this.canvas = document.createElement('canvas');
        this.canvas.id = 'particle-canvas';
        this.canvas.style.position = 'fixed';
        this.canvas.style.top = '0';
        this.canvas.style.left = '0';
        this.canvas.style.width = '100%';
        this.canvas.style.height = '100%';
        this.canvas.style.pointerEvents = 'none';
        this.canvas.style.zIndex = '1';
        this.ctx = this.canvas.getContext('2d');
        this.particles = [];
        this.mouse = { x: 0, y: 0 };
        
        document.body.appendChild(this.canvas);
        this.resize();
        this.initParticles();
        this.animate();
        
        window.addEventListener('resize', () => this.resize());
        document.addEventListener('mousemove', (e) => {
            this.mouse.x = e.clientX;
            this.mouse.y = e.clientY;
        });
    }
    
    resize() {
        this.canvas.width = window.innerWidth;
        this.canvas.height = window.innerHeight;
    }
    
    initParticles() {
        const count = Math.min(50, Math.floor((window.innerWidth * window.innerHeight) / 15000));
        for (let i = 0; i < count; i++) {
            this.particles.push({
                x: Math.random() * this.canvas.width,
                y: Math.random() * this.canvas.height,
                radius: Math.random() * 2 + 1,
                vx: (Math.random() - 0.5) * 0.5,
                vy: (Math.random() - 0.5) * 0.5,
                color: `rgba(${217 + Math.random() * 20}, ${166 + Math.random() * 20}, ${159 + Math.random() * 20}, ${0.3 + Math.random() * 0.3})`
            });
        }
    }
    
    animate() {
        this.ctx.clearRect(0, 0, this.canvas.width, this.canvas.height);
        
        this.particles.forEach((particle, i) => {
            particle.x += particle.vx;
            particle.y += particle.vy;
            
            // Bounce off edges
            if (particle.x < 0 || particle.x > this.canvas.width) particle.vx *= -1;
            if (particle.y < 0 || particle.y > this.canvas.height) particle.vy *= -1;
            
            // Mouse interaction
            const dx = this.mouse.x - particle.x;
            const dy = this.mouse.y - particle.y;
            const distance = Math.sqrt(dx * dx + dy * dy);
            
            if (distance < 100) {
                const force = (100 - distance) / 100;
                particle.vx -= (dx / distance) * force * 0.02;
                particle.vy -= (dy / distance) * force * 0.02;
            }
            
            // Draw particle
            this.ctx.beginPath();
            this.ctx.arc(particle.x, particle.y, particle.radius, 0, Math.PI * 2);
            this.ctx.fillStyle = particle.color;
            this.ctx.fill();
            
            // Draw connections
            this.particles.slice(i + 1).forEach(other => {
                const dx = other.x - particle.x;
                const dy = other.y - particle.y;
                const distance = Math.sqrt(dx * dx + dy * dy);
                
                if (distance < 120) {
                    this.ctx.beginPath();
                    this.ctx.moveTo(particle.x, particle.y);
                    this.ctx.lineTo(other.x, other.y);
                    this.ctx.strokeStyle = `rgba(217, 166, 159, ${0.2 * (1 - distance / 120)})`;
                    this.ctx.lineWidth = 1;
                    this.ctx.stroke();
                }
            });
        });
        
        requestAnimationFrame(() => this.animate());
    }
}

// Ripple effect on click
function addRippleEffect(element) {
    element.addEventListener('click', function(e) {
        const ripple = document.createElement('span');
        const rect = this.getBoundingClientRect();
        const size = Math.max(rect.width, rect.height);
        const x = e.clientX - rect.left - size / 2;
        const y = e.clientY - rect.top - size / 2;
        
        ripple.style.width = ripple.style.height = size + 'px';
        ripple.style.left = x + 'px';
        ripple.style.top = y + 'px';
        ripple.classList.add('ripple');
        
        this.appendChild(ripple);
        
        setTimeout(() => ripple.remove(), 600);
    });
}

// Enhanced window animations
function enhanceWindows() {
    const windows = document.querySelectorAll('.window');
    windows.forEach(window => {
        // Add glow effect on hover
        window.addEventListener('mouseenter', function() {
            if (!this.classList.contains('active')) {
                this.style.transition = 'box-shadow 0.3s';
            }
        });
    });
}

// Smooth scroll enhancement
function enhanceScroll() {
    const scrollElements = document.querySelectorAll('.window-content, .start-menu-items');
    scrollElements.forEach(el => {
        el.style.scrollBehavior = 'smooth';
    });
}

// Add keyboard shortcuts
function addKeyboardShortcuts() {
    document.addEventListener('keydown', (e) => {
        // Alt + Tab to switch windows
        if (e.altKey && e.key === 'Tab') {
            e.preventDefault();
            // Window switching logic could go here
        }
        
        // Escape to close start menu
        if (e.key === 'Escape') {
            const startMenu = document.getElementById('start-menu');
            if (startMenu) {
                startMenu.classList.remove('show');
            }
        }
    });
}

// Add loading states
function addLoadingStates() {
    const buttons = document.querySelectorAll('button, .vm-btn, .start-button');
    buttons.forEach(button => {
        button.addEventListener('click', function() {
            if (!this.classList.contains('loading')) {
                this.classList.add('loading');
                setTimeout(() => {
                    this.classList.remove('loading');
                }, 500);
            }
        });
    });
}

// Initialize all enhancements
document.addEventListener('DOMContentLoaded', () => {
    // Initialize particle system
    new ParticleSystem();
    
    // Add ripple effects to buttons
    document.querySelectorAll('button, .desktop-icon, .start-menu-item').forEach(addRippleEffect);
    
    // Enhance windows
    enhanceWindows();
    
    // Enhance scrolling
    enhanceScroll();
    
    // Add keyboard shortcuts
    addKeyboardShortcuts();
    
    // Add loading states
    addLoadingStates();
    
    // Add smooth transitions
    document.body.style.transition = 'all 0.3s ease';
});

