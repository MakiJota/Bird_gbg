// Bird Confetti Animation
function createBirdConfetti() {
    const duration = 5 * 1000;
    const animationEnd = Date.now() + duration;
    const colors = ['#FF6B6B', '#4ECDC4', '#45B7D1', '#FFA07A', '#98D8C8', '#F7DC6F', '#BB8FCE', '#85C1E2', '#F39C12', '#E74C3C'];
    
    // Feather icon SVG (matching memory game icon)
    const birdSvg = `<svg width="40" height="40" viewBox="0 0 24 24" xmlns="http://www.w3.org/2000/svg" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round">
        <path d="M20.24 12.24a6 6 0 0 0-8.49-8.49L5 10.5V19h8.5z"></path>
        <line x1="16" y1="8" x2="2" y2="22"></line>
        <line x1="17.5" y1="15" x2="9" y2="15"></line>
    </svg>`;
    
    const interval = setInterval(function() {
        const timeLeft = animationEnd - Date.now();
        
        if (timeLeft <= 0) {
            return clearInterval(interval);
        }
        
        const particleCount = 8;  // More birds
        
        for (let i = 0; i < particleCount; i++) {
            const bird = document.createElement('div');
            bird.innerHTML = birdSvg;
            bird.style.position = 'fixed';
            bird.style.width = '50px';  // Bigger
            bird.style.height = '50px';
            bird.style.color = colors[Math.floor(Math.random() * colors.length)];
            bird.style.left = Math.random() * 100 + '%';
            bird.style.top = '-50px';
            bird.style.zIndex = '9999';
            bird.style.pointerEvents = 'none';
            bird.style.opacity = '1';  // Less transparent
            
            document.body.appendChild(bird);
            
            const duration = Math.random() * 3 + 2;
            const rotation = Math.random() * 720 - 360;
            const xMovement = (Math.random() - 0.5) * 300;
            
            bird.animate([
                {
                    transform: 'translate(0, 0) rotate(0deg)',
                    opacity: 1
                },
                {
                    transform: `translate(${xMovement}px, ${window.innerHeight + 100}px) rotate(${rotation}deg)`,
                    opacity: 0.3
                }
            ], {
                duration: duration * 1000,
                easing: 'cubic-bezier(0.25, 0.46, 0.45, 0.94)'
            });
            
            setTimeout(() => {
                bird.remove();
            }, duration * 1000);
        }
    }, 100);
}

// Make it available globally
window.createBirdConfetti = createBirdConfetti;
