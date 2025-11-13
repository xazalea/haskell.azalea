// AI Assistant UI - Stunning interface for AI control
// Crystal clear display with zero lag

class AIAssistantUI {
    constructor(containerId, aiAssistant) {
        this.container = document.getElementById(containerId);
        this.aiAssistant = aiAssistant;
        this.isOpen = false;
        this.init();
    }

    init() {
        this.createUI();
        this.setupEventHandlers();
    }

    createUI() {
        // AI Assistant Button (floating)
        this.button = document.createElement('button');
        this.button.className = 'ai-assistant-btn';
        this.button.innerHTML = '<i class="fas fa-robot"></i>';
        this.button.title = 'AI Assistant';
        document.body.appendChild(this.button);

        // AI Assistant Panel
        this.panel = document.createElement('div');
        this.panel.className = 'ai-assistant-panel';
        this.panel.innerHTML = `
            <div class="ai-panel-header">
                <div class="ai-panel-title">
                    <i class="fas fa-robot"></i>
                    <span>AI Assistant</span>
                </div>
                <button class="ai-panel-close">
                    <i class="fas fa-times"></i>
                </button>
            </div>
            <div class="ai-panel-messages" id="ai-messages"></div>
            <div class="ai-panel-input-container">
                <input type="text" class="ai-panel-input" id="ai-input" placeholder="Ask me to control the OS...">
                <button class="ai-panel-send">
                    <i class="fas fa-paper-plane"></i>
                </button>
            </div>
        `;
        document.body.appendChild(this.panel);

        this.messagesContainer = document.getElementById('ai-messages');
        this.input = document.getElementById('ai-input');
    }

    setupEventHandlers() {
        // Toggle panel
        this.button.addEventListener('click', () => this.toggle());

        // Close button
        this.panel.querySelector('.ai-panel-close').addEventListener('click', () => this.close());

        // Send message
        const sendBtn = this.panel.querySelector('.ai-panel-send');
        sendBtn.addEventListener('click', () => this.sendMessage());

        this.input.addEventListener('keypress', (e) => {
            if (e.key === 'Enter') {
                this.sendMessage();
            }
        });
    }

    toggle() {
        this.isOpen = !this.isOpen;
        if (this.isOpen) {
            this.panel.classList.add('open');
            this.input.focus();
        } else {
            this.panel.classList.remove('open');
        }
    }

    close() {
        this.isOpen = false;
        this.panel.classList.remove('open');
    }

    async sendMessage() {
        const message = this.input.value.trim();
        if (!message) return;

        // Add user message to UI
        this.addMessage('user', message);
        this.input.value = '';

        // Show thinking indicator
        const thinkingId = this.addMessage('assistant', 'Thinking...', true);

        try {
            // Send to AI
            const response = await this.aiAssistant.sendMessage(message);

            // Update thinking message with response
            this.updateMessage(thinkingId, 'assistant', response);
        } catch (error) {
            this.updateMessage(thinkingId, 'assistant', `Error: ${error.message}`);
        }
    }

    addMessage(role, content, isThinking = false) {
        const messageId = `msg-${Date.now()}-${Math.random()}`;
        const messageEl = document.createElement('div');
        messageEl.id = messageId;
        messageEl.className = `ai-message ai-message-${role}`;
        
        if (isThinking) {
            messageEl.classList.add('thinking');
            messageEl.innerHTML = `
                <div class="ai-message-content">
                    ${content}
                    <span class="ai-thinking-dots">
                        <span>.</span><span>.</span><span>.</span>
                    </span>
                </div>
            `;
        } else {
            messageEl.innerHTML = `
                <div class="ai-message-avatar">
                    ${role === 'user' ? '<i class="fas fa-user"></i>' : '<i class="fas fa-robot"></i>'}
                </div>
                <div class="ai-message-content">${this.formatMessage(content)}</div>
            `;
        }

        this.messagesContainer.appendChild(messageEl);
        this.messagesContainer.scrollTop = this.messagesContainer.scrollHeight;

        return messageId;
    }

    updateMessage(messageId, role, content) {
        const messageEl = document.getElementById(messageId);
        if (messageEl) {
            messageEl.classList.remove('thinking');
            messageEl.className = `ai-message ai-message-${role}`;
            messageEl.innerHTML = `
                <div class="ai-message-avatar">
                    ${role === 'user' ? '<i class="fas fa-user"></i>' : '<i class="fas fa-robot"></i>'}
                </div>
                <div class="ai-message-content">${this.formatMessage(content)}</div>
            `;
        }
    }

    formatMessage(content) {
        // Format code blocks, links, etc.
        return content
            .replace(/\n/g, '<br>')
            .replace(/`([^`]+)`/g, '<code>$1</code>')
            .replace(/\*\*([^*]+)\*\*/g, '<strong>$1</strong>');
    }
}

// Export
if (typeof window !== 'undefined') {
    window.AIAssistantUI = AIAssistantUI;
}

