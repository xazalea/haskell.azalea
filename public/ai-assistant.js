// AI Assistant - Controls the OS using OpenAI API
// Provides natural language interface to control the VM and OS

class AIAssistant {
    constructor(vmManager, options = {}) {
        this.vmManager = vmManager;
        this.config = {
            baseURL: options.baseURL || 'https://api.llm7.io/v1',
            apiKey: options.apiKey || 'unused',
            model: options.model || 'gpt-4.1-2025-04-14' // Powerful model
        };
        this.conversationHistory = [];
        this.isActive = false;
    }

    async sendMessage(userMessage) {
        if (!this.isActive) {
            this.isActive = true;
        }

        // Add user message to history
        this.conversationHistory.push({
            role: 'user',
            content: userMessage
        });

        try {
            // Prepare messages with system prompt
            const messages = [
                {
                    role: 'system',
                    content: `You are an AI assistant controlling an advanced virtual machine OS. You can:
- Create and manage processes
- Execute system calls
- Control the VM (step, run, reset, load programs)
- Monitor system state
- Help users interact with the OS

Use the available tools to control the system. Be helpful and efficient.`
                },
                ...this.conversationHistory
            ];

            // Prepare tools for AI
            const tools = this.getAITools();

            // Make API request
            const response = await fetch(`${this.config.baseURL}/chat/completions`, {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json',
                    'Authorization': `Bearer ${this.config.apiKey}`
                },
                body: JSON.stringify({
                    model: this.config.model,
                    messages: messages,
                    tools: tools,
                    tool_choice: 'auto'
                })
            });

            if (!response.ok) {
                throw new Error(`AI API error: ${response.status}`);
            }

            const data = await response.json();
            
            if (data.choices && data.choices.length > 0) {
                const choice = data.choices[0];
                const message = choice.message;

                // Handle tool calls
                if (message.tool_calls && message.tool_calls.length > 0) {
                    const toolResults = await Promise.all(
                        message.tool_calls.map(toolCall => this.executeTool(toolCall))
                    );

                    // Add assistant message and tool results
                    this.conversationHistory.push({
                        role: 'assistant',
                        content: message.content || '',
                        tool_calls: message.tool_calls
                    });

                    // Add tool results
                    toolResults.forEach((result, index) => {
                        this.conversationHistory.push({
                            role: 'tool',
                            tool_call_id: message.tool_calls[index].id,
                            content: result
                        });
                    });

                    // Get final response after tool execution
                    return await this.sendMessage('Tool execution complete. Provide a summary.');
                } else {
                    // Regular text response
                    const assistantMessage = message.content || '';
                    this.conversationHistory.push({
                        role: 'assistant',
                        content: assistantMessage
                    });
                    return assistantMessage;
                }
            }

            return 'No response from AI';
        } catch (error) {
            console.error('AI Assistant error:', error);
            return `Error: ${error.message}`;
        }
    }

    getAITools() {
        return [
            {
                type: 'function',
                function: {
                    name: 'create_process',
                    description: 'Create a new process in the OS',
                    parameters: {
                        type: 'object',
                        properties: {
                            name: {
                                type: 'string',
                                description: 'Process name'
                            }
                        },
                        required: ['name']
                    }
                }
            },
            {
                type: 'function',
                function: {
                    name: 'kill_process',
                    description: 'Kill a process by PID',
                    parameters: {
                        type: 'object',
                        properties: {
                            pid: {
                                type: 'number',
                                description: 'Process ID to kill'
                            }
                        },
                        required: ['pid']
                    }
                }
            },
            {
                type: 'function',
                function: {
                    name: 'get_processes',
                    description: 'Get list of all running processes',
                    parameters: {
                        type: 'object',
                        properties: {},
                        required: []
                    }
                }
            },
            {
                type: 'function',
                function: {
                    name: 'system_call',
                    description: 'Execute a system call',
                    parameters: {
                        type: 'object',
                        properties: {
                            syscall_id: {
                                type: 'number',
                                description: 'System call ID (1=write, 2=read, 3=open, 4=close, 5=fork, 6=exec, 7=exit)'
                            },
                            args: {
                                type: 'array',
                                description: 'System call arguments',
                                items: { type: 'number' }
                            }
                        },
                        required: ['syscall_id', 'args']
                    }
                }
            },
            {
                type: 'function',
                function: {
                    name: 'vm_control',
                    description: 'Control the VM (step, run, reset, load program)',
                    parameters: {
                        type: 'object',
                        properties: {
                            action: {
                                type: 'string',
                                enum: ['step', 'run', 'reset', 'load'],
                                description: 'VM action to perform'
                            },
                            program: {
                                type: 'array',
                                description: 'Program data (for load action)',
                                items: { type: 'number' }
                            }
                        },
                        required: ['action']
                    }
                }
            },
            {
                type: 'function',
                function: {
                    name: 'get_vm_state',
                    description: 'Get current VM state (registers, memory, framebuffer)',
                    parameters: {
                        type: 'object',
                        properties: {},
                        required: []
                    }
                }
            },
            {
                type: 'function',
                function: {
                    name: 'draw_pixel',
                    description: 'Draw a pixel on the framebuffer',
                    parameters: {
                        type: 'object',
                        properties: {
                            x: { type: 'number', description: 'X coordinate' },
                            y: { type: 'number', description: 'Y coordinate' },
                            color: { type: 'number', description: 'Color value (RGB)' }
                        },
                        required: ['x', 'y', 'color']
                    }
                }
            }
        ];
    }

    async executeTool(toolCall) {
        const functionName = toolCall.function.name;
        const args = JSON.parse(toolCall.function.arguments || '{}');

        try {
            switch (functionName) {
                case 'create_process':
                    if (this.vmManager && this.vmManager.unifiedOS) {
                        const pid = this.vmManager.unifiedOS.createProcess(args.name || 'unnamed');
                        return `Process created with PID: ${pid}`;
                    }
                    return 'Error: Unified OS not available';

                case 'kill_process':
                    if (this.vmManager && this.vmManager.unifiedOS) {
                        const success = this.vmManager.unifiedOS.killProcess(args.pid);
                        return success ? 'Process killed successfully' : 'Process not found';
                    }
                    return 'Error: Unified OS not available';

                case 'get_processes':
                    if (this.vmManager && this.vmManager.unifiedOS) {
                        const processes = this.vmManager.unifiedOS.getProcesses();
                        return JSON.stringify(processes);
                    }
                    return 'Error: Unified OS not available';

                case 'system_call':
                    if (this.vmManager && this.vmManager.unifiedOS) {
                        const result = await this.vmManager.unifiedOS.systemCall(
                            args.syscall_id,
                            args.args || []
                        );
                        return `System call executed. Result: ${result}`;
                    }
                    return 'Error: Unified OS not available';

                case 'vm_control':
                    if (this.vmManager) {
                        switch (args.action) {
                            case 'step':
                                await this.vmManager.step();
                                return 'VM stepped successfully';
                            case 'run':
                                await this.vmManager.run();
                                return 'VM ran successfully';
                            case 'reset':
                                this.vmManager.reset();
                                return 'VM reset successfully';
                            case 'load':
                                if (args.program) {
                                    await this.vmManager.loadProgram(args.program);
                                    return 'Program loaded successfully';
                                }
                                return 'Error: No program data provided';
                            default:
                                return 'Error: Unknown action';
                        }
                    }
                    return 'Error: VM Manager not available';

                case 'get_vm_state':
                    if (this.vmManager) {
                        const state = this.vmManager.getState();
                        return JSON.stringify(state);
                    }
                    return 'Error: VM Manager not available';

                case 'draw_pixel':
                    if (this.vmManager && this.vmManager.rustVM) {
                        this.vmManager.rustVM.drawPixel(args.x, args.y, args.color);
                        return 'Pixel drawn successfully';
                    }
                    return 'Error: VM not available';

                default:
                    return `Error: Unknown tool ${functionName}`;
            }
        } catch (error) {
            return `Error executing tool: ${error.message}`;
        }
    }

    clearHistory() {
        this.conversationHistory = [];
    }
}

// Export
if (typeof window !== 'undefined') {
    window.AIAssistant = AIAssistant;
}

