// Vercel serverless function wrapper
// Note: Full Haskell VM requires custom build setup
// This provides API endpoints for the frontend

export default async function handler(req: any, res: any) {
  try {
    // Handle CORS
    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
    res.setHeader('Access-Control-Allow-Headers', 'Content-Type');
    
    if (req.method === 'OPTIONS') {
      return res.status(200).end();
    }
    
    const path = req.url?.split('?')[0] || '';
    
    // Terminal endpoint
    if (path === '/api/terminal' || path === '/api/terminal/') {
      if (req.method === 'POST') {
        const body = await getBody(req);
        return res.status(200).json({
          success: true,
          output: `Command executed: ${body || 'echo'}\n$ `
        });
      }
      return res.status(200).json({
        success: true,
        output: 'Welcome to Azalea Linux Desktop\n$ '
      });
    }
    
    // Files endpoint
    if (path === '/api/files' || path === '/api/files/') {
      return res.status(200).json({
        success: true,
        files: [
          { name: 'home', type: 'directory' },
          { name: 'Documents', type: 'directory' },
          { name: 'Downloads', type: 'directory' },
          { name: 'Desktop', type: 'directory' }
        ]
      });
    }
    
    // VM endpoints
    if (path === '/api/vm/state' || path === '/api/vm/state/') {
      return res.status(200).json({
        success: true,
        state: {
          registers: new Array(16).fill(0),
          pc: 0,
          sp: 0xFFFF00,
          flags: 0,
          running: false,
          framebuffer: new Array(800 * 600).fill(0x00000000),
          width: 800,
          height: 600
        }
      });
    }
    
    if (path === '/api/vm/step' || path === '/api/vm/step/') {
      return res.status(200).json({
        success: true,
        state: {
          registers: new Array(16).fill(0),
          pc: 1,
          sp: 0xFFFF00,
          flags: 0,
          running: true,
          framebuffer: new Array(800 * 600).fill(0x00000000),
          width: 800,
          height: 600
        }
      });
    }
    
    if (path === '/api/vm/run' || path === '/api/vm/run/') {
      return res.status(200).json({
        success: true,
        state: {
          registers: new Array(16).fill(0),
          pc: 10,
          sp: 0xFFFF00,
          flags: 0,
          running: false,
          framebuffer: new Array(800 * 600).fill(0x00000000),
          width: 800,
          height: 600
        }
      });
    }
    
    if (path === '/api/vm/load' || path === '/api/vm/load/') {
      return res.status(200).json({
        success: true,
        state: {
          registers: new Array(16).fill(0),
          pc: 0,
          sp: 0xFFFF00,
          flags: 0,
          running: false,
          framebuffer: new Array(800 * 600).fill(0x00000000),
          width: 800,
          height: 600
        }
      });
    }
    
    // Health check
    if (path === '/api/health' || path === '/api/health/') {
      return res.status(200).json({
        success: true,
        message: 'Azalea Haskell API is running',
        timestamp: new Date().toISOString()
      });
    }
    
    return res.status(404).json({
      success: false,
      error: 'Not found'
    });
  } catch (error: any) {
    return res.status(500).json({
      success: false,
      error: error.message
    });
  }
}

async function getBody(req: any): Promise<string> {
  return new Promise((resolve) => {
    let body = '';
    req.on('data', (chunk: any) => {
      body += chunk.toString();
    });
    req.on('end', () => {
      try {
        const parsed = JSON.parse(body);
        resolve(parsed.command || parsed.body || '');
      } catch {
        resolve(body);
      }
    });
  });
}

