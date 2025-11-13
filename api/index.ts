// Vercel serverless function wrapper for Haskell
// This uses vercel-hs to run Haskell code
import { spawn } from 'child_process';
import { readFileSync } from 'fs';
import { join } from 'path';

export default async function handler(req: any, res: any) {
  // For vercel-hs, we need to compile and run the Haskell executable
  // This is a simplified version - vercel-hs would handle the build process
  
  try {
    // Handle CORS
    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, OPTIONS');
    res.setHeader('Access-Control-Allow-Headers', 'Content-Type');
    
    if (req.method === 'OPTIONS') {
      return res.status(200).end();
    }
    
    // Route to Haskell handler
    const { method, url, body } = req;
    const path = url.split('?')[0];
    
    // For now, return a simple response
    // In production, this would spawn the Haskell executable
    if (path === '/api/terminal' || path === '/api/terminal/') {
      return res.status(200).json({
        success: true,
        message: 'Terminal API endpoint',
        data: { output: 'Welcome to Azalea Linux Desktop\n$ ' }
      });
    }
    
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
    
    return res.status(200).json({
      success: true,
      message: 'Azalea Haskell API',
      timestamp: new Date().toISOString()
    });
  } catch (error: any) {
    return res.status(500).json({
      success: false,
      error: error.message
    });
  }
}

