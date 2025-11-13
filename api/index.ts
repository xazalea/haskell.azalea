// Minimal API handler for Azalea
// Backend can be extended with Haskell via vercel-hs

export default async function handler(req: any, res: any) {
  try {
    res.setHeader('Access-Control-Allow-Origin', '*');
    res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS');
    res.setHeader('Access-Control-Allow-Headers', 'Content-Type');
    
    if (req.method === 'OPTIONS') {
      return res.status(200).end();
    }
    
    // Health check
    if (req.url === '/api/health' || req.url === '/api/health/') {
      return res.status(200).json({
        success: true,
        message: 'Azalea API',
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
