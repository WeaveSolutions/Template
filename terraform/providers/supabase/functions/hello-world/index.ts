/// <reference path="../types.d.ts" />

// ==============================================================================
// HELLO WORLD EDGE FUNCTION
// ==============================================================================

import { serve } from "https://deno.land/std@0.168.0/http/server.ts"

interface HelloRequest {
  name?: string
}

serve(async (req: Request): Promise<Response> => {
  const { name }: HelloRequest = await req.json().catch(() => ({}))
  
  const data = {
    message: `Hello ${name || 'World'}!`,
    timestamp: new Date().toISOString(),
    method: req.method,
    url: req.url
  }

  return new Response(
    JSON.stringify(data),
    { 
      headers: { 
        "Content-Type": "application/json",
        "Access-Control-Allow-Origin": "*",
        "Access-Control-Allow-Methods": "POST, GET, OPTIONS",
        "Access-Control-Allow-Headers": "Content-Type, Authorization"
      } 
    },
  )
})
