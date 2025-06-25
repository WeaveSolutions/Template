// ==============================================================================
// DENO EDGE FUNCTIONS TYPE DECLARATIONS
// ==============================================================================

// Deno global declarations
declare namespace Deno {
  export const env: {
    get(key: string): string | undefined;
  };
}

// Web API types (already available in modern TypeScript/browsers)
declare const Request: {
  prototype: Request;
  new(input: RequestInfo | URL, init?: RequestInit): Request;
};

declare const Response: {
  prototype: Response;
  new(body?: BodyInit | null, init?: ResponseInit): Response;
};

// Crypto API (available in modern browsers and Deno)
declare const crypto: Crypto;

// Module declarations for URL imports
declare module "https://deno.land/std@0.168.0/http/server.ts" {
  export function serve(handler: (request: Request) => Response | Promise<Response>): void;
}

declare module "https://esm.sh/@supabase/supabase-js@2" {
  export function createClient(url: string, key: string, options?: any): any;
}
