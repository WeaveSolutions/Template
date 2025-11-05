<?php

namespace App\Http\Middleware;

use Closure;
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Http;
use Illuminate\Support\Facades\Cache;
use Illuminate\Support\Facades\Log;
use Firebase\JWT\JWT;
use Firebase\JWT\Key;
use Firebase\JWT\JWK;

class Auth0Middleware
{
    /**
     * Handle an incoming request.
     */
    public function handle(Request $request, Closure $next)
    {
        $token = $request->bearerToken();

        if (!$token) {
            return response()->json([
                'error' => 'missing_token',
                'message' => 'Authorization token is required'
            ], 401);
        }

        try {
            $user = $this->validateToken($token);
            $request->setUserResolver(function () use ($user) {
                return $user;
            });

            return $next($request);

        } catch (\Exception $e) {
            Log::warning('JWT validation failed', [
                'error' => $e->getMessage(),
                'token_preview' => substr($token, 0, 20) . '...'
            ]);

            return response()->json([
                'error' => 'invalid_token',
                'message' => 'Invalid or expired token'
            ], 401);
        }
    }

    private function validateToken(string $token): array
    {
        // Get Auth0 JWKS
        $jwks = $this->getAuth0JWKS();
        
        // Decode JWT header to get key ID
        $header = $this->decodeJWTHeader($token);
        $kid = $header['kid'] ?? null;

        if (!$kid) {
            throw new \Exception('Token missing key ID');
        }

        // Find the appropriate key
        $key = null;
        foreach ($jwks['keys'] as $jwk) {
            if ($jwk['kid'] === $kid) {
                $key = $jwk;
                break;
            }
        }

        if (!$key) {
            throw new \Exception('Token key not found in JWKS');
        }

        // Convert JWK to PEM format
        $publicKey = JWK::parseKey($key);

        // Verify and decode the token
        $decoded = JWT::decode($token, $publicKey);
        $payload = (array) $decoded;

        // Validate audience
        $expectedAudience = env('AUTH0_AUDIENCE');
        if ($expectedAudience && !in_array($expectedAudience, $payload['aud'] ?? [])) {
            throw new \Exception('Invalid audience');
        }

        // Validate issuer
        $expectedIssuer = 'https://' . env('AUTH0_DOMAIN') . '/';
        if (($payload['iss'] ?? '') !== $expectedIssuer) {
            throw new \Exception('Invalid issuer');
        }

        return $payload;
    }

    private function getAuth0JWKS(): array
    {
        $cacheKey = 'auth0_jwks';
        
        return Cache::remember($cacheKey, 3600, function () {
            $response = Http::get('https://' . env('AUTH0_DOMAIN') . '/.well-known/jwks.json');
            
            if (!$response->successful()) {
                throw new \Exception('Failed to fetch JWKS');
            }

            return $response->json();
        });
    }

    private function decodeJWTHeader(string $token): array
    {
        $parts = explode('.', $token);
        
        if (count($parts) !== 3) {
            throw new \Exception('Invalid JWT format');
        }

        $header = json_decode(base64_decode($parts[0]), true);
        
        if (!$header) {
            throw new \Exception('Invalid JWT header');
        }

        return $header;
    }
}
