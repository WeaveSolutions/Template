<?php

namespace App\Http\Controllers;

use Illuminate\Http\JsonResponse;
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Http;
use Illuminate\Support\Facades\Log;
use Firebase\JWT\JWT;
use Firebase\JWT\Key;

class AuthController extends Controller
{
    /**
     * @OA\Post(
     *     path="/api/auth/token",
     *     operationId="tokenExchange",
     *     tags={"Authentication"},
     *     summary="Exchange authorization code for tokens",
     *     description="Exchanges authorization code for access and refresh tokens",
     *     @OA\RequestBody(
     *         required=true,
     *         @OA\JsonContent(
     *             @OA\Property(property="code", type="string", description="Authorization code"),
     *             @OA\Property(property="redirect_uri", type="string", description="Redirect URI"),
     *             @OA\Property(property="code_verifier", type="string", description="PKCE code verifier")
     *         )
     *     ),
     *     @OA\Response(
     *         response=200,
     *         description="Token exchange successful",
     *         @OA\JsonContent(
     *             @OA\Property(property="access_token", type="string"),
     *             @OA\Property(property="refresh_token", type="string"),
     *             @OA\Property(property="id_token", type="string"),
     *             @OA\Property(property="token_type", type="string"),
     *             @OA\Property(property="expires_in", type="integer")
     *         )
     *     ),
     *     @OA\Response(
     *         response=400,
     *         description="Bad request"
     *     ),
     *     @OA\Response(
     *         response=500,
     *         description="Internal server error"
     *     )
     * )
     */
    public function tokenExchange(Request $request): JsonResponse
    {
        $validated = $request->validate([
            'code' => 'required|string',
            'redirect_uri' => 'required|string|url',
            'code_verifier' => 'nullable|string'
        ]);

        try {
            $tokenData = [
                'grant_type' => 'authorization_code',
                'client_id' => env('AUTH0_CLIENT_ID'),
                'client_secret' => env('AUTH0_CLIENT_SECRET'),
                'code' => $validated['code'],
                'redirect_uri' => $validated['redirect_uri']
            ];

            if (isset($validated['code_verifier'])) {
                $tokenData['code_verifier'] = $validated['code_verifier'];
            }

            $response = Http::asForm()->post(
                'https://' . env('AUTH0_DOMAIN') . '/oauth/token',
                $tokenData
            );

            if (!$response->successful()) {
                Log::error('Auth0 token exchange failed', [
                    'status' => $response->status(),
                    'body' => $response->body()
                ]);
                
                return response()->json([
                    'error' => 'token_exchange_failed',
                    'message' => 'Failed to exchange authorization code for tokens'
                ], $response->status());
            }

            $tokens = $response->json();
            
            Log::info('Token exchange successful', [
                'user_id' => $this->extractUserIdFromToken($tokens['access_token'] ?? null)
            ]);

            return response()->json($tokens);

        } catch (\Exception $e) {
            Log::error('Token exchange error', [
                'error' => $e->getMessage(),
                'trace' => $e->getTraceAsString()
            ]);

            return response()->json([
                'error' => 'internal_error',
                'message' => 'An internal error occurred during token exchange'
            ], 500);
        }
    }

    private function extractUserIdFromToken(?string $token): ?string
    {
        if (!$token) {
            return null;
        }

        try {
            $parts = explode('.', $token);
            if (count($parts) !== 3) {
                return null;
            }

            $payload = json_decode(base64_decode($parts[1]), true);
            return $payload['sub'] ?? null;
        } catch (\Exception $e) {
            return null;
        }
    }
}
