<?php

namespace App\Http\Controllers;

use Illuminate\Http\JsonResponse;
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Http;
use Illuminate\Support\Facades\Log;

class UserController extends Controller
{
    /**
     * @OA\Get(
     *     path="/api/user/profile",
     *     operationId="getUserProfile",
     *     tags={"User"},
     *     summary="Get user profile",
     *     description="Retrieves the authenticated user's profile information",
     *     security={{"bearerAuth":{}}},
     *     @OA\Response(
     *         response=200,
     *         description="User profile retrieved successfully",
     *         @OA\JsonContent(
     *             @OA\Property(property="user_id", type="string"),
     *             @OA\Property(property="email", type="string"),
     *             @OA\Property(property="name", type="string"),
     *             @OA\Property(property="nickname", type="string"),
     *             @OA\Property(property="picture", type="string"),
     *             @OA\Property(property="email_verified", type="boolean"),
     *             @OA\Property(property="created_at", type="string", format="date-time"),
     *             @OA\Property(property="updated_at", type="string", format="date-time")
     *         )
     *     ),
     *     @OA\Response(
     *         response=401,
     *         description="Unauthorized"
     *     ),
     *     @OA\Response(
     *         response=500,
     *         description="Internal server error"
     *     )
     * )
     */
    public function profile(Request $request): JsonResponse
    {
        try {
            $user = $request->user();
            
            if (!$user) {
                return response()->json([
                    'error' => 'unauthorized',
                    'message' => 'User not authenticated'
                ], 401);
            }

            // Get additional user info from Auth0
            $accessToken = $request->bearerToken();
            $userInfo = $this->getUserInfoFromAuth0($accessToken);

            $profile = [
                'user_id' => $user['sub'] ?? 'unknown',
                'email' => $userInfo['email'] ?? $user['email'] ?? null,
                'name' => $userInfo['name'] ?? $user['name'] ?? null,
                'nickname' => $userInfo['nickname'] ?? $user['nickname'] ?? null,
                'picture' => $userInfo['picture'] ?? $user['picture'] ?? null,
                'email_verified' => $userInfo['email_verified'] ?? $user['email_verified'] ?? false,
                'created_at' => $userInfo['created_at'] ?? null,
                'updated_at' => $userInfo['updated_at'] ?? null
            ];

            Log::info('User profile retrieved', [
                'user_id' => $profile['user_id']
            ]);

            return response()->json($profile);

        } catch (\Exception $e) {
            Log::error('Error retrieving user profile', [
                'error' => $e->getMessage(),
                'trace' => $e->getTraceAsString()
            ]);

            return response()->json([
                'error' => 'internal_error',
                'message' => 'Failed to retrieve user profile'
            ], 500);
        }
    }

    /**
     * @OA\Get(
     *     path="/api/user/linked-accounts",
     *     operationId="getUserLinkedAccounts",
     *     tags={"User"},
     *     summary="Get user linked accounts",
     *     description="Retrieves the authenticated user's linked social accounts",
     *     security={{"bearerAuth":{}}},
     *     @OA\Response(
     *         response=200,
     *         description="Linked accounts retrieved successfully",
     *         @OA\JsonContent(
     *             @OA\Property(
     *                 property="linked_accounts",
     *                 type="array",
     *                 @OA\Items(
     *                     @OA\Property(property="connection", type="string"),
     *                     @OA\Property(property="user_id", type="string"),
     *                     @OA\Property(property="provider", type="string"),
     *                     @OA\Property(property="is_social", type="boolean")
     *                 )
     *             )
     *         )
     *     ),
     *     @OA\Response(
     *         response=401,
     *         description="Unauthorized"
     *     ),
     *     @OA\Response(
     *         response=500,
     *         description="Internal server error"
     *     )
     * )
     */
    public function linkedAccounts(Request $request): JsonResponse
    {
        try {
            $user = $request->user();
            
            if (!$user) {
                return response()->json([
                    'error' => 'unauthorized',
                    'message' => 'User not authenticated'
                ], 401);
            }

            $userId = $user['sub'] ?? null;
            if (!$userId) {
                return response()->json([
                    'error' => 'invalid_user',
                    'message' => 'User ID not found'
                ], 400);
            }

            // Get management API token
            $managementToken = $this->getAuth0ManagementToken();
            if (!$managementToken) {
                return response()->json([
                    'error' => 'auth0_error',
                    'message' => 'Failed to get Auth0 management token'
                ], 500);
            }

            // Get user identities from Auth0 Management API
            $response = Http::withToken($managementToken)
                ->get('https://' . env('AUTH0_DOMAIN') . '/api/v2/users/' . $userId);

            if (!$response->successful()) {
                Log::error('Failed to fetch user identities', [
                    'user_id' => $userId,
                    'status' => $response->status()
                ]);

                return response()->json([
                    'error' => 'auth0_error',
                    'message' => 'Failed to fetch user identities'
                ], $response->status());
            }

            $userData = $response->json();
            $identities = $userData['identities'] ?? [];

            $linkedAccounts = array_map(function ($identity) {
                return [
                    'connection' => $identity['connection'] ?? 'unknown',
                    'user_id' => $identity['user_id'] ?? 'unknown',
                    'provider' => $identity['provider'] ?? 'unknown',
                    'is_social' => $identity['isSocial'] ?? false
                ];
            }, $identities);

            return response()->json([
                'linked_accounts' => $linkedAccounts
            ]);

        } catch (\Exception $e) {
            Log::error('Error retrieving linked accounts', [
                'error' => $e->getMessage(),
                'trace' => $e->getTraceAsString()
            ]);

            return response()->json([
                'error' => 'internal_error',
                'message' => 'Failed to retrieve linked accounts'
            ], 500);
        }
    }

    private function getUserInfoFromAuth0(string $accessToken): array
    {
        try {
            $response = Http::withToken($accessToken)
                ->get('https://' . env('AUTH0_DOMAIN') . '/userinfo');

            if ($response->successful()) {
                return $response->json();
            }
        } catch (\Exception $e) {
            Log::warning('Failed to get user info from Auth0', [
                'error' => $e->getMessage()
            ]);
        }

        return [];
    }

    private function getAuth0ManagementToken(): ?string
    {
        try {
            $response = Http::asForm()->post(
                'https://' . env('AUTH0_DOMAIN') . '/oauth/token',
                [
                    'grant_type' => 'client_credentials',
                    'client_id' => env('AUTH0_CLIENT_ID'),
                    'client_secret' => env('AUTH0_CLIENT_SECRET'),
                    'audience' => 'https://' . env('AUTH0_DOMAIN') . '/api/v2/'
                ]
            );

            if ($response->successful()) {
                $data = $response->json();
                return $data['access_token'] ?? null;
            }
        } catch (\Exception $e) {
            Log::error('Failed to get Auth0 management token', [
                'error' => $e->getMessage()
            ]);
        }

        return null;
    }
}
