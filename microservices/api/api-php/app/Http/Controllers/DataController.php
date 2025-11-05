<?php

namespace App\Http\Controllers;

use Illuminate\Http\JsonResponse;
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Http;
use Illuminate\Support\Facades\Log;

class DataController extends Controller
{
    /**
     * @OA\Get(
     *     path="/api/data/query",
     *     operationId="queryMindsDB",
     *     tags={"Data"},
     *     summary="Query MindsDB",
     *     description="Executes a query against MindsDB and returns the results",
     *     security={{"bearerAuth":{}}},
     *     @OA\Parameter(
     *         name="query",
     *         in="query",
     *         required=true,
     *         description="SQL query to execute",
     *         @OA\Schema(type="string")
     *     ),
     *     @OA\Response(
     *         response=200,
     *         description="Query executed successfully",
     *         @OA\JsonContent(
     *             @OA\Property(property="query", type="string"),
     *             @OA\Property(property="results", type="array", @OA\Items(type="object")),
     *             @OA\Property(property="execution_time", type="number"),
     *             @OA\Property(property="timestamp", type="string", format="date-time")
     *         )
     *     ),
     *     @OA\Response(
     *         response=400,
     *         description="Bad request - invalid query"
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
    public function query(Request $request): JsonResponse
    {
        $validated = $request->validate([
            'query' => 'required|string|min:1'
        ]);

        try {
            $user = $request->user();
            if (!$user) {
                return response()->json([
                    'error' => 'unauthorized',
                    'message' => 'User not authenticated'
                ], 401);
            }

            $query = trim($validated['query']);
            $startTime = microtime(true);

            Log::info('MindsDB query initiated', [
                'user_id' => $user['sub'] ?? 'unknown',
                'query' => $query
            ]);

            // Execute query against MindsDB
            $results = $this->executeMindsDBQuery($query);
            
            $executionTime = round((microtime(true) - $startTime) * 1000, 2);

            Log::info('MindsDB query completed', [
                'user_id' => $user['sub'] ?? 'unknown',
                'execution_time_ms' => $executionTime,
                'result_count' => is_array($results) ? count($results) : 0
            ]);

            return response()->json([
                'query' => $query,
                'results' => $results,
                'execution_time' => $executionTime,
                'timestamp' => now()->toISOString()
            ]);

        } catch (\Exception $e) {
            Log::error('MindsDB query error', [
                'user_id' => $user['sub'] ?? 'unknown',
                'query' => $validated['query'] ?? 'unknown',
                'error' => $e->getMessage(),
                'trace' => $e->getTraceAsString()
            ]);

            return response()->json([
                'error' => 'query_failed',
                'message' => 'Failed to execute MindsDB query: ' . $e->getMessage()
            ], 500);
        }
    }

    private function executeMindsDBQuery(string $query): array
    {
        $mindsdbUrl = env('MINDSDB_URL', 'https://cloud.mindsdb.com');
        $username = env('MINDSDB_USERNAME');
        $password = env('MINDSDB_PASSWORD');

        if (!$username || !$password) {
            throw new \Exception('MindsDB credentials not configured');
        }

        // First, authenticate with MindsDB
        $authResponse = Http::post($mindsdbUrl . '/cloud/login', [
            'email' => $username,
            'password' => $password
        ]);

        if (!$authResponse->successful()) {
            throw new \Exception('MindsDB authentication failed');
        }

        $authData = $authResponse->json();
        $sessionCookie = $authResponse->cookies()->toArray()[0] ?? null;

        if (!$sessionCookie) {
            throw new \Exception('Failed to get MindsDB session cookie');
        }

        // Execute the query
        $queryResponse = Http::withCookies([$sessionCookie['Name'] => $sessionCookie['Value']], $sessionCookie['Domain'])
            ->post($mindsdbUrl . '/api/sql/query', [
                'query' => $query,
                'context' => ['db' => 'mindsdb']
            ]);

        if (!$queryResponse->successful()) {
            $errorData = $queryResponse->json();
            throw new \Exception('MindsDB query failed: ' . ($errorData['error'] ?? 'Unknown error'));
        }

        $responseData = $queryResponse->json();
        
        // Handle different response formats
        if (isset($responseData['data'])) {
            return $responseData['data'];
        } elseif (isset($responseData['result'])) {
            return $responseData['result'];
        } else {
            return $responseData;
        }
    }
}
