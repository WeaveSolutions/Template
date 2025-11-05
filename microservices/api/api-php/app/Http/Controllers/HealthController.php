<?php

namespace App\Http\Controllers;

use Illuminate\Http\JsonResponse;
use Illuminate\Http\Request;

/**
 * @OA\Info(
 *     title="Nexpo PHP Laravel API",
 *     version="1.0.0",
 *     description="PHP Laravel backend for Nexpo platform",
 *     @OA\Contact(
 *         email="dev@nexpo.com"
 *     )
 * )
 * 
 * @OA\Server(
 *     url="http://localhost:7080",
 *     description="Development server"
 * )
 * 
 * @OA\SecurityScheme(
 *     securityScheme="bearerAuth",
 *     type="http",
 *     scheme="bearer",
 *     bearerFormat="JWT"
 * )
 */
class HealthController extends Controller
{
    /**
     * @OA\Get(
     *     path="/health",
     *     operationId="healthCheck",
     *     tags={"Health"},
     *     summary="Health check endpoint",
     *     description="Returns API health status and system information",
     *     @OA\Response(
     *         response=200,
     *         description="API is healthy",
     *         @OA\JsonContent(
     *             @OA\Property(property="status", type="string", example="healthy"),
     *             @OA\Property(property="service", type="string", example="nexpo-php-api"),
     *             @OA\Property(property="version", type="string", example="1.0.0"),
     *             @OA\Property(property="language", type="string", example="PHP"),
     *             @OA\Property(property="framework", type="string", example="Laravel"),
     *             @OA\Property(property="port", type="integer", example=7080),
     *             @OA\Property(property="timestamp", type="string", format="date-time"),
     *             @OA\Property(property="uptime", type="string", example="5 minutes"),
     *             @OA\Property(
     *                 property="dependencies",
     *                 type="object",
     *                 @OA\Property(property="database", type="string", example="connected"),
     *                 @OA\Property(property="auth0", type="string", example="configured"),
     *                 @OA\Property(property="mindsdb", type="string", example="available")
     *             )
     *         )
     *     )
     * )
     */
    public function health(Request $request): JsonResponse
    {
        $startTime = config('app.start_time', microtime(true));
        $uptime = gmdate('H:i:s', time() - (int)$startTime);

        return response()->json([
            'status' => 'healthy',
            'service' => 'nexpo-php-api',
            'version' => '1.0.0',
            'language' => 'PHP',
            'framework' => 'Laravel ' . app()->version(),
            'port' => (int) env('API_PORT', 7080),
            'timestamp' => now()->toISOString(),
            'uptime' => $uptime,
            'dependencies' => [
                'database' => $this->checkDatabase(),
                'auth0' => $this->checkAuth0(),
                'mindsdb' => $this->checkMindsDB()
            ]
        ]);
    }

    /**
     * @OA\Get(
     *     path="/",
     *     operationId="rootEndpoint",
     *     tags={"Health"},
     *     summary="Root endpoint",
     *     description="Returns basic API information",
     *     @OA\Response(
     *         response=200,
     *         description="API information",
     *         @OA\JsonContent(
     *             @OA\Property(property="message", type="string"),
     *             @OA\Property(property="api", type="string"),
     *             @OA\Property(property="version", type="string"),
     *             @OA\Property(property="docs", type="string")
     *         )
     *     )
     * )
     */
    public function index(): JsonResponse
    {
        return response()->json([
            'message' => 'Welcome to Nexpo PHP Laravel API',
            'api' => 'nexpo-php-api',
            'version' => '1.0.0',
            'docs' => url('/api/documentation')
        ]);
    }

    private function checkDatabase(): string
    {
        try {
            \DB::connection()->getPdo();
            return 'connected';
        } catch (\Exception $e) {
            return 'disconnected';
        }
    }

    private function checkAuth0(): string
    {
        $domain = env('AUTH0_DOMAIN');
        $clientId = env('AUTH0_CLIENT_ID');
        
        return ($domain && $clientId) ? 'configured' : 'not configured';
    }

    private function checkMindsDB(): string
    {
        $url = env('MINDSDB_URL');
        
        return $url ? 'available' : 'not configured';
    }
}
