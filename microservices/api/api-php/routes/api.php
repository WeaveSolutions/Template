<?php

use Illuminate\Http\Request;
use Illuminate\Support\Facades\Route;
use App\Http\Controllers\HealthController;
use App\Http\Controllers\AuthController;
use App\Http\Controllers\UserController;
use App\Http\Controllers\DataController;

/*
|--------------------------------------------------------------------------
| API Routes
|--------------------------------------------------------------------------
|
| Here is where you can register API routes for your application. These
| routes are loaded by the RouteServiceProvider and all of them will
| be assigned to the "api" middleware group. Make something great!
|
*/

// Health check routes (no auth required)
Route::get('/', [HealthController::class, 'index']);
Route::get('/health', [HealthController::class, 'health']);

// Authentication routes (no auth required)
Route::prefix('auth')->group(function () {
    Route::post('/token', [AuthController::class, 'tokenExchange']);
});

// Protected routes (require Auth0 JWT)
Route::middleware(['auth0'])->group(function () {
    
    // User management routes
    Route::prefix('user')->group(function () {
        Route::get('/profile', [UserController::class, 'profile']);
        Route::get('/linked-accounts', [UserController::class, 'linkedAccounts']);
    });

    // Data access routes
    Route::prefix('data')->group(function () {
        Route::get('/query', [DataController::class, 'query']);
    });
    
});
