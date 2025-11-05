<?php

namespace Tests\Feature;

use Tests\TestCase;
use Illuminate\Foundation\Testing\WithFaker;
use Illuminate\Foundation\Testing\RefreshDatabase;

class AuthTest extends TestCase
{
    public function test_token_exchange_requires_authorization_code(): void
    {
        $response = $this->postJson('/api/auth/token', []);

        $response->assertStatus(422)
                 ->assertJsonValidationErrors(['authorization_code']);
    }

    public function test_token_exchange_with_invalid_code(): void
    {
        $response = $this->postJson('/api/auth/token', [
            'authorization_code' => 'invalid_code'
        ]);

        $response->assertStatus(400)
                 ->assertJson([
                     'error' => 'token_exchange_failed'
                 ]);
    }

    public function test_protected_route_requires_auth(): void
    {
        $response = $this->getJson('/api/user/profile');

        $response->assertStatus(401)
                 ->assertJson([
                     'error' => 'missing_token'
                 ]);
    }

    public function test_protected_route_with_invalid_token(): void
    {
        $response = $this->withHeaders([
            'Authorization' => 'Bearer invalid_token'
        ])->getJson('/api/user/profile');

        $response->assertStatus(401)
                 ->assertJson([
                     'error' => 'invalid_token'
                 ]);
    }
}
