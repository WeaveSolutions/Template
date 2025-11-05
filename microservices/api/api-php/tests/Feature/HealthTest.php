<?php

namespace Tests\Feature;

use Tests\TestCase;
use Illuminate\Foundation\Testing\WithFaker;
use Illuminate\Foundation\Testing\RefreshDatabase;

class HealthTest extends TestCase
{
    public function test_health_endpoint(): void
    {
        $response = $this->get('/health');

        $response->assertStatus(200)
                 ->assertJsonStructure([
                     'status',
                     'service',
                     'version',
                     'language',
                     'framework',
                     'port',
                     'timestamp',
                     'uptime',
                     'dependencies' => [
                         'database',
                         'auth0',
                         'mindsdb'
                     ]
                 ]);

        $this->assertEquals('healthy', $response->json('status'));
        $this->assertEquals('nexpo-php-api', $response->json('service'));
        $this->assertEquals('PHP', $response->json('language'));
    }

    public function test_root_endpoint(): void
    {
        $response = $this->get('/');

        $response->assertStatus(200)
                 ->assertJsonStructure([
                     'message',
                     'api',
                     'version',
                     'docs'
                 ]);

        $this->assertEquals('nexpo-php-api', $response->json('api'));
        $this->assertStringContains('Nexpo PHP Laravel API', $response->json('message'));
    }
}
