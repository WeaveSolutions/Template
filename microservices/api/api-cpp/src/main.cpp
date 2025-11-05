#include <drogon/drogon.h>
#include <iostream>
#include <memory>
#include <string>
#include <json/json.h>
#include <fstream>
#include <cstdlib>

using namespace drogon;

class HealthController : public HttpController<HealthController>
{
public:
    METHOD_LIST_BEGIN
    METHOD_ADD(HealthController::health, "/health", Get, Options);
    METHOD_ADD(HealthController::healthDetailed, "/health/detailed", Get, Options);
    METHOD_LIST_END

    void health(const HttpRequestPtr& req, std::function<void(const HttpResponsePtr&)>&& callback)
    {
        Json::Value response;
        response["status"] = "healthy";
        response["service"] = "cpp-api";
        response["version"] = "1.0.0";
        response["timestamp"] = std::to_string(std::time(nullptr));
        
        auto resp = HttpResponse::newHttpJsonResponse(response);
        resp->setStatusCode(k200OK);
        resp->addHeader("Content-Type", "application/json");
        resp->addHeader("Cache-Control", "no-cache");
        callback(resp);
    }

    void healthDetailed(const HttpRequestPtr& req, std::function<void(const HttpResponsePtr&)>&& callback)
    {
        Json::Value response;
        response["status"] = "healthy";
        response["service"] = "cpp-api";
        response["version"] = "1.0.0";
        response["timestamp"] = std::to_string(std::time(nullptr));
        response["uptime"] = "running";
        response["database"] = "connected";
        response["redis"] = "connected";
        response["memory_usage"] = "normal";
        response["cpu_usage"] = "normal";
        
        auto resp = HttpResponse::newHttpJsonResponse(response);
        resp->setStatusCode(k200OK);
        resp->addHeader("Content-Type", "application/json");
        resp->addHeader("Cache-Control", "no-cache");
        callback(resp);
    }
};

class UserController : public HttpController<UserController>
{
public:
    METHOD_LIST_BEGIN
    METHOD_ADD(UserController::getUsers, "/api/v1/cpp/users", Get, Options);
    METHOD_ADD(UserController::createUser, "/api/v1/cpp/users", Post, Options);
    METHOD_ADD(UserController::getUser, "/api/v1/cpp/users/{id}", Get, Options);
    METHOD_ADD(UserController::updateUser, "/api/v1/cpp/users/{id}", Put, Options);
    METHOD_ADD(UserController::deleteUser, "/api/v1/cpp/users/{id}", Delete, Options);
    METHOD_LIST_END

    void getUsers(const HttpRequestPtr& req, std::function<void(const HttpResponsePtr&)>&& callback)
    {
        Json::Value response;
        Json::Value users(Json::arrayValue);
        
        // Sample data - in production, this would come from database
        Json::Value user1;
        user1["id"] = 1;
        user1["name"] = "John Doe";
        user1["email"] = "john@example.com";
        user1["created_at"] = "2023-01-01T00:00:00Z";
        users.append(user1);

        Json::Value user2;
        user2["id"] = 2;
        user2["name"] = "Jane Smith";
        user2["email"] = "jane@example.com";
        user2["created_at"] = "2023-01-02T00:00:00Z";
        users.append(user2);

        response["data"] = users;
        response["meta"]["total"] = users.size();
        response["meta"]["page"] = 1;
        response["meta"]["per_page"] = 10;
        
        auto resp = HttpResponse::newHttpJsonResponse(response);
        resp->setStatusCode(k200OK);
        resp->addHeader("Content-Type", "application/json");
        resp->addHeader("X-Total-Count", std::to_string(users.size()));
        callback(resp);
    }

    void createUser(const HttpRequestPtr& req, std::function<void(const HttpResponsePtr&)>&& callback)
    {
        Json::Value requestBody;
        Json::Reader reader;
        
        if (!reader.parse(req->getBody(), requestBody)) {
            Json::Value error;
            error["error"] = "Invalid JSON";
            error["message"] = "Request body must be valid JSON";
            auto resp = HttpResponse::newHttpJsonResponse(error);
            resp->setStatusCode(k400BadRequest);
            callback(resp);
            return;
        }

        // Validate required fields
        if (!requestBody.isMember("name") || !requestBody.isMember("email")) {
            Json::Value error;
            error["error"] = "Missing required fields";
            error["message"] = "Name and email are required";
            auto resp = HttpResponse::newHttpJsonResponse(error);
            resp->setStatusCode(k400BadRequest);
            callback(resp);
            return;
        }

        Json::Value response;
        response["id"] = 123;
        response["name"] = requestBody["name"];
        response["email"] = requestBody["email"];
        response["created_at"] = "2023-12-01T00:00:00Z";
        response["updated_at"] = "2023-12-01T00:00:00Z";
        
        auto resp = HttpResponse::newHttpJsonResponse(response);
        resp->setStatusCode(k201Created);
        resp->addHeader("Content-Type", "application/json");
        resp->addHeader("Location", "/api/v1/cpp/users/123");
        callback(resp);
    }

    void getUser(const HttpRequestPtr& req, std::function<void(const HttpResponsePtr&)>&& callback, int id)
    {
        Json::Value response;
        response["id"] = id;
        response["name"] = "John Doe";
        response["email"] = "john@example.com";
        response["created_at"] = "2023-01-01T00:00:00Z";
        response["updated_at"] = "2023-01-01T00:00:00Z";
        
        auto resp = HttpResponse::newHttpJsonResponse(response);
        resp->setStatusCode(k200OK);
        resp->addHeader("Content-Type", "application/json");
        resp->addHeader("Cache-Control", "public, max-age=300");
        callback(resp);
    }

    void updateUser(const HttpRequestPtr& req, std::function<void(const HttpResponsePtr&)>&& callback, int id)
    {
        Json::Value requestBody;
        Json::Reader reader;
        
        if (!reader.parse(req->getBody(), requestBody)) {
            Json::Value error;
            error["error"] = "Invalid JSON";
            error["message"] = "Request body must be valid JSON";
            auto resp = HttpResponse::newHttpJsonResponse(error);
            resp->setStatusCode(k400BadRequest);
            callback(resp);
            return;
        }

        Json::Value response;
        response["id"] = id;
        response["name"] = requestBody.get("name", "John Doe");
        response["email"] = requestBody.get("email", "john@example.com");
        response["created_at"] = "2023-01-01T00:00:00Z";
        response["updated_at"] = "2023-12-01T00:00:00Z";
        
        auto resp = HttpResponse::newHttpJsonResponse(response);
        resp->setStatusCode(k200OK);
        resp->addHeader("Content-Type", "application/json");
        callback(resp);
    }

    void deleteUser(const HttpRequestPtr& req, std::function<void(const HttpResponsePtr&)>&& callback, int id)
    {
        Json::Value response;
        response["message"] = "User deleted successfully";
        response["id"] = id;
        
        auto resp = HttpResponse::newHttpJsonResponse(response);
        resp->setStatusCode(k200OK);
        resp->addHeader("Content-Type", "application/json");
        callback(resp);
    }
};

class AuthController : public HttpController<AuthController>
{
public:
    METHOD_LIST_BEGIN
    METHOD_ADD(AuthController::login, "/api/v1/cpp/auth/login", Post, Options);
    METHOD_ADD(AuthController::refresh, "/api/v1/cpp/auth/refresh", Post, Options);
    METHOD_ADD(AuthController::logout, "/api/v1/cpp/auth/logout", Post, Options);
    METHOD_LIST_END

    void login(const HttpRequestPtr& req, std::function<void(const HttpResponsePtr&)>&& callback)
    {
        Json::Value requestBody;
        Json::Reader reader;
        
        if (!reader.parse(req->getBody(), requestBody)) {
            Json::Value error;
            error["error"] = "Invalid JSON";
            error["message"] = "Request body must be valid JSON";
            auto resp = HttpResponse::newHttpJsonResponse(error);
            resp->setStatusCode(k400BadRequest);
            callback(resp);
            return;
        }

        // Validate credentials (in production, verify against database)
        if (!requestBody.isMember("email") || !requestBody.isMember("password")) {
            Json::Value error;
            error["error"] = "Missing credentials";
            error["message"] = "Email and password are required";
            auto resp = HttpResponse::newHttpJsonResponse(error);
            resp->setStatusCode(k400BadRequest);
            callback(resp);
            return;
        }

        Json::Value response;
        response["access_token"] = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.sample.token";
        response["refresh_token"] = "refresh_token_example";
        response["expires_in"] = 3600;
        response["token_type"] = "Bearer";
        response["user"] = Json::Value();
        response["user"]["id"] = 1;
        response["user"]["email"] = requestBody["email"];
        response["user"]["name"] = "John Doe";
        
        auto resp = HttpResponse::newHttpJsonResponse(response);
        resp->setStatusCode(k200OK);
        resp->addHeader("Content-Type", "application/json");
        callback(resp);
    }

    void refresh(const HttpRequestPtr& req, std::function<void(const HttpResponsePtr&)>&& callback)
    {
        Json::Value response;
        response["access_token"] = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.refreshed.token";
        response["expires_in"] = 3600;
        response["token_type"] = "Bearer";
        
        auto resp = HttpResponse::newHttpJsonResponse(response);
        resp->setStatusCode(k200OK);
        resp->addHeader("Content-Type", "application/json");
        callback(resp);
    }

    void logout(const HttpRequestPtr& req, std::function<void(const HttpResponsePtr&)>&& callback)
    {
        Json::Value response;
        response["message"] = "Logged out successfully";
        
        auto resp = HttpResponse::newHttpJsonResponse(response);
        resp->setStatusCode(k200OK);
        resp->addHeader("Content-Type", "application/json");
        callback(resp);
    }
};

// Global exception handler
class GlobalExceptionHandler : public ExceptionHandler
{
public:
    void handleException(const HttpRequestPtr& req, std::function<void(const HttpResponsePtr&)>&& callback, const std::exception& ex)
    {
        Json::Value error;
        error["error"] = "Internal Server Error";
        error["message"] = ex.what();
        error["timestamp"] = std::to_string(std::time(nullptr));
        
        auto resp = HttpResponse::newHttpJsonResponse(error);
        resp->setStatusCode(k500InternalServerError);
        resp->addHeader("Content-Type", "application/json");
        callback(resp);
    }
};

// CORS middleware
class CorsMiddleware : public HttpMiddleware<CorsMiddleware>
{
public:
    void invoke(const HttpRequestPtr& req, MiddlewareNextCallback&& nextCb, MiddlewareCallback&& mcb) override
    {
        // Set CORS headers
        if (req->getMethod() == HttpMethod::Options) {
            auto resp = HttpResponse::newHttpResponse();
            resp->setStatusCode(k200OK);
            resp->addHeader("Access-Control-Allow-Origin", "*");
            resp->addHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
            resp->addHeader("Access-Control-Allow-Headers", "Content-Type, Authorization, X-Requested-With");
            resp->addHeader("Access-Control-Max-Age", "86400");
            mcb(resp);
            return;
        }

        // Continue to next middleware
        nextCb([mcb](const HttpResponsePtr& resp) {
            resp->addHeader("Access-Control-Allow-Origin", "*");
            resp->addHeader("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS");
            resp->addHeader("Access-Control-Allow-Headers", "Content-Type, Authorization, X-Requested-With");
            mcb(resp);
        });
    }
};

// Load environment variables
void loadEnvironmentVariables()
{
    const char* port = std::getenv("PORT");
    if (port) {
        std::cout << "Port from environment: " << port << std::endl;
    }
    
    const char* nodeEnv = std::getenv("NODE_ENV");
    if (nodeEnv) {
        std::cout << "Environment: " << nodeEnv << std::endl;
    }
    
    const char* serviceName = std::getenv("SERVICE_NAME");
    if (serviceName) {
        std::cout << "Service name: " << serviceName << std::endl;
    }
}

int main()
{
    // Load environment variables
    loadEnvironmentVariables();
    
    // Load configuration
    app().loadConfigFile("config.json");
    
    // Register controllers
    app().registerController(std::make_shared<HealthController>());
    app().registerController(std::make_shared<UserController>());
    app().registerController(std::make_shared<AuthController>());
    
    // Register middleware
    app().registerMiddleware("cors", std::make_shared<CorsMiddleware>());
    
    // Register exception handler
    app().setExceptionHandler(std::make_shared<GlobalExceptionHandler>());
    
    // Add request logging
    app().setLogPath("./logs");
    app().setLogLevel(trantor::Logger::kInfo);
    
    // Start server
    std::cout << "Starting Nexpo C++ API server..." << std::endl;
    std::cout << "Server will be listening on port 7090" << std::endl;
    std::cout << "Health check endpoint: http://localhost:7090/health" << std::endl;
    std::cout << "API base path: http://localhost:7090/api/v1/cpp" << std::endl;
    
    app().addListener("0.0.0.0", 7090);
    app().run();
    
    return 0;
}
