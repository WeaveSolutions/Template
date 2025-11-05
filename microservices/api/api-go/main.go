package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"

	"github.com/beego/beego/v2/core/logs"
	"github.com/beego/beego/v2/server/web"
	"github.com/beego/beego/v2/server/web/context"
	"github.com/beego/beego/v2/server/web/filter/cors"
	_ "github.com/beego/beego/v2/server/web/swagger"
	"github.com/golang-jwt/jwt/v5"
	"github.com/joho/godotenv"
)

// Load environment variables
func init() {
	if err := godotenv.Load(); err != nil {
		logs.Info("No .env file found")
	}
}

// Models for Swagger documentation

// HealthResponse represents health check response
// @Description Health check response model
type HealthResponse struct {
	Status      string `json:"status" example:"healthy"`
	Service     string `json:"service" example:"nexpo-beego"`
	Timestamp   string `json:"timestamp" example:"2024-01-01T00:00:00Z"`
	Version     string `json:"version" example:"1.0.0"`
	Environment string `json:"environment" example:"development"`
}

// UserProfile represents user profile data
// @Description User profile information from Auth0
type UserProfile struct {
	Sub           string                 `json:"sub" example:"auth0|123456"`
	Email         string                 `json:"email" example:"user@example.com"`
	Name          string                 `json:"name" example:"John Doe"`
	Picture       string                 `json:"picture" example:"https://example.com/avatar.jpg"`
	EmailVerified bool                   `json:"email_verified" example:"true"`
	Metadata      map[string]interface{} `json:"metadata"`
}

// LinkedAccount represents a linked social account
// @Description Linked social account information
type LinkedAccount struct {
	Provider   string `json:"provider" example:"google"`
	UserID     string `json:"user_id" example:"google-oauth2|123456"`
	Connection string `json:"connection" example:"google-oauth2"`
	IsSocial   bool   `json:"is_social" example:"true"`
	LinkedAt   string `json:"linked_at" example:"2024-01-01T00:00:00Z"`
}

// ErrorResponse represents error response
// @Description Error response model
type ErrorResponse struct {
	Error   string                 `json:"error" example:"UNAUTHORIZED"`
	Message string                 `json:"message" example:"Invalid authentication credentials"`
	Details map[string]interface{} `json:"details,omitempty"`
}

// TokenResponse represents token exchange response
// @Description OAuth2 token response
type TokenResponse struct {
	AccessToken string `json:"access_token" example:"eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCJ9..."`
	TokenType   string `json:"token_type" example:"Bearer"`
	ExpiresIn   int    `json:"expires_in" example:"3600"`
	Scope       string `json:"scope,omitempty" example:"read:user"`
}

// BaseController embeds beego base controller
type BaseController struct {
	web.Controller
}

// Prepare runs before request handling
func (c *BaseController) Prepare() {
	// Set JSON response
	c.Ctx.Output.Header("Content-Type", "application/json; charset=utf-8")
}

// HealthController handles health check endpoints
type HealthController struct {
	BaseController
}

// Get handles GET / - Root health check
// @Title Root Health Check
// @Description Get service health status
// @Success 200 {object} HealthResponse
// @router / [get]
func (c *HealthController) Get() {
	response := HealthResponse{
		Status:      "healthy",
		Service:     "nexpo-beego",
		Timestamp:   time.Now().UTC().Format(time.RFC3339),
		Version:     "1.0.0",
		Environment: os.Getenv("NODE_ENV"),
	}
	c.Data["json"] = response
	c.ServeJSON()
}

// Health handles GET /health - Detailed health check
// @Title Detailed Health Check
// @Description Get detailed service health status
// @Success 200 {object} HealthResponse
// @router /health [get]
func (c *HealthController) Health() {
	response := HealthResponse{
		Status:      "healthy",
		Service:     "nexpo-beego",
		Timestamp:   time.Now().UTC().Format(time.RFC3339),
		Version:     "1.0.0",
		Environment: os.Getenv("NODE_ENV"),
	}
	c.Data["json"] = response
	c.ServeJSON()
}

// UserController handles user-related endpoints
type UserController struct {
	BaseController
}

// GetProfile handles GET /api/user/profile
// @Title Get User Profile
// @Description Get current user profile
// @Param Authorization header string true "Bearer token"
// @Success 200 {object} UserProfile
// @Failure 401 {object} ErrorResponse
// @router /api/user/profile [get]
func (c *UserController) GetProfile() {
	// Validate JWT token
	claims, err := validateToken(c.Ctx)
	if err != nil {
		c.Ctx.Output.SetStatus(401)
		c.Data["json"] = ErrorResponse{
			Error:   "UNAUTHORIZED",
			Message: err.Error(),
		}
		c.ServeJSON()
		return
	}

	// Build user profile from claims
	profile := UserProfile{
		Sub:           getStringClaim(claims, "sub"),
		Email:         getStringClaim(claims, "email"),
		Name:          getStringClaim(claims, "name"),
		Picture:       getStringClaim(claims, "picture"),
		EmailVerified: getBoolClaim(claims, "email_verified"),
		Metadata:      make(map[string]interface{}),
	}

	if metadata, ok := claims["metadata"].(map[string]interface{}); ok {
		profile.Metadata = metadata
	}

	c.Data["json"] = profile
	c.ServeJSON()
}

// GetLinkedAccounts handles GET /api/user/linked-accounts
// @Title Get Linked Accounts
// @Description Get user's linked social accounts
// @Param Authorization header string true "Bearer token"
// @Success 200 {array} LinkedAccount
// @Failure 401 {object} ErrorResponse
// @router /api/user/linked-accounts [get]
func (c *UserController) GetLinkedAccounts() {
	// Validate JWT token
	_, err := validateToken(c.Ctx)
	if err != nil {
		c.Ctx.Output.SetStatus(401)
		c.Data["json"] = ErrorResponse{
			Error:   "UNAUTHORIZED",
			Message: err.Error(),
		}
		c.ServeJSON()
		return
	}

	// Mock linked accounts (in production, query Auth0 Management API)
	accounts := []LinkedAccount{
		{
			Provider:   "google",
			UserID:     "google-oauth2|123456789",
			Connection: "google-oauth2",
			IsSocial:   true,
			LinkedAt:   time.Now().UTC().Format(time.RFC3339),
		},
	}

	c.Data["json"] = accounts
	c.ServeJSON()
}

// AuthController handles authentication endpoints
type AuthController struct {
	BaseController
}

// ExchangeToken handles POST /api/auth/token
// @Title Exchange Token
// @Description Exchange credentials for access token
// @Param grant_type formData string true "Grant type" default(client_credentials)
// @Param scope formData string false "Token scope"
// @Success 200 {object} TokenResponse
// @Failure 400 {object} ErrorResponse
// @router /api/auth/token [post]
func (c *AuthController) ExchangeToken() {
	_ = c.GetString("grant_type", "client_credentials") // Unused but required for API compatibility
	scope := c.GetString("scope", "read:user")

	// Mock token response (in production, implement proper OAuth2 flow)
	response := TokenResponse{
		AccessToken: "mock_access_token_" + time.Now().Format("20060102150405"),
		TokenType:   "Bearer",
		ExpiresIn:   3600,
		Scope:       scope,
	}

	c.Data["json"] = response
	c.ServeJSON()
}

// DataController handles data query endpoints
type DataController struct {
	BaseController
}

// QueryMindsDB handles GET /api/data/query
// @Title Query MindsDB
// @Description Execute query through MindsDB gateway
// @Param Authorization header string true "Bearer token"
// @Param query query string true "SQL query"
// @Success 200 {object} map[string]interface{}
// @Failure 401 {object} ErrorResponse
// @Failure 500 {object} ErrorResponse
// @router /api/data/query [get]
func (c *DataController) QueryMindsDB() {
	// Validate JWT token
	claims, err := validateToken(c.Ctx)
	if err != nil {
		c.Ctx.Output.SetStatus(401)
		c.Data["json"] = ErrorResponse{
			Error:   "UNAUTHORIZED",
			Message: err.Error(),
		}
		c.ServeJSON()
		return
	}

	query := c.GetString("query")
	if query == "" {
		c.Ctx.Output.SetStatus(400)
		c.Data["json"] = ErrorResponse{
			Error:   "BAD_REQUEST",
			Message: "Query parameter is required",
		}
		c.ServeJSON()
		return
	}

	// Mock MindsDB response (in production, make actual HTTP request to MindsDB)
	response := map[string]interface{}{
		"query":   query,
		"user_id": getStringClaim(claims, "sub"),
		"results": []map[string]interface{}{
			{"id": 1, "name": "Sample Result"},
		},
		"execution_time": "0.123s",
	}

	c.Data["json"] = response
	c.ServeJSON()
}

// JWT validation helper
func validateToken(ctx *context.Context) (jwt.MapClaims, error) {
	authHeader := ctx.Input.Header("Authorization")
	if authHeader == "" {
		return nil, fmt.Errorf("missing authorization header")
	}

	parts := strings.Split(authHeader, " ")
	if len(parts) != 2 || parts[0] != "Bearer" {
		return nil, fmt.Errorf("invalid authorization header format")
	}

	tokenString := parts[1]

	// In production, validate with Auth0 JWKS
	// For demo, parse without validation
	token, _, err := new(jwt.Parser).ParseUnverified(tokenString, jwt.MapClaims{})
	if err != nil {
		return nil, fmt.Errorf("failed to parse token: %v", err)
	}

	claims, ok := token.Claims.(jwt.MapClaims)
	if !ok {
		return nil, fmt.Errorf("invalid token claims")
	}

	return claims, nil
}

// Helper functions for claims
func getStringClaim(claims jwt.MapClaims, key string) string {
	if val, ok := claims[key].(string); ok {
		return val
	}
	return ""
}

func getBoolClaim(claims jwt.MapClaims, key string) bool {
	if val, ok := claims[key].(bool); ok {
		return val
	}
	return false
}

// @title Nexpo Beego API
// @version 1.0.0
// @description Production-ready Beego backend implementation for Nexpo template

// @contact.name API Support
// @contact.email support@nexpo.dev

// @license.name MIT
// @license.url https://opensource.org/licenses/MIT

// @host localhost:7020
// @BasePath /

// @securityDefinitions.apikey BearerAuth
// @in header
// @name Authorization
// @description Type "Bearer" followed by a space and JWT token.

func main() {
	// Configure Beego
	if os.Getenv("NODE_ENV") == "development" {
		web.BConfig.RunMode = web.DEV
	} else {
		web.BConfig.RunMode = web.PROD
	}

	// Set port
	port := os.Getenv("GO_API_PORT")
	if port == "" {
		port = "7020"
	}
	web.BConfig.Listen.HTTPPort, _ = strconv.Atoi(port)

	// Enable Swagger
	if web.BConfig.RunMode == web.DEV {
		web.BConfig.WebConfig.DirectoryIndex = true
		web.BConfig.WebConfig.StaticDir["/swagger"] = "swagger"
	}

	// Configure CORS
	corsOrigins := os.Getenv("CORS_ORIGINS")
	if corsOrigins == "" {
		corsOrigins = "http://localhost:3000"
	}

	web.InsertFilter("*", web.BeforeRouter, cors.Allow(&cors.Options{
		AllowOrigins:     strings.Split(corsOrigins, ","),
		AllowMethods:     []string{"GET", "POST", "PUT", "DELETE", "OPTIONS"},
		AllowHeaders:     []string{"Origin", "Authorization", "Content-Type", "X-Request-ID"},
		ExposeHeaders:    []string{"Content-Length", "X-Request-ID", "X-Kong-Request-ID"},
		AllowCredentials: true,
	}))

	// Configure logging
	logs.SetLogger(logs.AdapterConsole)
	logs.SetLevel(logs.LevelInfo)

	// Register routes
	web.Router("/", &HealthController{})
	web.Router("/health", &HealthController{}, "get:Health")
	web.Router("/api/user/profile", &UserController{}, "get:GetProfile")
	web.Router("/api/user/linked-accounts", &UserController{}, "get:GetLinkedAccounts")
	web.Router("/api/auth/token", &AuthController{}, "post:ExchangeToken")
	web.Router("/api/data/query", &DataController{}, "get:QueryMindsDB")

	// Start server
	logs.Info("Beego server starting on port %s", port)
	web.Run()
}
