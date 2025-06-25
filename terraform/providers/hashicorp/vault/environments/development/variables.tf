# HashiCorp Vault Development Environment Variables

variable "vault_address" {
  description = "Vault server address"
  type        = string
  default     = "https://vault.example.com:8200"
}

variable "vault_token" {
  description = "Vault authentication token"
  type        = string
  sensitive   = true
}

variable "enable_database_secrets" {
  description = "Enable database secret engine"
  type        = bool
  default     = true
}

variable "enable_jwt_auth" {
  description = "Enable JWT authentication method"
  type        = bool
  default     = true
}

variable "enable_userpass_auth" {
  description = "Enable userpass authentication (development only)"
  type        = bool
  default     = true
}

variable "enable_audit_logging" {
  description = "Enable audit logging to file"
  type        = bool
  default     = false
}

variable "database_connection_url" {
  description = "Database connection URL for dynamic credentials"
  type        = string
  sensitive   = true
  default     = "postgresql://{{username}}:{{password}}@localhost:5432/nextjs_dev?sslmode=disable"
}

variable "database_username" {
  description = "Database username for Vault connection"
  type        = string
  sensitive   = true
  default     = "vault"
}

variable "database_password" {
  description = "Database password for Vault connection"
  type        = string
  sensitive   = true
  default     = "vault-password"
}

variable "database_name" {
  description = "Database name for application"
  type        = string
  default     = "nextjs_dev"
}

variable "jwt_discovery_url" {
  description = "JWT OIDC discovery URL"
  type        = string
  default     = "https://auth.example.com/.well-known/openid_configuration"
}

variable "jwt_issuer" {
  description = "JWT token issuer"
  type        = string
  default     = "https://auth.example.com"
}

variable "jwt_audience" {
  description = "JWT token audience"
  type        = string
  default     = "next-solito-expo"
}

variable "jwt_subject" {
  description = "JWT token subject"
  type        = string
  default     = "app-service"
}

variable "jwt_secret" {
  description = "JWT signing secret"
  type        = string
  sensitive   = true
  default     = "development-jwt-secret-change-in-production"
}

variable "api_key" {
  description = "API key for external services"
  type        = string
  sensitive   = true
  default     = "development-api-key-change-in-production"
}

variable "encryption_key" {
  description = "Application encryption key"
  type        = string
  sensitive   = true
  default     = "development-encryption-key-32-chars"
}

variable "webhook_secret" {
  description = "Webhook validation secret"
  type        = string
  sensitive   = true
  default     = "development-webhook-secret-change-me"
}

variable "developer_password" {
  description = "Password for developer user (development only)"
  type        = string
  sensitive   = true
  default     = "developer-password-123"
}
