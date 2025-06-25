variable "aws_region" {
  description = "AWS region to deploy resources"
  type        = string
  default     = "us-east-1"
}

variable "vpc_cidr" {
  description = "CIDR block for VPC"
  type        = string
  default     = "10.0.0.0/16"
}

variable "availability_zones" {
  description = "List of availability zones"
  type        = list(string)
  default     = ["us-east-1a", "us-east-1b"]
}

variable "private_subnet_cidrs" {
  description = "List of private subnet CIDR blocks"
  type        = list(string)
  default     = ["10.0.1.0/24", "10.0.2.0/24"]
}

variable "public_subnet_cidrs" {
  description = "List of public subnet CIDR blocks"
  type        = list(string)
  default     = ["10.0.101.0/24", "10.0.102.0/24"]
}

variable "allowed_origins" {
  description = "List of allowed CORS origins"
  type        = list(string)
  default     = ["*"]
}

variable "enable_waf" {
  description = "Enable AWS WAF for API Gateway"
  type        = bool
  default     = true
}

variable "enable_private_endpoints" {
  description = "Enable private API Gateway endpoints"
  type        = bool
  default     = false
}

variable "api_gateway_log_retention_days" {
  description = "Number of days to retain API Gateway logs"
  type        = number
  default     = 30
}
