# General Variables
# ========================================
# General Variables
# ========================================

variable "project_name" {
  description = "Name of the project"
  type        = string
}

variable "environment" {
  description = "Environment name (e.g., dev, staging, prod)"
  type        = string
}

variable "allowed_origins" {
  description = "Comma-separated list of allowed origins for CORS"
  type        = string
  default     = "*"
}

# ========================================
# Auth0 Configuration
# ========================================

variable "auth0_domain" {
  description = "Auth0 domain for JWT validation"
  type        = string
  default     = ""
}

variable "auth0_audience" {
  description = "Auth0 audience for JWT validation"
  type        = string
  default     = ""
}

# ========================================
# Provider Toggle Flags
# ========================================

variable "enable_aws" {
  description = "Enable AWS provider and resources"
  type        = bool
  default     = false
}

variable "enable_gcp" {
  description = "Enable GCP provider and resources"
  type        = bool
  default     = false
}

variable "enable_azure" {
  description = "Enable Azure provider and resources"
  type        = bool
  default     = false
}

variable "enable_oci" {
  description = "Enable OCI provider and resources"
  type        = bool
  default     = false
}

variable "enable_ibm" {
  description = "Enable IBM Cloud provider and resources"
  type        = bool
  default     = false
}

# ========================================
# AWS Configuration
# ========================================

variable "aws_region" {
  description = "AWS region to deploy resources"
  type        = string
  default     = "us-east-1"
}

variable "aws_profile" {
  description = "AWS profile to use for authentication"
  type        = string
  default     = "default"
}

variable "aws_assume_role_arn" {
  description = "ARN of the IAM role to assume"
  type        = string
  default     = ""
}

variable "aws_vpc_id" {
  description = "VPC ID where the API Gateway will be deployed"
  type        = string
  default     = ""
}

variable "aws_subnet_ids" {
  description = "List of subnet IDs where the API Gateway will be deployed"
  type        = list(string)
  default     = []
}

variable "aws_security_group_id" {
  description = "The ID of the security group for the API Gateway"
  type        = string
  default     = ""
}

# GCP Configuration
variable "enable_gcp" {
  description = "Enable GCP API Gateway"
  type        = bool
  default     = false
}

variable "gcp_project_id" {
  description = "GCP project ID"
  type        = string
  default     = ""
}

variable "gcp_region" {
  description = "GCP region to deploy resources"
  type        = string
  default     = "us-central1"
}

# Azure Configuration
variable "enable_azure" {
  description = "Enable Azure API Management"
  type        = bool
  default     = false
}

variable "azurerm_location" {
  description = "Azure region to deploy resources"
  type        = string
  default     = "eastus"
}

variable "azurerm_resource_group_name" {
  description = "Name of the Azure resource group"
  type        = string
  default     = ""
}

variable "azurerm_sku_name" {
  description = "The SKU name of the API Management service"
  type        = string
  default     = "Consumption"
}

variable "azurerm_virtual_network_type" {
  description = "Type of virtual network for Azure API Management"
  type        = string
  default     = "None"
}

variable "azurerm_subnet_id" {
  description = "The ID of the subnet for Azure API Management"
  type        = string
  default     = ""
}

# OCI Configuration
variable "enable_oci" {
  description = "Enable OCI API Gateway"
  type        = bool
  default     = false
}

variable "oci_region" {
  description = "OCI region to deploy resources"
  type        = string
  default     = "us-ashburn-1"
}

variable "oci_compartment_ocid" {
  description = "OCID of the OCI compartment"
  type        = string
  default     = ""
}

variable "oci_subnet_ocid" {
  description = "The OCID of the subnet for the API Gateway"
  type        = string
  default     = ""
}

variable "oci_endpoint_type" {
  description = "The endpoint type for the OCI API Gateway"
  type        = string
  default     = "PUBLIC"
}

# ========================================
# IBM Cloud Configuration
# ========================================

variable "ibm_region" {
  description = "IBM Cloud region to deploy resources"
  type        = string
  default     = "us-south"
}

variable "ibmcloud_api_key" {
  description = "IBM Cloud API Key"
  type        = string
  sensitive   = true
  default     = ""
}

variable "ibm_api_client_id" {
  description = "Client ID for IBM API Gateway subscription"
  type        = string
  default     = ""
}

variable "ibm_api_client_secret" {
  description = "Client secret for IBM API Gateway subscription"
  type        = string
  sensitive   = true
  default     = ""
}

variable "ibm_enable_waf" {
  description = "Enable Web Application Firewall for IBM API Gateway"
  type        = bool
  default     = true
}

variable "ibm_api_domain" {
  description = "Domain name for the IBM API Gateway"
  type        = string
  default     = "api.example.com"
}

# PostHog Analytics Configuration
variable "enable_posthog" {
  description = "Enable PostHog analytics integration"
  type        = bool
  default     = false
}

variable "posthog_organization_id" {
  description = "PostHog organization ID"
  type        = string
  default     = ""
}

variable "posthog_host" {
  description = "PostHog host URL (use https://app.posthog.com for cloud)"
  type        = string
  default     = "https://app.posthog.com"
}

variable "posthog_analytics_rollout" {
  description = "Percentage of users to enable analytics for"
  type        = number
  default     = 100
}

variable "posthog_sampling_rate" {
  description = "Sampling rate for analytics events (0.0 to 1.0)"
  type        = number
  default     = 1.0
}

variable "posthog_anonymize_ips" {
  description = "Anonymize IP addresses for GDPR compliance"
  type        = bool
  default     = true
}

variable "posthog_data_retention_days" {
  description = "Number of days to retain analytics data"
  type        = number
  default     = 365
}

variable "posthog_batch_size" {
  description = "Number of events to batch before sending"
  type        = number
  default     = 50
}

variable "posthog_flush_interval_ms" {
  description = "Interval in milliseconds to flush events"
  type        = number
  default     = 5000
}

variable "posthog_enable_session_recording" {
  description = "Enable session recording"
  type        = bool
  default     = false
}

variable "posthog_enable_console_logs" {
  description = "Enable console log capture"
  type        = bool
  default     = true
}

variable "posthog_enable_detailed_logging" {
  description = "Enable detailed API logging for debugging"
  type        = bool
  default     = false
}

variable "posthog_enable_api_analytics" {
  description = "Enable API request/response analytics"
  type        = bool
  default     = true
}

variable "posthog_enable_error_tracking" {
  description = "Enable error tracking and monitoring"
  type        = bool
  default     = true
}

variable "posthog_enable_performance_monitoring" {
  description = "Enable API performance monitoring"
  type        = bool
  default     = true
}

variable "posthog_track_social_login_providers" {
  description = "Track social login provider usage"
  type        = bool
  default     = true
}

variable "posthog_social_login_conversion_tracking" {
  description = "Track conversion rates for social login providers"
  type        = bool
  default     = true
}

variable "posthog_enable_business_metrics" {
  description = "Enable business-specific metric tracking"
  type        = bool
  default     = true
}

variable "posthog_track_subscription_events" {
  description = "Track subscription-related events"
  type        = bool
  default     = true
}

variable "posthog_enable_revenue_tracking" {
  description = "Enable revenue and monetization tracking"
  type        = bool
  default     = false
}

variable "posthog_webhook_endpoint" {
  description = "Webhook endpoint for real-time PostHog events"
  type        = string
  default     = ""
}

variable "posthog_webhook_secret" {
  description = "Secret token for webhook security"
  type        = string
  default     = ""
  sensitive   = true
}

# Monitoring Variables
variable "enable_monitoring" {
  description = "Enable monitoring and alerting"
  type        = bool
  default     = true
}

variable "enable_public_access" {
  description = "Whether to create public subnets and internet gateway"
  type        = bool
  default     = true
}

variable "enable_nat_gateway" {
  description = "Whether to create NAT Gateway for private subnets"
  type        = bool
  default     = true
}

variable "single_nat_gateway" {
  description = "Use a single NAT Gateway for all private subnets"
  type        = bool
  default     = true
}

variable "enable_dns_hostnames" {
  description = "Enable DNS hostnames in the VPC"
  type        = bool
  default     = true
}

variable "enable_dns_support" {
  description = "Enable DNS support in the VPC"
  type        = bool
  default     = true
}

variable "tags" {
  description = "A map of tags to add to all resources"
  type        = map(string)
  default     = {}
}
