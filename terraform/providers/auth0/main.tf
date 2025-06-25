terraform {
  required_providers {
    auth0 = {
      source  = "auth0/auth0"
      version = "~> 1.0"
    }
  }
}

provider "auth0" {
  domain        = var.auth0_domain
  client_id     = var.auth0_client_id
  client_secret = var.auth0_client_secret
}

# Auth0 Application for Web
resource "auth0_client" "web_app" {
  name                = "${var.app_name}-web"
  description         = "Web Application (Next.js)"
  app_type            = "regular_web"
  callbacks           = ["${var.app_url}/api/auth/callback"]
  allowed_logout_urls = [var.app_url]
  web_origins         = [var.app_url]
  
  jwt_configuration {
    alg = "RS256"
  }
}

# Auth0 Application for Mobile (Expo)
resource "auth0_client" "mobile_app" {
  name                = "${var.app_name}-mobile"
  description         = "Mobile Application (Expo)"
  app_type            = "native"
  callbacks           = ["${var.app_scheme}://${var.auth0_domain}/ios/your.app/callback"]
  allowed_logout_urls = ["${var.app_scheme}://${var.auth0_domain}/ios/your.app/callback"]
  
  jwt_configuration {
    alg = "RS256"
  }
}

# API for backend services
resource "auth0_resource_server" "api" {
  name        = "${var.app_name}-api"
  identifier  = var.api_identifier
  signing_alg = "RS256"
  
  allow_off_access = true
  skip_consent_for_verifiable_first_party_clients = true
  
  token_lifetime = 86400
  
  scopes {
    value       = "read:users"
    description = "Read users"
  }
  
  scopes {
    value       = "update:users"
    description = "Update users"
  }
}

# Connections
resource "auth0_connection" "database" {
  name     = "Username-Password-Authentication"
  strategy = "auth0"
  
  options {
    password_policy = "excellent"
    password_dictionary {
      enable = true
    }
    password_no_personal_info {
      enable = true
    }
    password_history {
      enable = true
      size   = 5
    }
  }
}

# Rules for cloud provider integration
resource "auth0_rule" "add_cloud_provider_metadata" {
  name = "Add Cloud Provider Metadata"
  script = <<EOF
function (user, context, callback) {
  // Add cloud provider metadata to the ID token
  const namespace = 'https://cloud.providers';
  
  // Check for AWS Cognito
  if (context.connection === 'AWS-Cognito') {
    context.idToken[namespace] = {
      provider: 'aws',
      region: context.request.geoip.region_name,
      // Add any AWS specific claims
    };
  }
  
  // Check for GCP Identity Platform
  if (context.connection === 'google-oauth2') {
    context.idToken[namespace] = {
      provider: 'gcp',
      project_id: '${var.gcp_project_id}',
      // Add any GCP specific claims
    };
  }
  
  // Check for Azure AD
  if (context.connection === 'windowslive' || context.connection === 'aad') {
    context.idToken[namespace] = {
      provider: 'azure',
      tenant_id: '${var.azure_tenant_id}',
      // Add any Azure specific claims
    };
  }
  
  // Check for OCI
  if (context.connection === 'oci') {
    context.idToken[namespace] = {
      provider: 'oci',
      tenancy_id: '${var.oci_tenancy_id}',
      // Add any OCI specific claims
    };
  }
  
  callback(null, user, context);
}
EOF
  enabled = true
  order   = 1
}

# Actions for custom authentication flows
resource "auth0_action" "cloud_provider_integration" {
  name    = "cloud-provider-integration"
  runtime = "node16"
  deploy  = true
  
  code = <<-EOT
  exports.onExecutePostLogin = async (event, api) => {
    const namespace = 'https://cloud.providers';
    
    // Example: Add cloud provider specific roles based on user metadata
    if (event.connection === 'Username-Password-Authentication') {
      // Default role for database users
      api.idToken.setCustomClaim("${namespace}/roles", ['user']);
    } else if (event.connection === 'google-oauth2') {
      // GCP specific claims
      api.idToken.setCustomClaim("${namespace}/roles", ['gcp-user']);
      api.idToken.setCustomClaim("${namespace}/gcp_project_id", '${var.gcp_project_id}');
    } else if (event.connection === 'aws-cognito') {
      // AWS specific claims
      api.idToken.setCustomClaim("${namespace}/roles", ['aws-user']);
      api.idToken.setCustomClaim("${namespace}/aws_region", '${var.aws_region}');
    }
  };
  EOT
  
  supported_triggers {
    id      = "post-login"
    version = "v3"
  }
}

# Outputs
output "web_client_id" {
  value = auth0_client.web_app.client_id
}

output "mobile_client_id" {
  value = auth0_client.mobile_app.client_id
}

output "api_identifier" {
  value = auth0_resource_server.api.identifier
}
