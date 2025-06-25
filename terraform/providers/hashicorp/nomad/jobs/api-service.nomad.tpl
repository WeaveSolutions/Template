job "${job_name}" {
  region      = "global"
  datacenters = ["dc1"]
  type        = "service"
  
  meta {
    %{ for key, value in meta ~}
    ${key} = "${value}"
    %{ endfor ~}
  }
  
  group "api" {
    count = ${instance_count}
    
    # Restart policy
    restart {
      attempts = 3
      interval = "5m"
      delay    = "25s"
      mode     = "delay"
    }
    
    # Update strategy
    update {
      max_parallel      = 1
      min_healthy_time  = "30s"
      healthy_deadline  = "5m"
      progress_deadline = "10m"
      auto_revert       = true
      canary            = 0
    }
    
    # Network configuration
    network {
      port "http" {
        to = ${port}
      }
    }
    
    # Service registration
    service {
      name = "api-service"
      port = "http"
      
      tags = [
        "api",
        "backend",
        "${environment}",
        "traefik.enable=true",
        "traefik.http.routers.api-${environment}.rule=Host(`api-${environment}.example.com`)"
      ]
      
      check {
        type     = "http"
        path     = "/health"
        interval = "30s"
        timeout  = "10s"
      }
    }
    
    task "api" {
      driver = "docker"
      
      config {
        image = "your-registry/api-service:${image_tag}"
        ports = ["http"]
        
        # API-specific configuration
        args = ["node", "dist/server.js"]
      }
      
      # Environment variables
      env {
        NODE_ENV     = "${environment}"
        PORT         = "${port}"
        DATABASE_URL = "${database_url}"
        REDIS_URL    = "${redis_url}"
        
        # API-specific variables
        JWT_SECRET           = "your-jwt-secret-from-vault"
        CORS_ORIGIN         = "*"
        API_RATE_LIMIT      = "100"
        LOG_LEVEL           = "${environment == "production" ? "info" : "debug"}"
      }
      
      # Resource limits
      resources {
        cpu    = ${cpu_limit}
        memory = ${memory_limit}
      }
      
      # Health check
      service {
        name = "api-health"
        port = "http"
        
        check {
          type     = "http"
          path     = "/health"
          interval = "30s"
          timeout  = "5s"
        }
      }
      
      # Readiness check
      service {
        name = "api-ready"
        port = "http"
        
        check {
          type     = "http"
          path     = "/ready"
          interval = "10s"
          timeout  = "3s"
        }
      }
      
      # Log configuration
      logs {
        max_files     = 5
        max_file_size = 20
      }
      
      # Kill timeout
      kill_timeout = "30s"
      
      # Volume mounts for temporary files
      volume_mount {
        volume      = "tmp"
        destination = "/tmp"
        read_only   = false
      }
    }
    
    # Temporary volume
    volume "tmp" {
      type   = "host"
      source = "tmp"
    }
  }
}
