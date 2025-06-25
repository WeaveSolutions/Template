job "${job_name}" {
  region      = "global"
  datacenters = ["dc1"]
  type        = "service"
  
  meta {
    %{ for key, value in meta ~}
    ${key} = "${value}"
    %{ endfor ~}
  }
  
  group "web" {
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
        static = ${port}
      }
    }
    
    # Service registration
    service {
      name = "nextjs-app"
      port = "http"
      
      tags = [
        "frontend",
        "nextjs",
        "${environment}",
        "traefik.enable=true",
        "traefik.http.routers.nextjs-${environment}.rule=Host(`${environment}.example.com`)"
      ]
      
      check {
        type     = "http"
        path     = "/api/health"
        interval = "30s"
        timeout  = "10s"
      }
    }
    
    task "nextjs" {
      driver = "docker"
      
      config {
        image = "your-registry/nextjs-app:${image_tag}"
        ports = ["http"]
        
        # Environment-specific configuration
        args = ["pnpm", "start"]
      }
      
      # Environment variables
      env {
        NODE_ENV           = "${environment}"
        PORT               = "${port}"
        NEXT_PUBLIC_API_URL = "${api_url}"
        DATABASE_URL       = "${database_url}"
      }
      
      # Resource limits
      resources {
        cpu    = ${cpu_limit}
        memory = ${memory_limit}
      }
      
      # Health check
      service {
        name = "nextjs-health"
        port = "http"
        
        check {
          type     = "http"
          path     = "/api/health"
          interval = "30s"
          timeout  = "5s"
        }
      }
      
      # Log configuration
      logs {
        max_files     = 3
        max_file_size = 10
      }
      
      # Kill timeout
      kill_timeout = "30s"
    }
  }
}
