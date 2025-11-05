# Deploying with HashiCorp Nomad

This guide provides step-by-step instructions for deploying applications using HashiCorp Nomad, a simple and flexible workload orchestrator.

It is reccommended to use Terraform to deploy HashiCorp services such as Nomad.

## ✅ Prerequisites

### Install Nomad

Download from: https://developer.hashicorp.com/nomad/downloads

Or install via package managers:

**macOS (Homebrew):**
```bash
brew install nomad
```

**Linux (Ubuntu/Debian):**
```bash
curl -fsSL https://apt.releases.hashicorp.com/gpg | sudo apt-key add -
sudo apt-add-repository "deb [arch=amd64] https://apt.releases.hashicorp.com $(lsb_release -cs) main"
sudo apt-get update && sudo apt-get install nomad
```

**Windows (Chocolatey):**
```powershell
choco install nomad
```

### Install Consul (Optional for service discovery)

**macOS:**
```bash
brew install consul
```

**Windows:**
```powershell
choco install consul
```

**Linux:**
```bash
sudo apt-get install consul
```

### Create a Nomad Job File (HCL syntax)

This is where you define how to deploy your application.

---

## ⚙️ Step 1: Start Nomad Agent

For development mode (single-node cluster):

```bash
nomad agent -dev
```

This will:
- Start Nomad in single-node mode
- Run both server and client locally
- Store everything in-memory (ephemeral)

**Expected Output:**
```
==> Starting Nomad agent...
==> Nomad agent configuration:
       Bind Addr: '127.0.0.1'
       Client: true
       Datacenter: 'dc1'
       Log Level: 'DEBUG'
       Region: 'global'
       Server: true
       Version: '1.6.1'
```

---

## 📄 Step 2: Create a Job File

Example: `example.nomad`

```hcl
job "web" {
  datacenters = ["dc1"]

  group "web-group" {
    count = 1

    task "web-task" {
      driver = "docker"

      config {
        image = "nginx:latest"
        port_map {
          http = 80
        }
      }

      resources {
        network {
          port "http" {}
        }
      }
    }
  }
}
```

### Advanced Job Example (Node.js App)

```hcl
job "nexpo-api" {
  datacenters = ["dc1"]
  type = "service"

  group "api" {
    count = 2

    network {
      port "http" {
        static = 3000
      }
    }

    task "nexpo-api" {
      driver = "docker"

      config {
        image = "nexpo/api:latest"
        ports = ["http"]
      }

      env {
        NODE_ENV = "production"
        PORT = "3000"
      }

      resources {
        cpu    = 500
        memory = 256
      }

      service {
        name = "nexpo-api"
        port = "http"
        
        check {
          type     = "http"
          path     = "/health"
          interval = "10s"
          timeout  = "3s"
        }
      }
    }
  }
}
```

---

## 🚀 Step 3: Deploy the Job

### Run the Job
```bash
nomad job run example.nomad
```

**Expected Output:**
```
==> Monitoring evaluation "abc123..."
    Evaluation triggered by job "web"
==> Evaluation status changed: "pending" -> "complete"
==> Evaluation "abc123..." finished with status "complete"
```

### Check Job Status
```bash
nomad job status web
```

### View Task Allocations
```bash
nomad alloc status <allocation_id>
```

### View Job Logs
```bash
nomad alloc logs <allocation_id>
```

---

## 🌐 Step 4: Access the Application

If using `-dev` mode:
- Nomad will bind ports randomly on localhost
- Check the port allocation:

```bash
nomad alloc status <allocation_id>
```

### For Explicit Port Control

Update your job file:

```hcl
network {
  port "http" {
    static = 8080
  }
}
```

Then access your application at: `http://localhost:8080`

---

## 🧼 Step 5: Stop / Remove the Job

### Stop the Job
```bash
nomad job stop web
```

### Purge Job Completely
```bash
nomad job stop -purge web
```

### Force Stop (if needed)
```bash
nomad job stop -force web
```

---

## 📦 Advanced: Load Balancing & Scaling

### Horizontal Scaling

Increase the `count` in your job file:

```hcl
group "web-group" {
  count = 3  # Scale to 3 instances
  
  # ... rest of configuration
}
```

### Integrate Consul for Service Discovery

1. **Start Consul:**
   ```bash
   consul agent -dev
   ```

2. **Update Nomad Job:**
   ```hcl
   service {
     name = "web-service"
     port = "http"
     
     check {
       type     = "http"
       path     = "/"
       interval = "10s"
       timeout  = "2s"
     }
   }
   ```

### Use Fabio or Traefik for Dynamic Load Balancing

**Fabio Example:**
```bash
# Install Fabio
go install github.com/fabiolb/fabio@latest

# Run Fabio (connects to Consul automatically)
fabio
```

**Traefik Configuration:**
```hcl
service {
  name = "web-service"
  port = "http"
  
  tags = [
    "traefik.enable=true",
    "traefik.http.routers.web.rule=Host(`web.localhost`)",
  ]
}
```

---

## 🔒 Optional: Running in Production

### Bootstrap a Nomad Cluster with Multiple Servers/Clients

**Server Configuration (`server.hcl`):**
```hcl
data_dir = "/opt/nomad/data"
bind_addr = "0.0.0.0"

server {
  enabled          = true
  bootstrap_expect = 3
}

client {
  enabled = false
}
```

**Client Configuration (`client.hcl`):**
```hcl
data_dir = "/opt/nomad/data"
bind_addr = "0.0.0.0"

server {
  enabled = false
}

client {
  enabled = true
  servers = ["nomad-server-1:4647", "nomad-server-2:4647", "nomad-server-3:4647"]
}
```

### Use systemd or Docker to Manage the Nomad Service

**Create service file (`/etc/systemd/system/nomad.service`):**
```ini
[Unit]
Description=Nomad
Documentation=https://www.nomadproject.io/
Requires=network-online.target
After=network-online.target
ConditionFileNotEmpty=/etc/nomad.d/nomad.hcl

[Service]
Type=notify
User=nomad
Group=nomad
ExecStart=/usr/local/bin/nomad agent -config=/etc/nomad.d/nomad.hcl
ExecReload=/bin/kill -HUP $MAINPID
KillMode=process
Restart=on-failure
LimitNOFILE=65536

[Install]
WantedBy=multi-user.target
```

### Enable TLS, ACLs, and Audit Logging

**TLS Configuration:**
```hcl
tls {
  http = true
  rpc  = true

  ca_file   = "/opt/nomad/tls/ca.crt"
  cert_file = "/opt/nomad/tls/nomad.crt"
  key_file  = "/opt/nomad/tls/nomad.key"

  verify_server_hostname = true
  verify_https_client    = true
}
```

**ACL Configuration:**
```hcl
acl {
  enabled = true
}
```

**Audit Logging:**
```hcl
audit {
  enabled = true
  sink "file" {
    path      = "/opt/nomad/audit.log"
    format    = "json"
    rotate_bytes = 10485760  # 10MB
    rotate_duration = "24h"
  }
}
```

### Connect with Vault for Secrets

**Vault Integration:**
```hcl
vault {
  enabled = true
  address = "https://vault.service.consul:8200"
  token   = "vault-token-here"
}
```

**Using Vault in Job:**
```hcl
task "app" {
  vault {
    policies = ["app-policy"]
  }

  template {
    data = <<EOH
DATABASE_URL="{{ with secret "secret/database" }}{{ .Data.url }}{{ end }}"
EOH
    destination = "secrets/app.env"
    env         = true
  }
}
```

---

## 🔧 Troubleshooting

### Common Issues

**1. Port Already in Use:**
```bash
# Check what's using the port
sudo netstat -tulpn | grep :8080

# Kill the process if needed
sudo kill -9 <PID>
```

**2. Job Fails to Start:**
```bash
# Check allocation events
nomad alloc status <allocation_id>

# Check logs
nomad alloc logs <allocation_id>
```

**3. Node Not Joining Cluster:**
```bash
# Check node status
nomad node status

# Check server members
nomad server members
```

### Useful Commands

```bash
# List all jobs
nomad job status

# Monitor job deployment
nomad deployment status <deployment_id>

# Scale a job
nomad job scale web 5

# Restart a job
nomad job restart web

# Get job specification
nomad job inspect web

# View UI (if enabled)
open http://localhost:4646
```

---

## 📚 Additional Resources

- [What is Nomad](https://developer.hashicorp.com/nomad/docs/what-is-nomad)
- [Nomad Documentation](https://developer.hashicorp.com/nomad/docs)
- [Job Specification](https://developer.hashicorp.com/nomad/docs/job-specification)
- [Nomad Tutorial](https://developer.hashicorp.com/nomad/tutorials)
- [Nomad API Reference](https://developer.hashicorp.com/nomad/api-docs)
- [HashiCorp Learn](https://learn.hashicorp.com/nomad)

---

## 🎯 Next Steps

1. **Practice Deployment**: Try deploying your Nexpo application using the examples above
2. **Set Up Monitoring**: Integrate with Prometheus and Grafana for observability
3. **Configure Networking**: Set up Consul Connect for service mesh capabilities
4. **Implement CI/CD**: Integrate Nomad deployments with your GitHub Actions workflows
5. **Production Hardening**: Implement security best practices, backups, and disaster recovery

This guide provides a solid foundation for deploying applications with HashiCorp Nomad, from simple development setups to production-ready clusters.