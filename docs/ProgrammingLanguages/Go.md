Go [https://golang.org/]

Beego [https://beego.me/]
QUIC [https://sourcegraph.com/github.com/quic-go/quic-go]

# Go Installation Guide (Latest Version)

## Overview

- Official Go site: [https://golang.org/dl/]
- Go is a statically typed, compiled programming language designed for building simple, reliable, and efficient software.
- Current stable version (as of July 2025): Go 1.22.x
- Built-in package manager: **go mod** (no external tools needed)
- Excellent for APIs, microservices, and concurrent applications

---

## Windows Installation

### Step 1: Download Go

- Visit: [https://golang.org/dl/]
- Download the latest Windows installer (e.g., `go1.22.x.windows-amd64.msi`)
- Run the installer and follow the setup wizard
- **Important**: The installer automatically adds Go to your PATH

### Step 2: Verify Installation

```powershell
go version
go env GOPATH
go env GOROOT
```

### Step 3: Create Go Workspace

```powershell
mkdir %USERPROFILE%\go
cd %USERPROFILE%\go
mkdir src bin pkg
```

### Step 4: Create First Go Project

```powershell
mkdir my-go-project
cd my-go-project
go mod init my-go-project
```

---

## macOS Installation

### Step 1: Install Go via Homebrew (Recommended)

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install Go
brew install go
```

### Alternative: Manual Installation

```bash
# Download and install manually
wget https://golang.org/dl/go1.22.x.darwin-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.22.x.darwin-amd64.tar.gz
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bash_profile
source ~/.bash_profile
```

### Step 2: Verify Installation

```bash
go version
go env GOPATH
go env GOROOT
```

### Step 3: Create Go Workspace

```bash
mkdir -p $HOME/go/{src,bin,pkg}
```

### Step 4: Create First Go Project

```bash
mkdir my-go-project
cd my-go-project
go mod init my-go-project
```

---

## Ubuntu / Debian Linux Installation

### Step 1: Remove Old Go Version (if exists)

```bash
sudo rm -rf /usr/local/go
```

### Step 2: Download and Install Go

```bash
wget https://golang.org/dl/go1.22.x.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.22.x.linux-amd64.tar.gz
```

### Step 3: Add Go to PATH

```bash
echo 'export PATH=$PATH:/usr/local/go/bin' >> ~/.bashrc
source ~/.bashrc
```

### Step 4: Verify Installation

```bash
go version
go env GOPATH
go env GOROOT
```

### Step 5: Create Go Workspace

```bash
mkdir -p $HOME/go/{src,bin,pkg}
```

---

## Quick Start: Beego Web Framework

### Step 1: Install Beego and Bee Tool

```bash
go install github.com/beego/beego/v2@latest
go install github.com/beego/bee/v2@latest
```

### Step 2: Create Beego Project

```bash
bee new my-beego-api
cd my-beego-api
```

### Step 3: Run Development Server

```bash
bee run
```

### Step 4: Create Simple API Controller

Create `controllers/api.go`:
```go
package controllers

import (
    "github.com/beego/beego/v2/server/web"
)

type APIController struct {
    web.Controller
}

func (c *APIController) Get() {
    c.Data["json"] = map[string]interface{}{
        "message": "Hello from Go Beego API!",
        "status":  "success",
        "port":    8040,
    }
    c.ServeJSON()
}

func (c *APIController) GetUser() {
    userID := c.Ctx.Input.Param(":id")
    c.Data["json"] = map[string]interface{}{
        "user_id": userID,
        "name":    "User " + userID,
        "platform": "Go",
    }
    c.ServeJSON()
}
```

### Step 5: Add Routes

Update `routers/router.go`:
```go
package routers

import (
    "my-beego-api/controllers"
    "github.com/beego/beego/v2/server/web"
)

func init() {
    web.Router("/", &controllers.MainController{})
    web.Router("/api", &controllers.APIController{})
    web.Router("/api/users/:id", &controllers.APIController{}, "get:GetUser")
}
```

Visit `http://localhost:8080/api` to see your Go Beego API.

---

## Summary Table

| Platform        | Install Method          | Go Command | Module Init         | Framework          |
|-----------------|-------------------------|------------|--------------------|--------------------||
| Windows         | Official MSI installer  | `go`       | `go mod init`      | `go install beego` |
| macOS           | Homebrew or manual tar  | `go`       | `go mod init`      | `go install beego` |
| Ubuntu/Debian   | Manual tar installation | `go`       | `go mod init`      | `go install beego` |

---

## Why Go for APIs?

- **Fast compilation** and execution
- **Built-in concurrency** with goroutines
- **Standard library** includes HTTP server
- **Static binary** deployment - no dependencies
- **Cross-platform** compilation support
- **Memory efficient** and garbage collected

Beego is a web framework for Go that provides features for routing, templating, authentication, and creating API backends. You can use it to build a performant server-side API that handles data processing and logic in Go.
