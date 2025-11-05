Genie.jl [https://genieframework.com/](Web Framework)
Genie Builder [https://juliahub.com/blog/announcing-genie-builder-on-juliahub](Visual Web App Builder)

# Julia Installation Guide (Latest Version)

## Overview

- Official Julia site: [https://julialang.org/downloads/]
- Julia is a high-performance programming language designed for numerical and scientific computing.
- Current stable version (as of July 2025): Julia 1.10.x
- Package manager: **Pkg** (built-in Julia package manager)
- Excellent for scientific computing, machine learning, and high-performance web APIs

---

## Windows Installation

### Step 1: Download Julia

- Visit: [https://julialang.org/downloads/]
- Download the latest Windows installer (64-bit recommended)
- Run the installer and follow the setup wizard
- **Important**: Check "Add Julia to PATH" during installation

### Step 2: Verify Installation

```powershell
julia --version
```

### Step 3: Start Julia REPL

```powershell
julia
```

In the Julia REPL:
```julia
# Test basic functionality
println("Hello from Julia!")

# Exit Julia
exit()
```

### Step 4: Install Genie.jl (Web Framework)

```powershell
julia -e "using Pkg; Pkg.add(\"Genie\")"
```

---

## macOS Installation

### Step 1: Install Julia via Homebrew (Recommended)

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install Julia
brew install julia
```

### Alternative: Manual Installation

```bash
# Download and install manually
wget https://julialang-s3.julialang.org/bin/mac/x64/1.10/julia-1.10.x-mac64.dmg
# Open the DMG and drag Julia to Applications
```

### Step 2: Verify Installation

```bash
julia --version
```

### Step 3: Install Genie.jl

```bash
julia -e "using Pkg; Pkg.add(\"Genie\")"
```

---

## Ubuntu / Debian Linux Installation

### Step 1: Download Julia

```bash
wget https://julialang-s3.julialang.org/bin/linux/x64/1.10/julia-1.10.x-linux-x86_64.tar.gz
```

### Step 2: Extract and Install

```bash
tar -xzf julia-1.10.x-linux-x86_64.tar.gz
sudo mv julia-1.10.x /opt/julia
```

### Step 3: Add to PATH

```bash
echo 'export PATH="$PATH:/opt/julia/bin"' >> ~/.bashrc
source ~/.bashrc
```

### Step 4: Verify Installation

```bash
julia --version
```

### Step 5: Install Genie.jl

```bash
julia -e "using Pkg; Pkg.add(\"Genie\")"
```

---

## Quick Start: Genie.jl Web Framework

### Step 1: Create New Genie Project

```bash
julia
```

In Julia REPL:
```julia
using Genie
Genie.Generator.newapp("MyGenieAPI")
exit()
```

### Step 2: Navigate to Project

```bash
cd MyGenieAPI
```

### Step 3: Start Development Server

```bash
julia --project -e "using Genie; Genie.loadapp(); up()"
```

### Step 4: Create API Routes

Create `routes.jl` in your project:
```julia
using Genie.Router
using Genie.Responses
using JSON3

# Health check endpoint
route("/health") do
    response = Dict(
        "message" => "Hello from Julia Genie API!",
        "status" => "success",
        "port" => 8090,
        "julia_version" => string(VERSION)
    )
    JSON3.write(response) |> json
end

# User endpoint
route("/api/users/:id") do
    user_id = params(:id)
    user = Dict(
        "user_id" => user_id,
        "name" => "User $user_id",
        "platform" => "Julia",
        "capabilities" => ["scientific_computing", "machine_learning", "high_performance"]
    )
    JSON3.write(user) |> json
end

# Data analysis endpoint
route("/api/data/stats", method = POST) do
    try
        data = JSON3.read(rawpayload(), Dict)
        values = data["values"]
        
        stats = Dict(
            "mean" => mean(values),
            "median" => median(values),
            "std" => std(values),
            "min" => minimum(values),
            "max" => maximum(values),
            "count" => length(values)
        )
        
        JSON3.write(stats) |> json
    catch e
        JSON3.write(Dict("error" => string(e))) |> json
    end
end
```

### Step 5: Install Required Packages

```bash
julia --project -e "using Pkg; Pkg.add([\"JSON3\", \"Statistics\"])"
```

### Step 6: Start Server

```bash
julia --project -e "using Genie; Genie.loadapp(); up(8090)"
```

Visit `http://localhost:8090/health` to see your Julia Genie API.

---

## Julia Package Management

### Common Pkg Commands

```julia
# In Julia REPL, press ]
Pkg> add PackageName          # Install package
Pkg> remove PackageName       # Remove package
Pkg> update                   # Update all packages
Pkg> status                   # Show installed packages
Pkg> activate .               # Activate project environment
Pkg> instantiate              # Install project dependencies
```

### Using Pkg in Scripts

```julia
using Pkg
Pkg.add("Genie")
Pkg.add(["JSON3", "HTTP", "Statistics"])
```

---

## Performance Optimization

### Pre-compilation for Production

```bash
# Create system image for faster startup
julia --project -e "using PackageCompiler; create_sysimage([:Genie, :JSON3, :HTTP]; sysimage_path=\"genie_sysimage.so\")"

# Run with custom system image
julia --project --sysimage genie_sysimage.so -e "using Genie; Genie.loadapp(); up()"
```

### Multi-threading

```bash
# Start Julia with multiple threads
julia --project --threads=4 -e "using Genie; Genie.loadapp(); up()"
```

---

## Summary Table

| Platform        | Install Method       | Julia Command | Package Manager | Web Framework     |
|-----------------|---------------------|---------------|-----------------|-------------------|
| Windows         | Official installer   | `julia`       | `Pkg`           | `Pkg.add("Genie")` |
| macOS           | Homebrew or manual   | `julia`       | `Pkg`           | `Pkg.add("Genie")` |
| Ubuntu/Debian   | Manual tar extract   | `julia`       | `Pkg`           | `Pkg.add("Genie")` |

---

## Why Julia for APIs?

- **High performance** approaching C/Fortran speeds
- **Dynamic language** with static language performance
- **Excellent for data science** and machine learning
- **Built-in parallelism** and distributed computing
- **Rich ecosystem** for scientific computing
- **Easy C/Python interop** for existing libraries