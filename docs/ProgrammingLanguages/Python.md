Python [https://www.python.org/](Programming Language)
FastAPI [https://fastapi.tiangolo.com/](API Framework)
QUIC [https://aioquic.readthedocs.io/en/latest/](QUIC Protocol)

Auth0 [https://auth0.com/ai/docs/python-sdk](Auth0 Python SDK)

# Python Installation Guide (Latest Version)

## Overview

- Official Python site: [https://www.python.org/downloads/]
- Python is a versatile, high-level programming language used for web development, data science, AI/ML, automation, and more.
- Current stable version (as of July 2025): Python 3.12.x
- Recommended package manager: **uv** (fastest) or **pip**

---

## Windows Installation

### Step 1: Download Python

- Visit: [https://www.python.org/downloads/windows/]
- Download the latest Python 3.12.x installer (64-bit recommended)
- **Important**: During installation, check "Add Python to PATH"
- **Important**: Check "Install py launcher for all users"

### Step 2: Verify Installation

```powershell
python --version
pip --version
```

### Step 3: Install uv (Recommended Package Manager)

```powershell
pip install uv
uv --version
```

### Step 4: (Optional) Create Virtual Environment

```powershell
# Using uv (recommended)
uv venv myproject
.\myproject\Scripts\activate

# Or using built-in venv
python -m venv myproject
.\myproject\Scripts\activate
```

---

## macOS Installation

### Step 1: Install Python via Homebrew (Recommended)

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install Python
brew install python
```

### Step 2: Verify Installation

```bash
python3 --version
pip3 --version
```

### Step 3: Install uv

```bash
pip3 install uv
uv --version
```

### Step 4: (Optional) Create Virtual Environment

```bash
# Using uv (recommended)
uv venv myproject
source myproject/bin/activate

# Or using built-in venv
python3 -m venv myproject
source myproject/bin/activate
```

---

## Ubuntu / Debian Linux Installation

### Step 1: Update System and Install Python

```bash
sudo apt update
sudo apt install python3 python3-pip python3-venv
```

### Step 2: Verify Installation

```bash
python3 --version
pip3 --version
```

### Step 3: Install uv

```bash
pip3 install uv
uv --version
```

### Step 4: (Optional) Create Virtual Environment

```bash
# Using uv (recommended)
uv venv myproject
source myproject/bin/activate

# Or using built-in venv
python3 -m venv myproject
source myproject/bin/activate
```

---

## CentOS / RHEL / Fedora Installation

### For Fedora:
```bash
sudo dnf install python3 python3-pip
```

### For CentOS/RHEL:
```bash
sudo yum install python3 python3-pip
```

---

## Quick Start: FastAPI Development

### Step 1: Create Project with uv

```bash
uv venv fastapi-project
source fastapi-project/bin/activate  # On Windows: fastapi-project\Scripts\activate
```

### Step 2: Install FastAPI and Dependencies

```bash
uv add fastapi uvicorn
```

### Step 3: Create Simple API

Create `main.py`:
```python
from fastapi import FastAPI

app = FastAPI()

@app.get("/")
def read_root():
    return {"message": "Hello World from FastAPI!"}

@app.get("/items/{item_id}")
def read_item(item_id: int, q: str = None):
    return {"item_id": item_id, "q": q}
```

### Step 4: Run Development Server

```bash
uvicorn main:app --reload --port 8030
```

Visit `http://localhost:8030` and `http://localhost:8030/docs` for interactive API docs.

---

## Summary Table

| Platform        | Install Method          | Python Command | Package Manager | Virtual Env           |
|-----------------|-------------------------|----------------|----------------|-----------------------|
| Windows         | Official installer      | `python`       | `uv` or `pip`  | `uv venv` or `python -m venv` |
| macOS           | Homebrew                | `python3`      | `uv` or `pip3` | `uv venv` or `python3 -m venv` |
| Ubuntu/Debian   | APT package manager     | `python3`      | `uv` or `pip3` | `uv venv` or `python3 -m venv` |
| CentOS/RHEL     | YUM/DNF package manager | `python3`      | `uv` or `pip3` | `uv venv` or `python3 -m venv` |

---

## Why uv over pip?

- **100x faster** package installation and resolution
- **Rust-based** for exceptional performance
- **Drop-in replacement** for pip workflows
- **Built-in virtual environment** management
- **Lock files** for reproducible installs

## Deployment
FastAPI Deploy to Render.com or Alternative