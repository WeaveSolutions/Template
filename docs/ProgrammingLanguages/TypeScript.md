TypeScript [https://www.typescriptlang.org/](Programming Language)

Node.js [https://nodejs.org/](Runtime Environment)
Express [https://expressjs.com/](Web Framework)

# TypeScript Installation Guide (Latest Version)

## Overview

- Official TypeScript site: [https://www.typescriptlang.org/download]
- TypeScript is a superset of JavaScript that adds static typing and other features for building large-scale applications.
- Current stable version (as of July 2025): TypeScript 5.5.x
- Recommended package manager: **Bun** (fastest) or **pnpm**
- Requires Node.js runtime environment

---

## Windows Installation

### Step 1: Install Node.js

- Visit: [https://nodejs.org/en/download/]
- Download the latest LTS version (recommended)
- Run the installer and follow the setup wizard
- **Important**: Check "Add to PATH" during installation

### Step 2: Install Bun (Recommended Package Manager)

```powershell
powershell -c "irm bun.sh/install.ps1 | iex"
```

### Step 3: Install TypeScript Globally

```powershell
# With Bun (recommended)
bun install -g typescript

# Or with pnpm (fallback)
pnpm install -g typescript
```

### Step 4: Verify Installation

```powershell
node --version
bun --version
tsc --version
```

### Step 5: Create TypeScript Project

```powershell
mkdir my-typescript-project
cd my-typescript-project
bun init
bun add -D @types/node
tsc --init
```

---

## macOS Installation

### Step 1: Install Node.js via Homebrew

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install Node.js
brew install node
```

### Step 2: Install Bun

```bash
curl -fsSL https://bun.sh/install | bash
```

### Step 3: Install TypeScript

```bash
# With Bun (recommended)
bun install -g typescript

# Or with pnpm (fallback)
pnpm install -g typescript
```

### Step 4: Verify Installation

```bash
node --version
bun --version
tsc --version
```

### Step 5: Create TypeScript Project

```bash
mkdir my-typescript-project
cd my-typescript-project
bun init
bun add -D @types/node
tsc --init
```

Node.js is a runtime environment for executing JavaScript code on the server-side. It is built on top of the V8 JavaScript engine, which is the same engine used by Google Chrome. Node.js provides a set of built-in modules and APIs that allow developers to build server-side applications using JavaScript. It is a popular choice for building APIs and microservices, as it is fast, scalable, and has a large community of developers.

Express is a web application framework for Node.js. It provides a set of tools and features for building web applications, including routing, middleware, and template rendering. It is a popular choice for building web applications, as it is easy to use, has a large community of developers, and is compatible with a wide range of web technologies.

TypeScript is a programming language that is a superset of JavaScript. It adds features such as static typing, interfaces, and classes, which make it easier to write and maintain code. TypeScript is a popular choice for building large-scale applications, as it provides better code quality and helps prevent errors.
