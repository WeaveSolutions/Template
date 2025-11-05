Elixir [https://elixir-lang.org/](Programming Language)

Phoenix [https://phoenixframework.org/](Web Framework)
Erlang/OTP [https://www.erlang.org/](Runtime Platform)

# Elixir Installation Guide (Latest Version)

## Overview

- Official Elixir site: [https://elixir-lang.org/]
- Elixir is a dynamic, functional language designed for building maintainable and scalable applications.
- Current version: **Elixir 1.16** (requires Erlang/OTP 24+)
- Built on **Erlang/OTP** for fault-tolerance and concurrency
- Web framework: **Phoenix** (highly productive, fault-tolerant web framework)
- Package manager: **Hex** (built-in)
- Excellent for real-time applications, IoT, and distributed systems

---

## Windows Installation

### Step 1: Install Erlang/OTP

- Visit: [https://www.erlang.org/downloads]
- Download Erlang/OTP 26 for Windows (x64)
- Run the installer and follow the setup wizard
- **Important**: Add Erlang to PATH during installation

### Alternative: Use Chocolatey

```powershell
# Using Chocolatey (recommended)
choco install erlang
choco install elixir
```

### Step 2: Install Elixir (Manual)

- Visit: [https://github.com/elixir-lang/elixir/releases]
- Download Elixir 1.16 precompiled for Windows
- Extract to `C:\elixir`
- Add `C:\elixir\bin` to your PATH

### Step 3: Verify Installation

```powershell
erl -version  # Erlang version
elixir --version
iex --version  # Interactive Elixir shell
```

---

## macOS Installation

### Step 1: Install via Homebrew (Recommended)

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install Elixir (automatically installs Erlang/OTP as dependency)
brew install elixir
```

### Alternative: Install with asdf (Version Manager)

```bash
# Install asdf
git clone https://github.com/asdf-vm/asdf.git ~/.asdf --branch v0.14.0
echo '. "$HOME/.asdf/asdf.sh"' >> ~/.zshrc
echo '. "$HOME/.asdf/completions/asdf.bash"' >> ~/.zshrc
source ~/.zshrc

# Install Erlang and Elixir plugins
asdf plugin add erlang https://github.com/asdf-vm/asdf-erlang.git
asdf plugin add elixir https://github.com/asdf-vm/asdf-elixir.git

# Install specific versions
asdf install erlang 26.2.1
asdf install elixir 1.16.0-otp-26
asdf global erlang 26.2.1
asdf global elixir 1.16.0-otp-26
```

### Step 2: Verify Installation

```bash
erl -version
elixir --version
iex --version
```

---

## Ubuntu / Debian Linux Installation

### Step 1: Install Erlang/OTP

```bash
# Add Erlang Solutions repository
wget https://packages.erlang-solutions.com/erlang-solutions_2.0_all.deb
sudo dpkg -i erlang-solutions_2.0_all.deb
rm erlang-solutions_2.0_all.deb

# Update package list and install Erlang
sudo apt update
sudo apt install esl-erlang
```

### Step 2: Install Elixir

```bash
# Install Elixir
sudo apt install elixir
```

### Alternative: Build from Source

```bash
# Install dependencies
sudo apt install build-essential autoconf m4 libncurses5-dev libwxgtk3.0-gtk3-dev libwxgtk-webview3.0-gtk3-dev libgl1-mesa-dev libglu1-mesa-dev libpng-dev libssh-dev unixodbc-dev xsltproc fop libxml2-utils libncurses-dev openjdk-11-jdk

# Clone and build Elixir
git clone https://github.com/elixir-lang/elixir.git
cd elixir
make clean test
sudo make install
```

### Step 3: Verify Installation

```bash
erl -version
elixir --version
iex --version
```

---

## Quick Start: Phoenix Web Framework

### Step 1: Install Hex and Phoenix

```bash
# Install Hex package manager (if not already installed)
mix local.hex

# Install Phoenix application generator
mix archive.install hex phx_new
```

### Step 2: Install Node.js (for Phoenix assets)

#### Windows:
```powershell
choco install nodejs
```

#### macOS:
```bash
brew install node
```

#### Ubuntu/Debian:
```bash
curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
sudo apt-get install -y nodejs
```

### Step 3: Create Phoenix Project

```bash
mix phx.new my_elixir_api --no-ecto --no-html --no-assets
cd my_elixir_api
```

### Step 4: Add API Routes and Controller

Update `lib/my_elixir_api_web/router.ex`:
```elixir
defmodule MyElixirApiWeb.Router do
  use MyElixirApiWeb, :router

  pipeline :api do
    plug :accepts, ["json"]
  end

  scope "/api", MyElixirApiWeb do
    pipe_through :api
    
    get "/health", ApiController, :health
    get "/users/:id", ApiController, :get_user
    post "/users", ApiController, :create_user
  end
end
```

Create `lib/my_elixir_api_web/controllers/api_controller.ex`:
```elixir
defmodule MyElixirApiWeb.ApiController do
  use MyElixirApiWeb, :controller

  def health(conn, _params) do
    response = %{
      message: "Hello from Elixir Phoenix API!",
      status: "success",
      port: 8140,
      timestamp: DateTime.utc_now()
    }
    
    json(conn, response)
  end

  def get_user(conn, %{"id" => id}) do
    user = %{
      user_id: id,
      name: "User #{id}",
      platform: "Elixir",
      framework: "Phoenix"
    }
    
    json(conn, user)
  end

  def create_user(conn, %{"name" => name, "email" => email}) do
    user = %{
      user_id: Ecto.UUID.generate(),
      name: name,
      email: email,
      platform: "Elixir",
      created_at: DateTime.utc_now()
    }
    
    conn
    |> put_status(:created)
    |> json(user)
  end

  def create_user(conn, _params) do
    conn
    |> put_status(:bad_request)
    |> json(%{error: "Missing required fields: name, email"})
  end
end
```

### Step 5: Configure Port

Update `config/dev.exs`:
```elixir
config :my_elixir_api, MyElixirApiWeb.Endpoint,
  # ...
  http: [ip: {127, 0, 0, 1}, port: 8140],
  # ...
```

### Step 6: Start the Server

```bash
# Install dependencies
mix deps.get

# Start Phoenix server
mix phx.server

# Or start with interactive shell
iex -S mix phx.server
```

Visit `http://localhost:8140/api/health` to see your Elixir Phoenix API.

---

## Advanced Elixir Features

### GenServer for State Management

Create `lib/my_elixir_api/user_store.ex`:
```elixir
defmodule MyElixirApi.UserStore do
  use GenServer

  # Client API
  def start_link(opts) do
    GenServer.start_link(__MODULE__, %{}, opts)
  end

  def get_user(server, id) do
    GenServer.call(server, {:get_user, id})
  end

  def create_user(server, user_data) do
    GenServer.call(server, {:create_user, user_data})
  end

  def list_users(server) do
    GenServer.call(server, :list_users)
  end

  # Server Callbacks
  def init(state) do
    {:ok, state}
  end

  def handle_call({:get_user, id}, _from, state) do
    user = Map.get(state, id, nil)
    {:reply, user, state}
  end

  def handle_call({:create_user, user_data}, _from, state) do
    id = Ecto.UUID.generate()
    user = Map.put(user_data, :user_id, id)
    new_state = Map.put(state, id, user)
    {:reply, {:ok, user}, new_state}
  end

  def handle_call(:list_users, _from, state) do
    users = Map.values(state)
    {:reply, users, state}
  end
end
```

### Pattern Matching and Guards

```elixir
defmodule MyElixirApi.UserValidator do
  def validate_user(%{"name" => name, "email" => email}) 
      when is_binary(name) and byte_size(name) > 0 
      and is_binary(email) and byte_size(email) > 0 do
    case validate_email(email) do
      true -> {:ok, %{name: name, email: email}}
      false -> {:error, "Invalid email format"}
    end
  end

  def validate_user(_), do: {:error, "Missing required fields"}

  defp validate_email(email) do
    email =~ ~r/^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Z|a-z]{2,}$/
  end
end
```

### Task for Async Operations

```elixir
defmodule MyElixirApi.AsyncProcessor do
  def process_data_async(data) do
    Task.async(fn ->
      # Simulate heavy computation
      Process.sleep(1000)
      
      data
      |> Enum.filter(&(&1 > 0))
      |> Enum.map(&(&1 * 2))
      |> Enum.sum()
    end)
  end

  def get_result(task) do
    case Task.yield(task, 5000) do
      {:ok, result} -> {:ok, result}
      nil -> 
        Task.shutdown(task)
        {:error, :timeout}
    end
  end
end

# Usage in controller
def async_process(conn, %{"data" => data}) do
  task = MyElixirApi.AsyncProcessor.process_data_async(data)
  
  case MyElixirApi.AsyncProcessor.get_result(task) do
    {:ok, result} -> json(conn, %{result: result})
    {:error, :timeout} -> 
      conn
      |> put_status(:request_timeout)
      |> json(%{error: "Processing timeout"})
  end
end
```

### LiveView for Real-time Updates

Add to `mix.exs` dependencies:
```elixir
{:phoenix_live_view, "~> 0.20.0"}
```

Create `lib/my_elixir_api_web/live/dashboard_live.ex`:
```elixir
defmodule MyElixirApiWeb.DashboardLive do
  use MyElixirApiWeb, :live_view

  def mount(_params, _session, socket) do
    if connected?(socket) do
      :timer.send_interval(1000, self(), :tick)
    end
    
    {:ok, assign(socket, :counter, 0)}
  end

  def handle_info(:tick, socket) do
    {:noreply, assign(socket, :counter, socket.assigns.counter + 1)}
  end

  def render(assigns) do
    ~H"""
    <div>
      <h1>Real-time Counter</h1>
      <p>Count: <%= @counter %></p>
      <button phx-click="increment">+1</button>
      <button phx-click="decrement">-1</button>
    </div>
    """
  end

  def handle_event("increment", _params, socket) do
    {:noreply, assign(socket, :counter, socket.assigns.counter + 1)}
  end

  def handle_event("decrement", _params, socket) do
    {:noreply, assign(socket, :counter, socket.assigns.counter - 1)}
  end
end
```

---

## Package Management with Hex

### Essential Packages

```elixir
# Add to mix.exs dependencies
defp deps do
  [
    {:phoenix, "~> 1.7.0"},
    {:phoenix_live_view, "~> 0.20.0"},
    {:jason, "~> 1.4"},  # JSON encoding/decoding
    {:httpoison, "~> 2.0"},  # HTTP client
    {:tesla, "~> 1.7"},  # HTTP client with middleware
    {:ecto_sql, "~> 3.10"},  # Database wrapper
    {:postgrex, ">= 0.0.0"},  # PostgreSQL adapter
    {:cors_plug, "~> 3.0"},  # CORS handling
    {:guardian, "~> 2.3"},  # JWT authentication
    {:bcrypt_elixir, "~> 3.0"},  # Password hashing
    {:ex_machina, "~> 2.7", only: :test},  # Test factories
    {:credo, "~> 1.7", only: [:dev, :test], runtime: false}  # Code analysis
  ]
end
```

### Install Dependencies

```bash
mix deps.get
mix deps.compile
```

---

## Summary Table

| Platform      | Installation       | Package Manager | Web Framework | Runtime    |
|---------------|--------------------|-----------------|---------------|------------|
| Windows       | Chocolatey/Manual  | Hex             | Phoenix       | Erlang/OTP |
| macOS         | Homebrew/asdf      | Hex             | Phoenix       | Erlang/OTP |
| Ubuntu/Debian | APT/Source         | Hex             | Phoenix       | Erlang/OTP |

---

## Why Elixir for APIs?

- **Fault tolerance** - "Let it crash" philosophy with supervisor trees
- **Concurrency** - Lightweight processes (Actor model)
- **Scalability** - Handle millions of concurrent connections
- **Hot code swapping** - Update code without stopping the system
- **Pattern matching** - Powerful feature for clean, readable code
- **Functional programming** - Immutable data and pure functions
- **Real-time capabilities** - Built-in support for WebSockets and LiveView
- **Distributed systems** - Built for clustering and distribution

---

## Development Best Practices

### Error Handling with Pattern Matching

```elixir
defmodule MyElixirApi.UserService do
  def create_user(attrs) do
    with {:ok, validated} <- validate_user(attrs),
         {:ok, user} <- save_user(validated),
         {:ok, _email} <- send_welcome_email(user) do
      {:ok, user}
    else
      {:error, :validation_failed} -> {:error, "Invalid user data"}
      {:error, :user_exists} -> {:error, "User already exists"}
      {:error, :email_failed} -> {:error, "Failed to send welcome email"}
    end
  end

  defp validate_user(%{name: name, email: email}) 
      when is_binary(name) and is_binary(email) do
    {:ok, %{name: name, email: email}}
  end
  
  defp validate_user(_), do: {:error, :validation_failed}
  
  defp save_user(user_data) do
    # Database logic here
    {:ok, Map.put(user_data, :id, Ecto.UUID.generate())}
  end
  
  defp send_welcome_email(_user) do
    # Email sending logic
    {:ok, :sent}
  end
end
```

### Testing with ExUnit

```elixir
defmodule MyElixirApiWeb.ApiControllerTest do
  use MyElixirApiWeb.ConnCase

  test "GET /api/health", %{conn: conn} do
    conn = get(conn, "/api/health")
    
    assert json_response(conn, 200) == %{
      "message" => "Hello from Elixir Phoenix API!",
      "status" => "success",
      "port" => 8140
    }
  end

  test "GET /api/users/:id", %{conn: conn} do
    conn = get(conn, "/api/users/123")
    
    assert json_response(conn, 200) == %{
      "user_id" => "123",
      "name" => "User 123",
      "platform" => "Elixir",
      "framework" => "Phoenix"
    }
  end
end
```

### Configuration Management

```elixir
# config/config.exs
import Config

config :my_elixir_api, MyElixirApiWeb.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "your-secret-key-base",
  render_errors: [view: MyElixirApiWeb.ErrorView, accepts: ~w(json)]

# config/dev.exs
import Config

config :my_elixir_api, MyElixirApiWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: 8140],
  debug_errors: true,
  code_reloader: true,
  check_origin: false

# Access configuration
port = Application.get_env(:my_elixir_api, MyElixirApiWeb.Endpoint)[:http][:port]
```