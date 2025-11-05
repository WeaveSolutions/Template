C++ [https://isocpp.org/](Programming Language)

Drogon [https://drogonframework.org/](Web Framework)
CMake [https://cmake.org/](Build System)

# C++ Installation Guide (Latest Version)

## Overview

- Official C++ site: [https://isocpp.org/]
- C++ is a high-performance, general-purpose programming language ideal for system programming and APIs.
- Current standard: **C++23** (latest), **C++20** (widely supported)
- Build system: **CMake** (recommended) or **Make**
- Web framework: **Drogon** (high-performance HTTP framework)
- Excellent for high-performance APIs and system-level programming

---

## Windows Installation

### Step 1: Install Visual Studio Community (Recommended)

- Visit: [https://visualstudio.microsoft.com/vs/community/]
- Download Visual Studio Community (free)
- During installation, select:
  - "Desktop development with C++" workload
  - "CMake tools for C++"
  - "vcpkg package manager"

### Alternative: Install MinGW-w64

```powershell
# Using Chocolatey
choco install mingw

# Or download from https://www.mingw-w64.org/downloads/
```

### Step 2: Install CMake

```powershell
# Using Chocolatey
choco install cmake

# Or download from https://cmake.org/download/
```

### Step 3: Install vcpkg (Package Manager)

```powershell
git clone https://github.com/Microsoft/vcpkg.git
cd vcpkg
.\bootstrap-vcpkg.bat
.\vcpkg integrate install
```

### Step 4: Verify Installation

```powershell
g++ --version
cmake --version
.\vcpkg\vcpkg --version
```

---

## macOS Installation

### Step 1: Install Xcode Command Line Tools

```bash
xcode-select --install
```

### Step 2: Install Homebrew and Dependencies

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install CMake and modern GCC
brew install cmake gcc
```

### Step 3: Install vcpkg

```bash
git clone https://github.com/Microsoft/vcpkg.git
cd vcpkg
./bootstrap-vcpkg.sh
./vcpkg integrate install
```

### Step 4: Verify Installation

```bash
g++ --version
clang++ --version
cmake --version
```

---

## Ubuntu / Debian Linux Installation

### Step 1: Install Build Essentials

```bash
sudo apt update
sudo apt install build-essential cmake git pkg-config
```

### Step 2: Install Modern GCC (Optional - for C++20/23 features)

```bash
# For Ubuntu 22.04+ (already has GCC 11+)
sudo apt install gcc-12 g++-12

# For older Ubuntu versions
sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt update
sudo apt install gcc-12 g++-12
```

### Step 3: Install vcpkg

```bash
git clone https://github.com/Microsoft/vcpkg.git
cd vcpkg
./bootstrap-vcpkg.sh
./vcpkg integrate install
```

### Step 4: Verify Installation

```bash
g++ --version
cmake --version
```

---

## Quick Start: Drogon Web Framework

### Step 1: Install Drogon Dependencies

#### Windows (vcpkg):
```powershell
.\vcpkg\vcpkg install drogon[core,ctl]:x64-windows
```

#### macOS:
```bash
brew install jsoncpp uuid ossp-uuid openssl zlib
# Clone and build Drogon
git clone https://github.com/drogonframework/drogon.git
cd drogon
mkdir build
cd build
cmake ..
make -j$(nproc)
sudo make install
```

#### Linux:
```bash
sudo apt install libjsoncpp-dev uuid-dev openssl libssl-dev zlib1g-dev
git clone https://github.com/drogonframework/drogon.git
cd drogon
mkdir build
cd build
cmake ..
make -j$(nproc)
sudo make install
```

### Step 2: Create Drogon Project

```bash
mkdir my-cpp-api
cd my-cpp-api
```

### Step 3: Create CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.16)
project(MyCppAPI)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(Drogon CONFIG REQUIRED)

add_executable(${PROJECT_NAME} src/main.cpp)
target_link_libraries(${PROJECT_NAME} PRIVATE Drogon::Drogon)
```

### Step 4: Create Simple API

Create `src/main.cpp`:
```cpp
#include <drogon/drogon.h>
#include <json/json.h>

using namespace drogon;

int main() {
    // Health check endpoint
    app().registerHandler("/health",
        [](const HttpRequestPtr &req,
           std::function<void(const HttpResponsePtr &)> &&callback) {
            Json::Value response;
            response["message"] = "Hello from C++ Drogon API!";
            response["status"] = "success";
            response["port"] = 8110;
            
            auto resp = HttpResponse::newHttpJsonResponse(response);
            callback(resp);
        },
        {Get});
    
    // User endpoint
    app().registerHandler("/api/users/{id}",
        [](const HttpRequestPtr &req,
           std::function<void(const HttpResponsePtr &)> &&callback,
           const std::string &id) {
            Json::Value user;
            user["user_id"] = id;
            user["name"] = "User " + id;
            user["platform"] = "C++";
            
            auto resp = HttpResponse::newHttpJsonResponse(user);
            callback(resp);
        },
        {Get});
    
    // Configure server
    app().addListener("127.0.0.1", 8110);
    app().setThreadNum(4);
    
    std::cout << "🚀 C++ Drogon API starting on http://localhost:8110" << std::endl;
    
    // Start server
    app().run();
    
    return 0;
}
```

### Step 5: Build and Run

```bash
mkdir build
cd build
cmake ..
make -j$(nproc)
./MyCppAPI
```

Visit `http://localhost:8110/health` to see your C++ Drogon API.

---

## Modern C++ Features

### C++20/23 Example

```cpp
#include <drogon/drogon.h>
#include <json/json.h>
#include <ranges>
#include <concepts>
#include <coroutine>

using namespace drogon;
using namespace std;

// Modern C++ concepts
template<typename T>
concept Numeric = requires(T t) {
    t + t;
    t - t;
    t * t;
};

// Coroutine example for async operations
Task<Json::Value> processDataAsync(const vector<int>& data) {
    Json::Value result;
    
    // Use ranges (C++20)
    auto filtered = data 
        | views::filter([](int x) { return x > 0; })
        | views::transform([](int x) { return x * 2; });
    
    Json::Value arr(Json::arrayValue);
    for (int val : filtered) {
        arr.append(val);
    }
    
    result["processed_data"] = arr;
    result["count"] = static_cast<int>(ranges::distance(filtered));
    
    co_return result;
}
```

---

## Build Systems Comparison

### CMake (Recommended)

```cmake
# Modern CMake practices
cmake_minimum_required(VERSION 3.20)
project(MyAPI VERSION 1.0.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

# Enable modern C++ warnings
if(MSVC)
    target_compile_options(MyAPI PRIVATE /W4 /WX)
else()
    target_compile_options(MyAPI PRIVATE -Wall -Wextra -Wpedantic -Werror)
endif()

find_package(Drogon CONFIG REQUIRED)
add_executable(MyAPI src/main.cpp)
target_link_libraries(MyAPI PRIVATE Drogon::Drogon)
```

### Meson (Alternative)

```meson
project('my-cpp-api', 'cpp',
  version : '1.0.0',
  default_options : ['cpp_std=c++20'])

drogon_dep = dependency('drogon')

executable('my-cpp-api',
  'src/main.cpp',
  dependencies : [drogon_dep],
  install : true)
```

---

## Summary Table

| Platform        | Compiler           | Build System | Package Manager | Web Framework |
|-----------------|--------------------|--------------|----------------|---------------|
| Windows         | MSVC/MinGW         | CMake        | vcpkg          | Drogon        |
| macOS           | Clang/GCC          | CMake        | vcpkg/Homebrew | Drogon        |
| Ubuntu/Debian   | GCC/Clang          | CMake        | vcpkg/APT      | Drogon        |

---

## Why C++ for APIs?

- **Unmatched performance** - near-metal speed
- **Memory control** - manual memory management for optimization
- **Zero-cost abstractions** - high-level features without runtime overhead
- **Mature ecosystem** - decades of libraries and tools
- **Cross-platform** - runs on virtually any hardware/OS
- **Modern features** - C++20/23 bring powerful language improvements
- **Industry standard** - used in high-performance systems worldwide

---

## Performance Optimization Tips

### Compiler Optimizations

```bash
# Release build with optimizations
cmake -DCMAKE_BUILD_TYPE=Release ..

# Or manually specify optimization flags
g++ -O3 -march=native -flto main.cpp -o my-api
```

### Memory Management

```cpp
// Use smart pointers
std::unique_ptr<MyClass> obj = std::make_unique<MyClass>();
std::shared_ptr<Resource> resource = std::make_shared<Resource>();

// RAII for resource management
class DatabaseConnection {
public:
    DatabaseConnection() { /* connect */ }
    ~DatabaseConnection() { /* disconnect */ }
};
```

### Concurrency

```cpp
#include <thread>
#include <future>
#include <execution>

// Parallel STL algorithms (C++17)
std::vector<int> data = {1, 2, 3, 4, 5};
std::for_each(std::execution::par_unseq, data.begin(), data.end(),
              [](int& x) { x *= 2; });

// Async operations
auto future = std::async(std::launch::async, []() {
    return expensiveComputation();
});
```