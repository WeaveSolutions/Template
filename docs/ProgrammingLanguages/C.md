C [https://en.cppreference.com/w/c](Programming Language)

Libmicrohttpd [https://www.gnu.org/software/libmicrohttpd/](HTTP Server Library)
Civetweb [https://github.com/civetweb/civetweb](Web Server Library)

# C Installation Guide (Latest Version)

## Overview

- Official C reference: [https://en.cppreference.com/w/c]
- C is a foundational, system-level programming language ideal for high-performance applications.
- Current standard: **C23** (latest), **C17/C18** (widely supported)
- Build system: **Make** (traditional) or **CMake** (modern)
- Web libraries: **Civetweb** or **libmicrohttpd** for HTTP servers
- Excellent for embedded systems, OS development, and high-performance APIs

---

## Windows Installation

### Step 1: Install Visual Studio Community (Recommended)

- Visit: [https://visualstudio.microsoft.com/vs/community/]
- Download Visual Studio Community (free)
- During installation, select:
  - "Desktop development with C++" workload (includes C compiler)
  - "CMake tools for C++"
  - "Git for Windows"

### Alternative: Install MinGW-w64

```powershell
# Using Chocolatey
choco install mingw

# Or download from https://www.mingw-w64.org/downloads/
```

### Step 2: Install CMake (Optional but Recommended)

```powershell
# Using Chocolatey
choco install cmake

# Or download from https://cmake.org/download/
```

### Step 3: Install Make (If using MinGW)

```powershell
# Make is included with MinGW, verify:
make --version
```

### Step 4: Verify Installation

```powershell
gcc --version
cl # For MSVC
cmake --version
```

---

## macOS Installation

### Step 1: Install Xcode Command Line Tools

```bash
xcode-select --install
```

### Step 2: Install Homebrew and Dependencies (Optional)

```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install modern GCC and CMake
brew install gcc cmake
```

### Step 3: Verify Installation

```bash
clang --version
gcc --version  # If installed via Homebrew
cmake --version
make --version
```

---

## Ubuntu / Debian Linux Installation

### Step 1: Install Build Essentials

```bash
sudo apt update
sudo apt install build-essential cmake git pkg-config
```

### Step 2: Install Modern GCC (Optional - for C23 features)

```bash
# For Ubuntu 22.04+ (already has GCC 11+)
sudo apt install gcc-12

# For older Ubuntu versions
sudo add-apt-repository ppa:ubuntu-toolchain-r/test
sudo apt update
sudo apt install gcc-12
```

### Step 3: Install Additional Development Tools

```bash
sudo apt install gdb valgrind clang-format
```

### Step 4: Verify Installation

```bash
gcc --version
clang --version
cmake --version
make --version
```

---

## Quick Start: Civetweb HTTP Server

### Step 1: Clone and Build Civetweb

#### Windows:
```powershell
git clone https://github.com/civetweb/civetweb.git
cd civetweb
mkdir build
cd build
cmake ..
cmake --build . --config Release
```

#### macOS/Linux:
```bash
git clone https://github.com/civetweb/civetweb.git
cd civetweb
make
sudo make install
```

### Step 2: Create C API Project

```bash
mkdir my-c-api
cd my-c-api
```

### Step 3: Create Simple HTTP Server

Create `main.c`:
```c
#include "civetweb.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// Health check handler
static int health_handler(struct mg_connection *conn, void *cbdata) {
    const char *response = 
        "HTTP/1.1 200 OK\r\n"
        "Content-Type: application/json\r\n"
        "Access-Control-Allow-Origin: *\r\n"
        "\r\n"
        "{\n"
        "  \"message\": \"Hello from C Civetweb API!\",\n"
        "  \"status\": \"success\",\n"
        "  \"port\": 8120\n"
        "}";
    
    mg_write(conn, response, strlen(response));
    return 200;
}

// User handler
static int user_handler(struct mg_connection *conn, void *cbdata) {
    const struct mg_request_info *req = mg_get_request_info(conn);
    const char *user_id = "1"; // Simple example
    
    // Extract user ID from URL if needed
    if (req->query_string) {
        // Parse query parameters here
    }
    
    char response[512];
    snprintf(response, sizeof(response),
        "HTTP/1.1 200 OK\r\n"
        "Content-Type: application/json\r\n"
        "Access-Control-Allow-Origin: *\r\n"
        "\r\n"
        "{\n"
        "  \"user_id\": \"%s\",\n"
        "  \"name\": \"User %s\",\n"
        "  \"platform\": \"C\"\n"
        "}", user_id, user_id);
    
    mg_write(conn, response, strlen(response));
    return 200;
}

int main(void) {
    struct mg_context *ctx;
    struct mg_callbacks callbacks;
    const char *options[] = {
        "listening_ports", "8120",
        "num_threads", "4",
        NULL
    };
    
    // Initialize callbacks
    memset(&callbacks, 0, sizeof(callbacks));
    
    // Start server
    ctx = mg_start(&callbacks, NULL, options);
    
    if (ctx == NULL) {
        printf("Failed to start Civetweb server.\n");
        return 1;
    }
    
    // Set up handlers
    mg_set_request_handler(ctx, "/health", health_handler, NULL);
    mg_set_request_handler(ctx, "/api/users", user_handler, NULL);
    
    printf("🚀 C Civetweb API starting on http://localhost:8120\n");
    printf("Press Enter to stop the server...\n");
    
    getchar(); // Wait for user input
    
    mg_stop(ctx);
    printf("Server stopped.\n");
    
    return 0;
}
```

### Step 4: Create Makefile

Create `Makefile`:
```makefile
CC=gcc
CFLAGS=-Wall -Wextra -std=c17 -O2
LDFLAGS=-lcivetweb -lpthread -lm

# Adjust paths based on your installation
INCLUDES=-I/usr/local/include
LIBPATHS=-L/usr/local/lib

TARGET=my-c-api
SOURCES=main.c

$(TARGET): $(SOURCES)
	$(CC) $(CFLAGS) $(INCLUDES) -o $(TARGET) $(SOURCES) $(LIBPATHS) $(LDFLAGS)

clean:
	rm -f $(TARGET)

run: $(TARGET)
	./$(TARGET)

.PHONY: clean run
```

### Step 5: Build and Run

```bash
make
make run
# Or directly:
# ./my-c-api
```

Visit `http://localhost:8120/health` to see your C API.

---

## Modern C Features (C17/C23)

### C17 Example with Better Error Handling

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <assert.h>

// Modern C error handling
typedef enum {
    RESULT_OK = 0,
    RESULT_ERROR_MEMORY,
    RESULT_ERROR_IO,
    RESULT_ERROR_INVALID_INPUT
} result_t;

// Safer string operations
result_t safe_strcpy(char *dest, size_t dest_size, const char *src) {
    if (!dest || !src || dest_size == 0) {
        return RESULT_ERROR_INVALID_INPUT;
    }
    
    size_t src_len = strlen(src);
    if (src_len >= dest_size) {
        return RESULT_ERROR_INVALID_INPUT;
    }
    
    strcpy(dest, src);
    return RESULT_OK;
}

// Memory management helpers
void* safe_malloc(size_t size) {
    void *ptr = malloc(size);
    if (!ptr) {
        fprintf(stderr, "Memory allocation failed: %zu bytes\n", size);
        exit(EXIT_FAILURE);
    }
    return ptr;
}

// C23 features (when available)
#if __STDC_VERSION__ >= 202311L
#include <stdbit.h>

unsigned int count_set_bits(unsigned int value) {
    return stdc_count_ones(value);
}
#endif
```

### JSON Response Helper (Manual)

```c
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>

// Simple JSON response builder
char* build_json_response(const char* format, ...) {
    va_list args;
    va_start(args, format);
    
    // Calculate required size
    int size = vsnprintf(NULL, 0, format, args);
    va_end(args);
    
    if (size < 0) return NULL;
    
    char* buffer = malloc(size + 1);
    if (!buffer) return NULL;
    
    va_start(args, format);
    vsnprintf(buffer, size + 1, format, args);
    va_end(args);
    
    return buffer;
}

// Usage example
void send_user_response(struct mg_connection *conn, const char* user_id, const char* name) {
    char* json = build_json_response(
        "{\"user_id\": \"%s\", \"name\": \"%s\", \"platform\": \"C\"}",
        user_id, name
    );
    
    if (json) {
        char response[1024];
        snprintf(response, sizeof(response),
            "HTTP/1.1 200 OK\r\n"
            "Content-Type: application/json\r\n"
            "\r\n%s", json);
        
        mg_write(conn, response, strlen(response));
        free(json);
    }
}
```

---

## Build Systems Comparison

### Traditional Makefile

```makefile
CC=gcc
CFLAGS=-Wall -Wextra -std=c17 -O2 -g
LDFLAGS=-lcivetweb -lpthread

SRCDIR=src
BUILDDIR=build
SOURCES=$(wildcard $(SRCDIR)/*.c)
OBJECTS=$(SOURCES:$(SRCDIR)/%.c=$(BUILDDIR)/%.o)
TARGET=$(BUILDDIR)/my-c-api

.PHONY: all clean install

all: $(TARGET)

$(TARGET): $(OBJECTS) | $(BUILDDIR)
	$(CC) $(OBJECTS) -o $@ $(LDFLAGS)

$(BUILDDIR)/%.o: $(SRCDIR)/%.c | $(BUILDDIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(BUILDDIR):
	mkdir -p $(BUILDDIR)

clean:
	rm -rf $(BUILDDIR)

install: $(TARGET)
	cp $(TARGET) /usr/local/bin/
```

### CMake (Modern Approach)

```cmake
cmake_minimum_required(VERSION 3.16)
project(MyCAPI VERSION 1.0.0 LANGUAGES C)

set(CMAKE_C_STANDARD 17)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Find dependencies
find_package(PkgConfig REQUIRED)
pkg_check_modules(CIVETWEB REQUIRED civetweb)

# Compiler flags
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    add_compile_options(-Wall -Wextra -Wpedantic)
endif()

# Executable
add_executable(my-c-api src/main.c)
target_link_libraries(my-c-api ${CIVETWEB_LIBRARIES})
target_include_directories(my-c-api PRIVATE ${CIVETWEB_INCLUDE_DIRS})

# Install
install(TARGETS my-c-api DESTINATION bin)
```

---

## Summary Table

| Platform      | Compiler    | Build System   | HTTP Library | Development Tools  |
|---------------|-------------|----------------|--------------|--------------------|
| Windows       | MSVC/MinGW  | Make/CMake     | Civetweb     | Visual Studio      |
| macOS         | Clang/GCC   | Make/CMake     | Civetweb     | Xcode CLI Tools    |
| Ubuntu/Debian | GCC/Clang   | Make/CMake     | Civetweb     | GDB, Valgrind      |

---

## Why C for APIs?

- **Ultimate performance** - minimal overhead, direct hardware control
- **Memory efficiency** - precise memory management
- **Portability** - runs on virtually any platform
- **Minimal dependencies** - small footprint
- **Industry standard** - foundational language for system programming
- **Embedded systems** - ideal for IoT and microcontroller applications
- **Legacy integration** - seamless integration with existing C codebases

---

## Development Best Practices

### Memory Safety

```c
#include <stdlib.h>
#include <string.h>

// Always check malloc return values
char* safe_strdup(const char* str) {
    if (!str) return NULL;
    
    size_t len = strlen(str) + 1;
    char* copy = malloc(len);
    if (!copy) {
        fprintf(stderr, "Memory allocation failed\n");
        return NULL;
    }
    
    memcpy(copy, str, len);
    return copy;
}

// RAII-like cleanup with gcc/clang
void cleanup_file(FILE **fp) {
    if (fp && *fp) {
        fclose(*fp);
        *fp = NULL;
    }
}

#define auto_cleanup_file __attribute__((cleanup(cleanup_file)))

void example_usage() {
    auto_cleanup_file FILE *file = fopen("data.txt", "r");
    if (!file) return;
    
    // File automatically closed when going out of scope
    // Process file...
}
```

### Error Handling

```c
#include <errno.h>
#include <string.h>

typedef struct {
    int code;
    char message[256];
} error_t;

#define ERROR_NONE {0, ""}
#define ERROR_MEMORY {-1, "Memory allocation failed"}
#define ERROR_IO {-2, "I/O operation failed"}

error_t process_file(const char* filename) {
    FILE *file = fopen(filename, "r");
    if (!file) {
        error_t err;
        err.code = errno;
        snprintf(err.message, sizeof(err.message), 
                "Failed to open %s: %s", filename, strerror(errno));
        return err;
    }
    
    // Process file...
    fclose(file);
    return (error_t)ERROR_NONE;
}
```

### Performance Optimization

```c
// Compiler hints for optimization
#define LIKELY(x)   __builtin_expect(!!(x), 1)
#define UNLIKELY(x) __builtin_expect(!!(x), 0)

// Hot path optimization
int process_data(const int* data, size_t size) {
    int sum = 0;
    
    // Tell compiler this is the common case
    if (LIKELY(data && size > 0)) {
        for (size_t i = 0; i < size; ++i) {
            sum += data[i];
        }
    }
    
    return sum;
}

// Memory alignment for SIMD
#define ALIGN_16 __attribute__((aligned(16)))

ALIGN_16 float vector_data[4] = {1.0f, 2.0f, 3.0f, 4.0f};
```