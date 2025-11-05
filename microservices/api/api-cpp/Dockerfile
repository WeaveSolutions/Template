# Multi-stage Dockerfile for C++ Drogon API Service

# Build stage
FROM ubuntu:22.04 AS builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    cmake \
    git \
    libssl-dev \
    zlib1g-dev \
    libbrotli-dev \
    libhiredis-dev \
    libpq-dev \
    libjsoncpp-dev \
    uuid-dev \
    libsqlite3-dev \
    libmysqlclient-dev \
    curl \
    wget \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Install Drogon framework
RUN git clone https://github.com/drogonframework/drogon.git && \
    cd drogon && \
    git checkout v1.8.7 && \
    mkdir build && \
    cd build && \
    cmake .. -DCMAKE_BUILD_TYPE=Release && \
    make -j$(nproc) && \
    make install && \
    cd /app && \
    rm -rf drogon

# Copy source code
COPY src/ ./src/
COPY include/ ./include/
COPY CMakeLists.txt ./
COPY config.json ./

# Build the application
RUN mkdir build && \
    cd build && \
    cmake .. -DCMAKE_BUILD_TYPE=Release && \
    make -j$(nproc)

# Runtime stage
FROM ubuntu:22.04

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libssl3 \
    zlib1g \
    libbrotli1 \
    libhiredis0.14 \
    libpq5 \
    libjsoncpp25 \
    libuuid1 \
    libsqlite3-0 \
    libmysqlclient21 \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN groupadd -r nexpo && useradd -r -g nexpo nexpo

# Set working directory
WORKDIR /app

# Copy built application from builder stage
COPY --from=builder /app/build/cpp_api ./
COPY --from=builder /app/config.json ./
COPY --from=builder /usr/local/lib/libdrogon.so* /usr/local/lib/

# Create necessary directories
RUN mkdir -p logs uploads temp && \
    chown -R nexpo:nexpo /app

# Update library path
RUN ldconfig

# Switch to non-root user
USER nexpo

# Expose port
EXPOSE 8110

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8110/health || exit 1

# Set environment variables
ENV NODE_ENV=production
ENV PORT=8110
ENV SERVICE_NAME=cpp-api

# Start the application
CMD ["./cpp_api"]
