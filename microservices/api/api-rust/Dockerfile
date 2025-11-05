# Multi-stage build for production-ready Rust Actix
FROM rust:1.77-alpine AS builder

# Install build dependencies
RUN apk add --no-cache musl-dev openssl-dev

# Create app directory
WORKDIR /app

# Copy manifest files
COPY Cargo.toml Cargo.lock ./

# Build dependencies (cache layer)
RUN mkdir src && \
    echo "fn main() {}" > src/main.rs && \
    cargo build --release && \
    rm -rf src

# Copy source code
COPY src ./src

# Build the application
RUN cargo build --release

# Production stage
FROM alpine:latest

# Install runtime dependencies
RUN apk --no-cache add ca-certificates libgcc

# Create non-root user
RUN adduser -D -u 1000 actix

# Set working directory
WORKDIR /app

# Copy binary from builder
COPY --from=builder --chown=actix:actix /app/target/release/nexpo-api-rust .

# Switch to non-root user
USER actix

# Expose port
EXPOSE 8050

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=40s --retries=3 \
    CMD wget --no-verbose --tries=1 --spider http://localhost:8050/health || exit 1

# Run the application
CMD ["./nexpo-api-rust"]
