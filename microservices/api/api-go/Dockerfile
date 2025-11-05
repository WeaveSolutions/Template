# Multi-stage build for production-ready Go Beego
FROM golang:1.21-alpine AS builder

# Install build dependencies
RUN apk add --no-cache git gcc musl-dev

# Set working directory
WORKDIR /app

# Copy go mod files
COPY go.mod go.sum ./

# Download dependencies
RUN go mod download

# Copy source code
COPY . .

# Generate Swagger docs
RUN go install github.com/swaggo/swag/cmd/swag@latest
RUN swag init

# Build the application
RUN CGO_ENABLED=0 GOOS=linux go build -a -installsuffix cgo -o main .

# Production stage
FROM alpine:latest

# Install runtime dependencies
RUN apk --no-cache add ca-certificates curl

# Create non-root user
RUN adduser -D -u 1000 beego

# Set working directory
WORKDIR /app

# Copy binary from builder
COPY --from=builder --chown=beego:beego /app/main .
COPY --from=builder --chown=beego:beego /app/swagger ./swagger

# Switch to non-root user
USER beego

# Expose port
EXPOSE 8040

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=40s --retries=3 \
    CMD curl -f http://localhost:8040/health || exit 1

# Run the application
CMD ["./main"]
