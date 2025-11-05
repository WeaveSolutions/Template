# Multi-stage build for Julia Genie API
FROM julia:1.9-bullseye AS builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy project files
COPY Project.toml ./
COPY Manifest.toml* ./

# Install Julia packages
RUN julia -e 'using Pkg; Pkg.instantiate()'

# Production stage
FROM julia:1.9-bullseye

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -s /bin/bash apiuser

# Set working directory
WORKDIR /app

# Copy Julia depot from builder
COPY --from=builder /root/.julia /home/apiuser/.julia

# Copy application files
COPY . .

# Change ownership
RUN chown -R apiuser:apiuser /app /home/apiuser/.julia

# Switch to non-root user
USER apiuser

# Set Julia depot path
ENV JULIA_DEPOT_PATH=/home/apiuser/.julia

# Expose port
EXPOSE 8090

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8090/health || exit 1

# Set environment variables
ENV JULIA_API_PORT=8090
ENV LOG_LEVEL=INFO

# Run the API
CMD ["julia", "app.jl"]
