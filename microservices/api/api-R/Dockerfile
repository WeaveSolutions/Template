# Multi-stage build for R Plumber API
FROM rocker/r-ver:4.3.2 AS builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy R package requirements
COPY renv.lock* ./
COPY .Rprofile* ./

# Install R packages
RUN R -e "install.packages('remotes')" && \
    R -e "remotes::install_version('plumber', version = '1.2.1')" && \
    R -e "remotes::install_version('jose', version = '1.2.0')" && \
    R -e "remotes::install_version('httr', version = '1.4.7')" && \
    R -e "remotes::install_version('jsonlite', version = '1.8.8')" && \
    R -e "remotes::install_version('logger', version = '0.3.0')"

# Production stage
FROM rocker/r-ver:4.3.2

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN useradd -m -s /bin/bash apiuser

# Set working directory
WORKDIR /app

# Copy installed R packages from builder
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Copy application files
COPY api.R ./
COPY .env.example .env

# Change ownership
RUN chown -R apiuser:apiuser /app

# Switch to non-root user
USER apiuser

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Set environment variables
ENV R_API_PORT=8080
ENV LOG_LEVEL=INFO

# Run the API
CMD ["Rscript", "api.R"]
