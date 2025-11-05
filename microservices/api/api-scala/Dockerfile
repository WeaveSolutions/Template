# Multi-stage Dockerfile for Scala Play Framework

# Stage 1: Build
FROM hseeberger/scala-sbt:11.0.15_1.7.1_2.13.8 AS builder

WORKDIR /app

# Copy project files
COPY project ./project
COPY build.sbt ./

# Download dependencies
RUN sbt update

# Copy source code
COPY app ./app
COPY conf ./conf
COPY public ./public

# Build the application
RUN sbt dist

# Extract the built package
RUN unzip target/universal/*.zip -d /tmp && \
    mv /tmp/nexpo-api-scala-* /tmp/app

# Stage 2: Runtime
FROM openjdk:11-jre-slim

# Install required packages
RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    curl \
    ca-certificates && \
    rm -rf /var/lib/apt/lists/*

# Create non-root user
RUN groupadd -r scala && useradd -r -g scala scala

WORKDIR /app

# Copy built application from builder
COPY --from=builder --chown=scala:scala /tmp/app /app

# Set environment variables
ENV PLAY_HTTP_SECRET_KEY="changeme"
ENV APPLICATION_SECRET="changeme"
ENV SCALA_API_PORT=8060

# Expose port
EXPOSE 8060

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
    CMD curl -f http://localhost:8060/health || exit 1

# Switch to non-root user
USER scala

# Start the application
ENTRYPOINT ["/app/bin/nexpo-api-scala"]
CMD ["-Dplay.http.secret.key=${PLAY_HTTP_SECRET_KEY}", "-Dhttp.port=${SCALA_API_PORT}"]
