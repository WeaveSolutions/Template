# Build stage
FROM hseeberger/scala-sbt:17.0.2_1.6.2_3.1.1 AS builder

WORKDIR /build

# Copy build files
COPY build.sbt ./
COPY project ./project

# Download dependencies
RUN sbt update

# Copy source code
COPY app ./app
COPY conf ./conf

# Build the application
RUN sbt dist

# Extract the built application
RUN unzip -o target/universal/*.zip -d target/universal && \
    rm target/universal/*.zip && \
    mv target/universal/* target/app

# Runtime stage
FROM eclipse-temurin:17-jre-alpine

# Install required packages
RUN apk add --no-cache bash curl

# Create non-root user
RUN addgroup -g 1001 -S play && \
    adduser -u 1001 -S play -G play

WORKDIR /app

# Copy built application
COPY --from=builder --chown=play:play /build/target/app /app

# Set environment variables
ENV JAVA_API_PORT=8070
ENV PLAY_HTTP_SECRET_KEY=changeme
ENV AUTH0_DOMAIN=""
ENV AUTH0_AUDIENCE=""
ENV AUTH0_CLIENT_ID=""
ENV AUTH0_CLIENT_SECRET=""
ENV MINDSDB_URL="http://mindsdb:47334"
ENV MINDSDB_TIMEOUT="30s"
ENV CORS_ORIGINS="http://localhost:3000"
ENV LOG_LEVEL="INFO"

# Expose port
EXPOSE 8070

# Switch to non-root user
USER play

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=40s --retries=3 \
    CMD curl -f http://localhost:8070/health || exit 1

# Start the application
ENTRYPOINT ["bin/api-java"]
CMD ["-Dplay.http.secret.key=${PLAY_HTTP_SECRET_KEY}", "-Dhttp.port=${JAVA_API_PORT}"]
