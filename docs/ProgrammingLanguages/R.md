R [https://www.r-project.org/]

Windows [https://cran.r-project.org/bin/windows/base/]
Mac [https://cran.r-project.org/bin/macosx/]
Linux [https://cran.r-project.org/bin/linux/]

Deploy Plumber to DigitalOcean [https://www.rplumber.io/]

Scientific Computing [https://www.opencpu.org/]

# R Programming Language Integration

## Overview

R is a powerful statistical computing language and environment that excels in data analysis, machine learning, and statistical modeling. In the Nexpo ecosystem, R serves as one of our polyglot API backends, providing advanced analytics capabilities and seamless integration with our microservices architecture.

**Key Features:**
- Statistical computing and data analysis
- Advanced machine learning capabilities
- Comprehensive package ecosystem (CRAN)
- Scientific computing and visualization
- Integration with modern web frameworks via Plumber

## Installation

### System Requirements

**Download R:**
- **Windows**: [CRAN Windows Binary](https://cran.r-project.org/bin/windows/base/)
- **macOS**: [CRAN macOS Binary](https://cran.r-project.org/bin/macosx/)
- **Linux**: [CRAN Linux Binary](https://cran.r-project.org/bin/linux/)

### Development Environment Setup

#### VS Code Configuration

1. **Install R Extension**:
   ```bash
   code --install-extension REditorSupport.r
   ```

2. **Install R Language Server**:
   ```r
   install.packages("languageserver")
   ```

3. **Configure VS Code Settings**:
   ```json
   {
     "r.rterm.windows": "C:\\Program Files\\R\\R-4.3.0\\bin\\R.exe",
     "r.rterm.mac": "/usr/local/bin/R",
     "r.rterm.linux": "/usr/bin/R",
     "r.lsp.enabled": true,
     "r.plot.useHttpgd": true
   }
   ```

#### RStudio Integration (Optional)

For enhanced R development experience:
```bash
# Download RStudio Desktop
wget https://download1.rstudio.org/electron/jammy/amd64/rstudio-2023.06.1-524-amd64.deb
```

## Nexpo R API Backend

### Architecture Overview

The Nexpo R API backend runs on **port 8080** using the Plumber framework, providing REST API endpoints for statistical computing and data analysis.

```
┌─────────────────┐    ┌──────────────┐    ┌────────────────┐
│   Kong Gateway  │────│  R Plumber   │────│   MindsDB      │
│   Port 8000     │    │  Port 8080   │    │   Gateway      │
└─────────────────┘    └──────────────┘    └────────────────┘
```

### Key Features

- **🔐 Auth0 JWT Authentication**: Secure API access with token validation
- **🤖 MindsDB Integration**: Direct access to AI/ML database capabilities
- **📊 Statistical Computing**: Advanced analytics and data processing
- **🔄 CORS Support**: Cross-origin resource sharing enabled
- **📝 Swagger Documentation**: Automatic API documentation generation
- **🚀 Docker Containerization**: Production-ready deployment
- **🔍 Health Monitoring**: Built-in health checks and status endpoints

### Installation & Setup

#### 1. Install Required Packages

```r
# Core packages for Nexpo integration
install.packages(c(
  "plumber",        # Web API framework
  "jose",           # JWT validation
  "httr",           # HTTP client for MindsDB
  "jsonlite",       # JSON processing
  "logger",         # Structured logging
  "dplyr",          # Data manipulation
  "ggplot2",        # Data visualization
  "stringr",        # String processing
  "lubridate",      # Date/time handling
  "DBI"             # Database interface
))
```

#### 2. Environment Configuration

```r
# .env.example configuration
ENABLE_API_R=true
R_API_PORT=8080
AUTH0_DOMAIN=your-domain.auth0.com
AUTH0_AUDIENCE=https://api.nexpo.dev
MINDSDB_URL=http://localhost:4040
R_MAX_VSIZE=8Gb
R_KEEP_PKG_SOURCE=yes
```

#### 3. Basic API Implementation

```r
#!/usr/bin/env Rscript

# Load required libraries
library(plumber)
library(jose)
library(httr)
library(jsonlite)
library(logger)
library(dplyr)

# JWT validation middleware
auth_middleware <- function(req, res) {
  auth_header <- req$HTTP_AUTHORIZATION
  
  if (is.null(auth_header)) {
    res$status <- 401
    return(list(error = "Authorization header required"))
  }
  
  token <- gsub("Bearer ", "", auth_header)
  
  tryCatch({
    # Validate JWT with Auth0
    jwks_url <- paste0(Sys.getenv("AUTH0_DOMAIN"), "/.well-known/jwks.json")
    jwt_decode_sig(token, jwks_url)
    
    # Extract user info from token
    claims <- jwt_decode_sig(token, jwks_url)
    req$user <- claims
    
    forward()
  }, error = function(e) {
    log_error("JWT validation failed: {e$message}")
    res$status <- 401
    return(list(error = "Invalid token"))
  })
}

# Health check endpoint
#* @get /health
function() {
  list(
    status = "healthy",
    timestamp = Sys.time(),
    version = R.version.string,
    uptime = proc.time()["elapsed"]
  )
}

# User profile endpoint (Auth required)
#* @get /api/user/profile
#* @preempt auth_middleware
function(req) {
  list(
    user_id = req$user$sub,
    email = req$user$email,
    name = req$user$name,
    platform = "R",
    capabilities = c("statistical_analysis", "machine_learning", "data_visualization")
  )
}

# Statistical analysis endpoint
#* @post /api/data/analyze
#* @preempt auth_middleware
function(req, data) {
  tryCatch({
    # Parse input data
    df <- data.frame(data)
    
    # Perform statistical analysis
    summary_stats <- df %>%
      summarise(
        across(where(is.numeric), list(
          mean = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          min = ~min(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE)
        ))
      )
    
    correlation_matrix <- cor(select_if(df, is.numeric), use = "complete.obs")
    
    list(
      summary = summary_stats,
      correlations = correlation_matrix,
      row_count = nrow(df),
      col_count = ncol(df)
    )
  }, error = function(e) {
    log_error("Analysis failed: {e$message}")
    list(error = e$message)
  })
}

# MindsDB query proxy
#* @get /api/data/query
#* @preempt auth_middleware
function(req, sql) {
  mindsdb_url <- Sys.getenv("MINDSDB_URL")
  
  tryCatch({
    response <- POST(
      paste0(mindsdb_url, "/api/sql/query"),
      body = list(query = sql),
      encode = "json",
      add_headers(
        "Content-Type" = "application/json"
      )
    )
    
    if (status_code(response) == 200) {
      content(response, "parsed")
    } else {
      list(error = "MindsDB query failed")
    }
  }, error = function(e) {
    log_error("MindsDB query failed: {e$message}")
    list(error = e$message)
  })
}
```

### Advanced Features

#### Machine Learning Integration

```r
# Machine learning model endpoint
#* @post /api/ml/predict
#* @preempt auth_middleware
function(req, features, model_type = "linear") {
  tryCatch({
    df <- data.frame(features)
    
    if (model_type == "linear") {
      # Linear regression example
      if (ncol(df) >= 2) {
        model <- lm(df[, 1] ~ ., data = df[, -1, drop = FALSE])
        predictions <- predict(model, df[, -1, drop = FALSE])
        
        list(
          predictions = predictions,
          r_squared = summary(model)$r.squared,
          coefficients = coef(model)
        )
      } else {
        list(error = "Insufficient features for linear regression")
      }
    } else if (model_type == "clustering") {
      # K-means clustering
      numeric_cols <- select_if(df, is.numeric)
      if (ncol(numeric_cols) >= 2) {
        kmeans_result <- kmeans(numeric_cols, centers = 3)
        
        list(
          clusters = kmeans_result$cluster,
          centers = kmeans_result$centers,
          within_ss = kmeans_result$withinss
        )
      } else {
        list(error = "Insufficient numeric features for clustering")
      }
    }
  }, error = function(e) {
    list(error = e$message)
  })
}
```

#### Data Visualization

```r
# Data visualization endpoint
#* @post /api/viz/plot
#* @preempt auth_middleware
#* @serializer png
function(req, data, plot_type = "scatter") {
  tryCatch({
    df <- data.frame(data)
    
    p <- switch(plot_type,
      "scatter" = ggplot(df, aes(x = df[,1], y = df[,2])) + 
                   geom_point() + 
                   theme_minimal(),
      "histogram" = ggplot(df, aes(x = df[,1])) + 
                     geom_histogram(bins = 30) + 
                     theme_minimal(),
      "boxplot" = ggplot(df, aes(y = df[,1])) + 
                   geom_boxplot() + 
                   theme_minimal()
    )
    
    print(p)
  }, error = function(e) {
    # Return error plot
    plot.new()
    text(0.5, 0.5, paste("Error:", e$message), cex = 1.2)
  })
}
```

### Docker Deployment

#### Dockerfile

```dockerfile
# Multi-stage build for R API
FROM r-base:4.3.0 as builder

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
COPY DESCRIPTION .
RUN R -e "install.packages(c('plumber', 'jose', 'httr', 'jsonlite', 'logger', 'dplyr', 'ggplot2'))"

# Production stage
FROM r-base:4.3.0

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Copy installed packages from builder
COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

# Create non-root user
RUN useradd -r -u 1001 ruser
USER ruser

# Copy application
WORKDIR /app
COPY --chown=ruser:ruser . .

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=40s --retries=3 \
  CMD curl -f http://localhost:8080/health || exit 1

# Start application
CMD ["Rscript", "app.R"]
```

### Testing & Development

#### Local Development

```bash
# Enable R backend
echo "ENABLE_API_R=true" >> .env

# Start development server
cd microservices/api/api-R
Rscript app.R

# Test endpoints
curl http://localhost:8080/health
curl -H "Authorization: Bearer $JWT_TOKEN" http://localhost:8080/api/user/profile
```

#### Unit Testing

```r
# Install testing packages
install.packages(c("testthat", "httptest"))

# Example test file: tests/test_api.R
library(testthat)
library(plumber)

test_that("Health endpoint works", {
  r <- plumb("app.R")
  res <- r$call(make_req("GET", "/health"))
  
  expect_equal(res$status, 200)
  expect_true("status" %in% names(res$body))
  expect_equal(res$body$status, "healthy")
})

test_that("Auth middleware blocks unauthorized requests", {
  r <- plumb("app.R")
  res <- r$call(make_req("GET", "/api/user/profile"))
  
  expect_equal(res$status, 401)
})
```

## Integration with Nexpo Ecosystem

### Kong API Gateway

The R API integrates seamlessly with Kong Gateway:

```yaml
# Kong service configuration
services:
  - name: r-api
    url: http://r-api:8080
    plugins:
      - name: jwt
        config:
          uri_param_names: ["token"]
          header_names: ["authorization"]
      - name: rate-limiting
        config:
          minute: 100
          hour: 1000
```

### MindsDB Integration

Direct integration with MindsDB for AI/ML capabilities:

```r
# Query MindsDB models
query_mindsdb_model <- function(model_name, input_data) {
  sql <- sprintf(
    "SELECT * FROM %s WHERE %s",
    model_name,
    paste(names(input_data), input_data, sep = " = ", collapse = " AND ")
  )
  
  mindsdb_query(sql)
}
```

### Auth0 Integration

Seamless authentication with Auth0:

```r
# Extract user information from JWT
get_user_info <- function(req) {
  list(
    user_id = req$user$sub,
    email = req$user$email,
    roles = req$user$`https://nexpo.dev/roles`,
    permissions = req$user$`https://nexpo.dev/permissions`
  )
}
```

## Best Practices

### Performance Optimization

```r
# Memory management
options(max.print = 1000)
memory.limit(size = 8000)  # 8GB limit

# Parallel processing
library(parallel)
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, ls())

# Cleanup function
cleanup <- function() {
  stopCluster(cl)
  gc()
}

on.exit(cleanup())
```

### Error Handling

```r
# Comprehensive error handling
safe_execute <- function(expr) {
  tryCatch({
    result <- expr
    list(success = TRUE, data = result)
  }, error = function(e) {
    log_error("Execution failed: {e$message}")
    list(success = FALSE, error = e$message)
  }, warning = function(w) {
    log_warn("Warning: {w$message}")
    list(success = TRUE, data = NULL, warning = w$message)
  })
}
```

### Logging Strategy

```r
# Structured logging configuration
library(logger)

# Set log level based on environment
if (Sys.getenv("NODE_ENV") == "production") {
  log_threshold(INFO)
} else {
  log_threshold(DEBUG)
}

# Custom log layout
log_layout(layout_json_serialize())

# Log to file in production
if (Sys.getenv("NODE_ENV") == "production") {
  log_appender(appender_file("/var/log/r-api/app.log"))
}
```

## Deployment Strategies

### Cloud Deployment Options

#### 1. **DigitalOcean App Platform**

```yaml
# .do/app.yaml
name: nexpo-r-api
services:
- name: r-api
  source_dir: /
  github:
    repo: your-org/nexpo
    branch: main
    deploy_on_push: true
  run_command: Rscript app.R
  environment_slug: ubuntu-20
  instance_count: 2
  instance_size_slug: basic-xxs
  envs:
  - key: AUTH0_DOMAIN
    value: your-domain.auth0.com
  - key: MINDSDB_URL
    value: http://mindsdb:4040
```

#### 2. **AWS Lambda with Custom Runtime**

```r
# AWS Lambda handler
handler <- function(event, context) {
  tryCatch({
    # Process Lambda event
    result <- process_request(event)
    
    list(
      statusCode = 200,
      headers = list("Content-Type" = "application/json"),
      body = jsonlite::toJSON(result, auto_unbox = TRUE)
    )
  }, error = function(e) {
    list(
      statusCode = 500,
      body = jsonlite::toJSON(list(error = e$message), auto_unbox = TRUE)
    )
  })
}
```

#### 3. **Azure Container Instances**

```yaml
# azure-deploy.yml
apiVersion: 2019-12-01
location: eastus
name: nexpo-r-api
properties:
  containers:
  - name: r-api
    properties:
      image: nexpo/r-api:latest
      resources:
        requests:
          cpu: 1
          memoryInGb: 2
      ports:
      - port: 8080
        protocol: TCP
  osType: Linux
  restartPolicy: Always
```

### Terraform Deployment

The R API includes comprehensive Terraform configuration for AWS ECS deployment:

```bash
# Deploy R API infrastructure
cd terraform/microservices/api-R
terraform init
terraform plan -var="enable_r_api=true"
terraform apply
```

## Monitoring & Observability

### Health Checks

```r
# Comprehensive health check
#* @get /health/detailed
function() {
  list(
    status = "healthy",
    timestamp = Sys.time(),
    version = list(
      r = R.version.string,
      plumber = packageVersion("plumber"),
      dependencies = sapply(c("jose", "httr", "dplyr"), packageVersion)
    ),
    memory = list(
      used = memory.size(),
      limit = memory.limit()
    ),
    system = list(
      cores = parallel::detectCores(),
      uptime = proc.time()["elapsed"]
    ),
    integrations = list(
      auth0 = check_auth0_connection(),
      mindsdb = check_mindsdb_connection()
    )
  )
}
```

### Prometheus Metrics

```r
# Metrics collection
library(prometheus)

# Define metrics
request_counter <- counter$new(
  "r_api_requests_total",
  "Total number of HTTP requests",
  c("method", "endpoint", "status")
)

request_duration <- histogram$new(
  "r_api_request_duration_seconds",
  "HTTP request duration in seconds",
  c("method", "endpoint")
)

# Metrics endpoint
#* @get /metrics
function() {
  prometheus::render_metrics()
}
```

## Scientific Computing Extensions

### Bioinformatics Integration

```r
# Bioconductor packages for genomics
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install(c("Biostrings", "GenomicRanges", "DESeq2"))

#* @post /api/bio/sequence-analysis
function(sequences) {
  library(Biostrings)
  
  dna_strings <- DNAStringSet(sequences)
  
  list(
    gc_content = letterFrequency(dna_strings, "GC", as.prob = TRUE),
    length = width(dna_strings),
    complement = as.character(complement(dna_strings))
  )
}
```

### Time Series Analysis

```r
# Time series forecasting endpoint
#* @post /api/timeseries/forecast
function(data, periods = 12) {
  library(forecast)
  
  ts_data <- ts(data$values, frequency = data$frequency %||% 12)
  model <- auto.arima(ts_data)
  forecast_result <- forecast(model, h = periods)
  
  list(
    forecast = as.numeric(forecast_result$mean),
    lower = as.numeric(forecast_result$lower[,2]),
    upper = as.numeric(forecast_result$upper[,2]),
    model_info = list(
      order = model$arma[c(1,6,2)],
      aic = model$aic,
      sigma2 = model$sigma2
    )
  )
}
```

## Resources & Learning

### Official Documentation

- **[R Project](https://www.r-project.org/)** - Official R language website
- **[CRAN](https://cran.r-project.org/)** - Comprehensive R Archive Network
- **[Plumber Documentation](https://www.rplumber.io/)** - R API framework documentation
- **[RStudio](https://www.rstudio.com/)** - Popular R IDE and tools

### Nexpo-Specific Resources

- **[Microservice Documentation](../../microservices/api/api-R/README.md)** - R API implementation guide
- **[Terraform Configuration](../../terraform/microservices/api-R/)** - Infrastructure deployment
- **[Docker Configuration](../../microservices/api/api-R/Dockerfile)** - Container setup
- **[Port Configuration](../Ports/Ports.md#r-plumber-api-port-8080)** - Network configuration

### Community & Support

- **[R-help Mailing List](https://stat.ethz.ch/mailman/listinfo/r-help)** - Community support
- **[Stack Overflow R Tag](https://stackoverflow.com/questions/tagged/r)** - Q&A community
- **[RWeekly](https://rweekly.org/)** - Weekly R news and tutorials
- **[R-bloggers](https://www.r-bloggers.com/)** - R community blog aggregator

### Advanced Topics

- **[Advanced R](https://adv-r.hadley.nz/)** - Deep dive into R programming
- **[R Packages](https://r-pkgs.org/)** - Package development guide
- **[Efficient R Programming](https://csgillespie.github.io/efficientR/)** - Performance optimization
- **[R for Data Science](https://r4ds.had.co.nz/)** - Data analysis workflows

## Conclusion

The R integration in Nexpo provides a powerful platform for statistical computing, data analysis, and machine learning within a modern microservices architecture. With Auth0 authentication, MindsDB integration, and comprehensive Docker deployment, R becomes a first-class citizen in the polyglot backend ecosystem.

**Key Benefits:**
- ✅ **Statistical Excellence**: World-class statistical computing capabilities
- ✅ **ML/AI Integration**: Seamless machine learning model deployment
- ✅ **Enterprise Security**: Auth0 JWT authentication and authorization
- ✅ **Scalable Architecture**: Container-based deployment with auto-scaling
- ✅ **Developer Experience**: VS Code integration and comprehensive tooling
- ✅ **Production Ready**: Health checks, logging, and monitoring built-in

The R backend complements other language services in Nexpo, providing specialized analytics capabilities while maintaining consistency with the overall architecture and development workflow.