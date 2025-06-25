# MindsDB Enterprise Setup with MCP Integration

This directory contains a production-ready MindsDB setup with Model Control Plane (MCP) integration, designed to work seamlessly with your multi-cloud Terraform infrastructure. The system automatically connects to cloud providers based on your configuration.

## 🌟 Key Features

- **Model Control Plane (MCP)** for managing ML models at scale
- **Multi-Cloud Support**: AWS, GCP, Azure, and OCI integrations
- **Production-Grade** with health checks, monitoring, and backups
- **Auth0 Integration** for secure authentication
- **Containerized** with Docker Compose for easy deployment
- **Infrastructure as Code** with Terraform integration
- **Frontend Integration** with Next.js and Expo clients

## 🚀 Quick Start

### Prerequisites
- Docker and Docker Compose
- Node.js 18+ (for management scripts)
- Cloud provider credentials (for enabled providers)

### Easy Setup (Recommended)

From the project root directory:

```bash
# Start MindsDB with automatic setup
npm run start:mindsdb

# View logs
npm run logs:mindsdb

# Stop MindsDB
npm run stop:mindsdb

# Full development environment (MindsDB + Frontend + Backend)
npm run dev:full
```

### Manual Setup

1. **Copy environment variables:**
   ```bash
   cp ../.env.example ../.env
   ```

2. **Configure MindsDB settings in .env:**
   ```bash
   # MindsDB Ports
   MINDSDB_HTTP_PORT=47334
   MINDSDB_MYSQL_PORT=47335
   MINDSDB_MONGODB_PORT=47336
   MINDSDB_MCP_PORT=5500
   
   # MindsDB API URL
   MINDSDB_API_URL=http://localhost:47334
   ```

3. **Start services:**
   ```bash
   docker-compose up -d
   ```

## 📊 Access Points

Once MindsDB is running, you can access it via:

- **🌐 Web UI**: http://localhost:47334
- **🔌 HTTP API**: http://localhost:47334/api
- **🐬 MySQL Interface**: localhost:47335
- **🍃 MongoDB Interface**: localhost:47336
- **⚙️ MCP Control**: localhost:5500

## 🏗️ Architecture Integration

### Frontend Integration

The MindsDB client is integrated into the shared utilities:

```typescript
import { useMindsDB } from '@shared/utils';

function MyComponent() {
  const { client, isConnected } = useMindsDB(authToken);
  
  const runQuery = async () => {
    const result = await client.query('SELECT * FROM my_model WHERE input = "data"');
    console.log(result);
  };
}
```

### CRA Token Integration

MindsDB integrates with the Central Rank Authority for federated queries:

```sql
-- Query Discord messages with sentiment analysis
SELECT 
  content,
  author,
  PREDICT(sentiment) as mood
FROM discord.messages 
WHERE guild_id = 'YOUR_GUILD_ID'
LIMIT 10;

-- GitHub issue analysis
SELECT 
  title,
  state,
  PREDICT(priority) as urgency_score
FROM github.issues
WHERE repository = 'YOUR_REPO';
```

## 🛠️ Model Control Plane (MCP)

The Model Control Plane provides a unified interface for managing ML models across all connected cloud providers.

### Key Features

- **Centralized Model Management**: Deploy and manage models across multiple cloud providers
- **Auto-Scaling**: Automatically scale model deployments based on demand
- **A/B Testing**: Easily test new model versions with traffic splitting
- **Monitoring**: Track model performance and health metrics
- **Versioning**: Maintain multiple versions of models with rollback support

### MCP Endpoints

- **Health Check**: `GET http://localhost:5500/health`
- **Model Status**: `GET http://localhost:5500/api/v1/models`
- **Deploy Model**: `POST http://localhost:5500/api/v1/models`
- **Predict**: `POST http://localhost:5500/api/v1/models/{model_id}/predict`

## ☁️ Cloud Provider Management

Easily enable and manage cloud providers using the provided Make commands:

### Enable a Cloud Provider

```bash
# Enable AWS provider
make enable-provider PROVIDER=aws

# Enable GCP provider
make enable-provider PROVIDER=gcp

# Enable Azure provider
make enable-provider PROVIDER=azure

# Enable OCI provider
make enable-provider PROVIDER=oci
```

### List Enabled Providers

```bash
make list-providers
```

### Required Credentials

Each cloud provider requires specific environment variables to be set in your `.env` file. See the `.env.example` file for all available options.

## 📊 Monitoring & Logs

### Service Health

```bash
# Check all services status
make status

# Check MCP health
make mcp-status
```

### View Logs

```bash
# View all logs
make logs

# View MCP-specific logs
make mcp-logs

# View database logs
docker-compose logs -f postgres

# View Redis logs
docker-compose logs -f redis
```

### Metrics & Monitoring

MindsDB and MCP expose Prometheus metrics endpoints:
- **MindsDB Metrics**: `http://localhost:47334/metrics`
- **MCP Metrics**: `http://localhost:5500/metrics`

## 🔄 Backup & Restore

### Create Backup

```bash
# Create a timestamped backup
make backup
```

### Restore from Backup

```bash
# List available backups
ls -l backups/

# Restore the latest backup
make restore
```

## 🛠️ Troubleshooting

### Common Issues

#### MCP Not Starting
1. Check if the MCP port (5500) is available
2. Verify PostgreSQL is running and accessible
3. Check MCP logs: `make mcp-logs`

#### Cloud Provider Connection Issues
1. Verify credentials in `.env`
2. Check network connectivity to the cloud provider
3. Verify IAM permissions for the service account

#### Database Connection Issues
1. Check if PostgreSQL is running: `docker-compose ps postgres`
2. Verify credentials in `.env`
3. Check logs: `docker-compose logs postgres`

### Getting Help

- [MindsDB Documentation](https://docs.mindsdb.com/)
- [MindsDB Community Slack](https://join.slack.com/t/mindsdbcommunity/shared_invite/zt-o8grmz5y-5DGoqwMrzFSoJ0Ab0Z5zCA)
- [GitHub Issues](https://github.com/mindsdb/mindsdb/issues)

## 📝 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- [MindsDB](https://mindsdb.com/) - For the amazing open-source ML framework
- [Docker](https://www.docker.com/) - For containerization
- [Terraform](https://www.terraform.io/) - For infrastructure as code
- [Auth0](https://auth0.com/) - For authentication

## Cloud Provider Integration

MindsDB automatically detects which cloud providers are enabled in your Terraform configuration and creates appropriate datasources.

### AWS Integration
When `enable_aws = true`:
- S3 bucket access for data storage
- RDS/Aurora database connections
- Redshift data warehouse integration
- Athena for serverless queries

### GCP Integration
When `enable_gcp = true`:
- BigQuery for analytics
- Cloud Storage for data
- Cloud SQL connections
- Vertex AI integration

### Azure Integration
When `enable_azure = true`:
- Blob Storage access
- Azure SQL Database
- Cosmos DB connections
- Azure ML integration

### OCI Integration
When `enable_oci = true`:
- Object Storage access
- Autonomous Database
- MySQL HeatWave
- OCI Data Science

## Configuration

### Environment Variables

Create a `.env` file based on `.env.example`:

```bash
# Only configure providers that are enabled in Terraform

# AWS (if enable_aws = true)
AWS_ACCESS_KEY_ID=your-key
AWS_SECRET_ACCESS_KEY=your-secret
AWS_DEFAULT_REGION=us-east-1

# GCP (if enable_gcp = true)
GCP_PROJECT_ID=your-project
# Place service account JSON in config/gcp-credentials.json

# Azure (if enable_azure = true)
AZURE_TENANT_ID=your-tenant
AZURE_CLIENT_ID=your-client
AZURE_CLIENT_SECRET=your-secret

# OCI (if enable_oci = true)
OCI_USER_OCID=ocid1.user.oc1..xxxxx
OCI_TENANCY_OCID=ocid1.tenancy.oc1..xxxxx
# Place private key in config/oci-key.pem
```

## Usage Examples

### Creating a Datasource

```sql
-- Connect to S3 bucket (AWS)
CREATE DATABASE s3_datasource
WITH ENGINE = 's3',
PARAMETERS = {
  "aws_access_key_id": "your-key",
  "aws_secret_access_key": "your-secret",
  "bucket": "my-data-bucket"
};

-- Connect to BigQuery (GCP)
CREATE DATABASE bq_datasource
WITH ENGINE = 'bigquery',
PARAMETERS = {
  "project_id": "my-project",
  "dataset": "my_dataset"
};
```

### Creating ML Models

```sql
-- Create a predictive model
CREATE MODEL sales_predictor
FROM s3_datasource
  (SELECT * FROM sales_data.csv)
PREDICT revenue
USING ENGINE = 'lightwood';

-- Make predictions
SELECT revenue, revenue_predict
FROM sales_predictor
WHERE product = 'Widget A'
  AND region = 'North America';
```

## Commands

```bash
# Start services
make up

# Stop services
make down

# View logs
make logs

# Check status
make status

# Initialize cloud datasources
make init

# Backup data
make backup

# Restore data
make restore

# Connect via MySQL client
make mysql-client

# Update MindsDB
make update
```

## Monitoring

MindsDB provides built-in monitoring:

1. **Web UI Dashboard**: http://localhost:47334
2. **API Health Check**: http://localhost:47334/api/util/ping
3. **Logs**: `make logs`

## Troubleshooting

### MindsDB won't start
```bash
# Check Docker logs
make logs

# Ensure Docker is running
docker info

# Check port availability
netstat -an | grep 47334
```

### Cloud provider connection issues
```bash
# Test AWS connection
make check-aws

# Test GCP connection
make check-gcp

# Test Azure connection
make check-azure

# Test OCI connection
make check-oci
```

### Database connection failures
- Verify credentials in `.env`
- Check network connectivity
- Ensure firewall rules allow access
- Verify service account permissions

## Security Best Practices

1. **Never commit `.env` or credentials to git**
2. **Use service accounts with minimal permissions**
3. **Rotate credentials regularly**
4. **Enable encryption for data at rest**
5. **Use VPN/private endpoints when possible**

## Integration with CI/CD

```yaml
# GitHub Actions example
- name: Start MindsDB
  run: |
    docker-compose up -d
    sleep 30  # Wait for startup
    
- name: Run ML pipeline
  run: |
    python scripts/train_models.py
    
- name: Deploy models
  run: |
    python scripts/deploy_models.py
```

## Next Steps

1. **Create datasources** for your cloud data
2. **Build ML models** using SQL
3. **Integrate predictions** into your app
4. **Monitor performance** and costs
5. **Scale as needed** with cloud resources

## Support

- MindsDB Docs: https://docs.mindsdb.com
- Community: https://community.mindsdb.com
- GitHub: https://github.com/mindsdb/mindsdb
