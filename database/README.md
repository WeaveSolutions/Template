# Weave BI Dashboard - Database Setup

This directory contains all database-related files for the Weave Business Intelligence Dashboard, including schema migrations, seed data, and setup scripts.

## 📋 Table of Contents

- [Quick Start](#quick-start)
- [Database Structure](#database-structure)
- [Prerequisites](#prerequisites)
- [Installation](#installation)
- [Configuration](#configuration)
- [Looker Studio Integration](#looker-studio-integration)
- [Troubleshooting](#troubleshooting)

---

## 🚀 Quick Start

### Windows
```cmd
cd database
setup.bat
```

### Linux/Mac
```bash
cd database
chmod +x setup.sh
./setup.sh
```

This will:
1. Create the `weave_bi` database
2. Set up the `weave_admin` user
3. Run database migrations (create tables, indexes, views)
4. Optionally seed mock data for testing

---

## 📊 Database Structure

The BI Dashboard uses **PostgreSQL** with the following tables:

### 1. `kpi_metrics`
Stores daily snapshots of key performance indicators.

**Key Fields:**
- Revenue: `mrr`, `arr`, `arpu`
- Users: `total_users`, `active_users`, `dau`, `mau`
- Financial: `ltv`, `cac`, `ltv_cac_ratio`
- Technical: `uptime`, `avg_load_time`, `error_rate`

### 2. `revenue_trends`
Tracks MRR and ARR over time for trend analysis.

**Key Fields:**
- `period`: Time label (e.g., 'Jan', '2024')
- `metric_type`: 'MRR' or 'ARR'
- `value`: Revenue amount

### 3. `user_growth`
User metrics with platform breakdown (mobile, desktop, website).

**Key Fields:**
- `metric_type`: 'MAU' or 'DAU'
- `dau`, `mau`: Aggregate metrics
- `mobile`, `desktop`, `website`: Platform breakdown

### 4. `subscription_breakdown`
Current distribution of subscription plans.

**Key Fields:**
- `plan_name`: 'Free', 'Pro', or 'Enterprise'
- `subscriber_count`: Number of subscribers
- `percentage`: Percentage of total

---

## ✅ Prerequisites

### 1. PostgreSQL Installation

**Windows:**
Download and install from [postgresql.org](https://www.postgresql.org/download/windows/)

**Mac:**
```bash
brew install postgresql@15
brew services start postgresql@15
```

**Linux (Ubuntu/Debian):**
```bash
sudo apt update
sudo apt install postgresql postgresql-contrib
sudo systemctl start postgresql
```

### 2. Verify Installation
```bash
psql --version
# Should output: psql (PostgreSQL) 15.x or higher
```

---

## 🔧 Installation

### Step 1: Clone and Navigate
```bash
cd E:\Workspace\Templates\Weave\database
```

### Step 2: Set Environment Variables (Optional)

Create a `.env` file or set in your shell:

```bash
# Windows (PowerShell)
$env:DB_HOST="localhost"
$env:DB_PORT="5432"
$env:DB_NAME="weave_bi"
$env:DB_USER="weave_admin"
$env:DB_PASSWORD="weave_secure_pass_2024"

# Linux/Mac (Bash)
export DB_HOST=localhost
export DB_PORT=5432
export DB_NAME=weave_bi
export DB_USER=weave_admin
export DB_PASSWORD=weave_secure_pass_2024
```

### Step 3: Run Setup Script

**Windows:**
```cmd
setup.bat
```

**Linux/Mac:**
```bash
chmod +x setup.sh
./setup.sh
```

### Step 4: Verify Setup
```bash
psql -h localhost -U weave_admin -d weave_bi -c "SELECT COUNT(*) FROM kpi_metrics;"
```

---

## ⚙️ Configuration

### Connection Details

After running the setup script, your database will be configured with:

| Parameter | Value |
|-----------|-------|
| **Hostname/IP** | `localhost` (or `127.0.0.1`) |
| **Port** | `5432` (default PostgreSQL port) |
| **Database** | `weave_bi` |
| **Username** | `weave_admin` |
| **Password** | `weave_secure_pass_2024` |

### Connection String
```
postgresql://weave_admin:weave_secure_pass_2024@localhost:5432/weave_bi
```

### Environment Variables

Update your `.env` file in the project root:

```bash
# BI Dashboard Database
DB_HOST=localhost
DB_PORT=5432
DB_NAME=weave_bi
DB_USER=weave_admin
DB_PASSWORD=weave_secure_pass_2024

# Connection Pool
DB_MAX_CONNECTIONS=20
DB_IDLE_TIMEOUT_MS=30000
DB_CONNECTION_TIMEOUT_MS=2000
DB_SSL=false
```

---

## 📈 Looker Studio Integration

### Step 1: Connect Looker Studio to PostgreSQL

1. **Open Looker Studio**: https://lookerstudio.google.com
2. **Create a new Data Source**:
   - Click **"Create"** → **"Data Source"**
   - Select **"PostgreSQL"** from the connectors list
3. **Enter Connection Details**:
   - **Host**: `localhost` (or your server IP if remote)
   - **Port**: `5432`
   - **Database**: `weave_bi`
   - **Username**: `weave_admin`
   - **Password**: `weave_secure_pass_2024`
4. **Enable SSL** if connecting remotely (recommended for production)

### Step 2: Create Separate Reports

Create **4 dedicated reports** for the dashboard toggles:

#### 1. MRR Report
- **Data Source**: `revenue_trends` table
- **Filter**: `metric_type = 'MRR'`
- **Chart Type**: Line chart
- **X-Axis**: `period_date`
- **Y-Axis**: `value`
- **Time Range**: Last 6-12 months

#### 2. ARR Report
- **Data Source**: `revenue_trends` table
- **Filter**: `metric_type = 'ARR'`
- **Chart Type**: Line chart
- **X-Axis**: `period_date` (yearly)
- **Y-Axis**: `value`
- **Time Range**: Last 5-10 years

#### 3. MAU Report
- **Data Source**: `user_growth` table
- **Filter**: `metric_type = 'MAU'`
- **Chart Types**: 
  - Line chart for total MAU
  - Stacked area chart for platform breakdown (mobile, desktop, website)
- **X-Axis**: `period_date`
- **Y-Axis**: `mau`, platform fields
- **Time Range**: Last 6-12 months

#### 4. DAU Report
- **Data Source**: `user_growth` table
- **Filter**: `metric_type = 'DAU'`
- **Chart Types**:
  - Line chart for daily users
  - Stacked area chart for platform breakdown
- **X-Axis**: `period_date`
- **Y-Axis**: `dau`, platform fields
- **Time Range**: Last 30 days

### Step 3: Enable Embedding

For each report:
1. Click **Share** → **Embed report**
2. Copy the embed URL (format: `https://lookerstudio.google.com/embed/reporting/REPORT_ID/page/PAGE_ID`)
3. Add to your `.env` file:

```bash
# Looker Studio Embed URLs
LOOKER_MRR_URL=https://lookerstudio.google.com/embed/reporting/YOUR_MRR_REPORT_ID
LOOKER_ARR_URL=https://lookerstudio.google.com/embed/reporting/YOUR_ARR_REPORT_ID
LOOKER_MAU_URL=https://lookerstudio.google.com/embed/reporting/YOUR_MAU_REPORT_ID
LOOKER_DAU_URL=https://lookerstudio.google.com/embed/reporting/YOUR_DAU_REPORT_ID
LOOKER_STUDIO_URL=https://lookerstudio.google.com
```

### Step 4: Update Frontend Configuration

The frontend is already configured to read these URLs from environment variables via `packages/shared-bi-core/src/config/index.ts`.

Simply restart your development server after updating `.env`:

```bash
npm run dev
# or
bun run dev
```

---

## 🐛 Troubleshooting

### Issue: "psql: command not found"

**Solution**: Add PostgreSQL to your system PATH.

**Windows:**
1. Find PostgreSQL installation (usually `C:\Program Files\PostgreSQL\15\bin`)
2. Add to System Environment Variables → Path
3. Restart terminal

**Mac:**
```bash
echo 'export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"' >> ~/.zshrc
source ~/.zshrc
```

### Issue: "FATAL: password authentication failed"

**Solution**: Reset the password or update your configuration.

```sql
-- Connect as postgres superuser
psql -U postgres

-- Reset password
ALTER USER weave_admin WITH PASSWORD 'weave_secure_pass_2024';
```

### Issue: "FATAL: database \"weave_bi\" does not exist"

**Solution**: Create the database manually.

```sql
psql -U postgres -c "CREATE DATABASE weave_bi;"
psql -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE weave_bi TO weave_admin;"
```

### Issue: Connection refused on port 5432

**Solution**: Ensure PostgreSQL is running.

**Windows:**
```cmd
services.msc
# Find "postgresql-x64-15" and start it
```

**Mac:**
```bash
brew services start postgresql@15
```

**Linux:**
```bash
sudo systemctl start postgresql
```

### Issue: Looker Studio can't connect

**Solutions:**
1. **Firewall**: Ensure port 5432 is open
2. **PostgreSQL Config**: Edit `postgresql.conf` to allow external connections:
   ```
   listen_addresses = '*'
   ```
3. **pg_hba.conf**: Add Looker Studio IP ranges (or use `0.0.0.0/0` for testing only)
4. **SSL**: Enable SSL for remote connections in production

---

## 📚 Additional Resources

- **Full Documentation**: See [MockData.md](../docs/Dashboard/MockData.md)
- **PostgreSQL Docs**: https://www.postgresql.org/docs/
- **Looker Studio Help**: https://support.google.com/looker-studio

---

## 🔒 Security Notes

⚠️ **Important for Production:**

1. **Change default password** immediately
2. **Enable SSL** for all connections
3. **Use environment variables** (never hardcode credentials)
4. **Restrict database access** to specific IPs
5. **Regular backups** (daily recommended)
6. **Monitor for unauthorized access**
7. **Use strong passwords** (min 16 characters, mixed case, numbers, symbols)

---

## 📞 Support

For issues or questions:
- Check [Troubleshooting](#troubleshooting) section
- Review [MockData.md](../docs/Dashboard/MockData.md)
- Contact your development team

---

**Last Updated**: November 9, 2024  
**Database Version**: PostgreSQL 15+  
**Schema Version**: 1.0.0
