#!/bin/bash

# Weave BI Dashboard - Database Setup Script
# This script initializes the PostgreSQL database with schema and seed data
# Usage: ./setup.sh [options]

set -e  # Exit on error

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Default values
DB_HOST="${DB_HOST:-localhost}"
DB_PORT="${DB_PORT:-5432}"
DB_NAME="${DB_NAME:-weave_bi}"
DB_USER="${DB_USER:-weave_admin}"
DB_PASSWORD="${DB_PASSWORD:-weave_secure_pass_2024}"

# Function to print colored output
print_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to check if PostgreSQL is installed
check_postgres() {
    if ! command -v psql &> /dev/null; then
        print_error "PostgreSQL client (psql) is not installed."
        print_info "Please install PostgreSQL: https://www.postgresql.org/download/"
        exit 1
    fi
    print_info "PostgreSQL client found ✓"
}

# Function to check if database exists
check_database() {
    PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -lqt | cut -d \| -f 1 | grep -qw $DB_NAME
    return $?
}

# Function to create database
create_database() {
    print_info "Creating database '$DB_NAME'..."
    PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U postgres -c "CREATE DATABASE $DB_NAME;"
    
    if [ $? -eq 0 ]; then
        print_info "Database created successfully ✓"
    else
        print_error "Failed to create database. You may need superuser privileges."
        print_info "Try running: sudo -u postgres psql -c \"CREATE DATABASE $DB_NAME;\""
        exit 1
    fi
}

# Function to create user if not exists
create_user() {
    print_info "Creating database user '$DB_USER'..."
    PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U postgres -c "
        DO \$\$
        BEGIN
            IF NOT EXISTS (SELECT FROM pg_catalog.pg_user WHERE usename = '$DB_USER') THEN
                CREATE USER $DB_USER WITH PASSWORD '$DB_PASSWORD';
            END IF;
        END
        \$\$;
    "
    
    if [ $? -eq 0 ]; then
        print_info "User setup completed ✓"
    else
        print_warning "Could not create user (may already exist or need superuser privileges)"
    fi
}

# Function to grant privileges
grant_privileges() {
    print_info "Granting privileges to '$DB_USER'..."
    PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U postgres -c "
        GRANT ALL PRIVILEGES ON DATABASE $DB_NAME TO $DB_USER;
        ALTER DATABASE $DB_NAME OWNER TO $DB_USER;
    "
    
    if [ $? -eq 0 ]; then
        print_info "Privileges granted ✓"
    else
        print_warning "Could not grant privileges (may need superuser privileges)"
    fi
}

# Function to run migration
run_migration() {
    print_info "Running database migration..."
    PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -f migrations/001_initial_schema.sql
    
    if [ $? -eq 0 ]; then
        print_info "Migration completed successfully ✓"
    else
        print_error "Migration failed!"
        exit 1
    fi
}

# Function to seed data
seed_data() {
    print_info "Seeding mock data..."
    PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -f seeds/seed_mock_data.sql
    
    if [ $? -eq 0 ]; then
        print_info "Data seeded successfully ✓"
    else
        print_error "Seeding failed!"
        exit 1
    fi
}

# Function to verify setup
verify_setup() {
    print_info "Verifying database setup..."
    PGPASSWORD=$DB_PASSWORD psql -h $DB_HOST -p $DB_PORT -U $DB_USER -d $DB_NAME -c "
        SELECT 'Tables created:' AS status, COUNT(*) AS count 
        FROM information_schema.tables 
        WHERE table_schema = 'public';
    "
    
    if [ $? -eq 0 ]; then
        print_info "Verification completed ✓"
    else
        print_warning "Could not verify setup"
    fi
}

# Main setup process
main() {
    echo "========================================="
    echo "  Weave BI Dashboard - Database Setup  "
    echo "========================================="
    echo ""
    
    print_info "Configuration:"
    echo "  Host:     $DB_HOST"
    echo "  Port:     $DB_PORT"
    echo "  Database: $DB_NAME"
    echo "  User:     $DB_USER"
    echo ""
    
    # Check prerequisites
    check_postgres
    
    # Create database if it doesn't exist
    if check_database; then
        print_warning "Database '$DB_NAME' already exists. Skipping creation."
    else
        create_database
    fi
    
    # Create user and grant privileges
    create_user
    grant_privileges
    
    # Run migration
    run_migration
    
    # Seed data
    print_info "Do you want to seed mock data? (y/n)"
    read -r response
    if [[ "$response" =~ ^([yY][eE][sS]|[yY])$ ]]; then
        seed_data
    else
        print_info "Skipping data seeding."
    fi
    
    # Verify setup
    verify_setup
    
    echo ""
    print_info "========================================="
    print_info "  Database setup completed successfully! "
    print_info "========================================="
    echo ""
    print_info "Connection string:"
    echo "  postgresql://$DB_USER:****@$DB_HOST:$DB_PORT/$DB_NAME"
    echo ""
    print_info "Next steps:"
    echo "  1. Update your .env file with the database credentials"
    echo "  2. Configure Looker Studio to connect to this database"
    echo "  3. Create Looker Studio reports for MRR, ARR, MAU, and DAU"
    echo "  4. Update environment variables with Looker Studio embed URLs"
    echo ""
}

# Run main function
main
