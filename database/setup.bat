@echo off
REM Weave BI Dashboard - Database Setup Script (Windows)
REM This script initializes the PostgreSQL database with schema and seed data
REM Usage: setup.bat

setlocal EnableDelayedExpansion

REM Default values (override with environment variables)
if not defined DB_HOST set DB_HOST=localhost
if not defined DB_PORT set DB_PORT=5432
if not defined DB_NAME set DB_NAME=weave_bi
if not defined DB_USER set DB_USER=weave_admin
if not defined DB_PASSWORD set DB_PASSWORD=weave_secure_pass_2024

echo =========================================
echo   Weave BI Dashboard - Database Setup
echo =========================================
echo.
echo Configuration:
echo   Host:     %DB_HOST%
echo   Port:     %DB_PORT%
echo   Database: %DB_NAME%
echo   User:     %DB_USER%
echo.

REM Check if psql is available
where psql >nul 2>nul
if %ERRORLEVEL% NEQ 0 (
    echo [ERROR] PostgreSQL client (psql) not found!
    echo Please install PostgreSQL and add it to your PATH
    echo Download from: https://www.postgresql.org/download/windows/
    pause
    exit /b 1
)
echo [INFO] PostgreSQL client found
echo.

REM Check if database exists
set PGPASSWORD=%DB_PASSWORD%
psql -h %DB_HOST% -p %DB_PORT% -U %DB_USER% -lqt | findstr /C:"%DB_NAME%" >nul 2>nul
if %ERRORLEVEL% EQU 0 (
    echo [WARNING] Database '%DB_NAME%' already exists
    echo.
) else (
    echo [INFO] Creating database '%DB_NAME%'...
    psql -h %DB_HOST% -p %DB_PORT% -U postgres -c "CREATE DATABASE %DB_NAME%;"
    if %ERRORLEVEL% EQU 0 (
        echo [INFO] Database created successfully
    ) else (
        echo [ERROR] Failed to create database
        echo You may need to run this as administrator or create the database manually
        pause
        exit /b 1
    )
)

REM Create user if not exists
echo [INFO] Setting up database user...
psql -h %DB_HOST% -p %DB_PORT% -U postgres -c "DO $$ BEGIN IF NOT EXISTS (SELECT FROM pg_catalog.pg_user WHERE usename = '%DB_USER%') THEN CREATE USER %DB_USER% WITH PASSWORD '%DB_PASSWORD%'; END IF; END $$;"
if %ERRORLEVEL% EQU 0 (
    echo [INFO] User setup completed
) else (
    echo [WARNING] Could not create user (may already exist)
)

REM Grant privileges
echo [INFO] Granting privileges...
psql -h %DB_HOST% -p %DB_PORT% -U postgres -c "GRANT ALL PRIVILEGES ON DATABASE %DB_NAME% TO %DB_USER%; ALTER DATABASE %DB_NAME% OWNER TO %DB_USER%;"

REM Run migration
echo.
echo [INFO] Running database migration...
psql -h %DB_HOST% -p %DB_PORT% -U %DB_USER% -d %DB_NAME% -f migrations\001_initial_schema.sql
if %ERRORLEVEL% EQU 0 (
    echo [INFO] Migration completed successfully
) else (
    echo [ERROR] Migration failed!
    pause
    exit /b 1
)

REM Seed data
echo.
set /p SEED_DATA="Do you want to seed mock data? (Y/N): "
if /i "%SEED_DATA%"=="Y" (
    echo [INFO] Seeding mock data...
    psql -h %DB_HOST% -p %DB_PORT% -U %DB_USER% -d %DB_NAME% -f seeds\seed_mock_data.sql
    if %ERRORLEVEL% EQU 0 (
        echo [INFO] Data seeded successfully
    ) else (
        echo [ERROR] Seeding failed!
        pause
        exit /b 1
    )
) else (
    echo [INFO] Skipping data seeding
)

REM Verify setup
echo.
echo [INFO] Verifying database setup...
psql -h %DB_HOST% -p %DB_PORT% -U %DB_USER% -d %DB_NAME% -c "SELECT 'Tables created:' AS status, COUNT(*) AS count FROM information_schema.tables WHERE table_schema = 'public';"

echo.
echo =========================================
echo   Database setup completed successfully!
echo =========================================
echo.
echo Connection string:
echo   postgresql://%DB_USER%:****@%DB_HOST%:%DB_PORT%/%DB_NAME%
echo.
echo Next steps:
echo   1. Update your .env file with the database credentials
echo   2. Configure Looker Studio to connect to this database
echo   3. Create Looker Studio reports for MRR, ARR, MAU, and DAU
echo   4. Update environment variables with Looker Studio embed URLs
echo.
pause
