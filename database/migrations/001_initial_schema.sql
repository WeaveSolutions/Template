-- Weave BI Dashboard - Initial Database Schema
-- PostgreSQL Migration Script
-- Created: November 9, 2024

-- Create database (run separately as superuser if needed)
-- CREATE DATABASE weave_bi;
-- \c weave_bi;

-- Enable UUID extension for future use
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

-- ============================================
-- 1. KPI Metrics Table
-- ============================================
CREATE TABLE IF NOT EXISTS kpi_metrics (
    id SERIAL PRIMARY KEY,
    metric_date DATE NOT NULL UNIQUE,
    
    -- Revenue Metrics
    mrr DECIMAL(12,2) NOT NULL DEFAULT 0,
    arr DECIMAL(12,2) NOT NULL DEFAULT 0,
    arpu DECIMAL(10,2) NOT NULL DEFAULT 0,
    
    -- User Metrics
    total_users INTEGER NOT NULL DEFAULT 0,
    active_users INTEGER NOT NULL DEFAULT 0,
    dau INTEGER NOT NULL DEFAULT 0,
    mau INTEGER NOT NULL DEFAULT 0,
    
    -- Subscription Metrics
    active_subscriptions INTEGER NOT NULL DEFAULT 0,
    churn_rate DECIMAL(5,2) NOT NULL DEFAULT 0,
    
    -- Financial Metrics
    ltv DECIMAL(10,2) NOT NULL DEFAULT 0,
    cac DECIMAL(10,2) NOT NULL DEFAULT 0,
    ltv_cac_ratio DECIMAL(5,2) NOT NULL DEFAULT 0,
    
    -- Satisfaction
    nps INTEGER NOT NULL DEFAULT 0,
    
    -- Technical
    uptime DECIMAL(5,2) NOT NULL DEFAULT 0,
    avg_load_time DECIMAL(5,2) NOT NULL DEFAULT 0,
    error_rate DECIMAL(5,2) NOT NULL DEFAULT 0,
    
    -- Timestamps
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
);

CREATE INDEX idx_kpi_metrics_date ON kpi_metrics(metric_date DESC);

-- ============================================
-- 2. Revenue Trends Table
-- ============================================
CREATE TABLE IF NOT EXISTS revenue_trends (
    id SERIAL PRIMARY KEY,
    period VARCHAR(20) NOT NULL,
    period_date DATE NOT NULL,
    metric_type VARCHAR(10) NOT NULL CHECK (metric_type IN ('MRR', 'ARR')),
    value DECIMAL(12,2) NOT NULL,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    
    UNIQUE(period_date, metric_type)
);

CREATE INDEX idx_revenue_trends_period_date ON revenue_trends(period_date DESC);
CREATE INDEX idx_revenue_trends_metric_type ON revenue_trends(metric_type);
CREATE INDEX idx_revenue_trends_composite ON revenue_trends(metric_type, period_date DESC);

-- ============================================
-- 3. User Growth Table
-- ============================================
CREATE TABLE IF NOT EXISTS user_growth (
    id SERIAL PRIMARY KEY,
    period VARCHAR(20) NOT NULL,
    period_date DATE NOT NULL,
    metric_type VARCHAR(10) NOT NULL CHECK (metric_type IN ('MAU', 'DAU')),
    
    -- Aggregate metrics
    dau INTEGER NOT NULL DEFAULT 0,
    mau INTEGER NOT NULL DEFAULT 0,
    
    -- Platform breakdown
    mobile INTEGER NOT NULL DEFAULT 0,
    desktop INTEGER NOT NULL DEFAULT 0,
    website INTEGER NOT NULL DEFAULT 0,
    
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    
    UNIQUE(period_date, metric_type)
);

CREATE INDEX idx_user_growth_period_date ON user_growth(period_date DESC);
CREATE INDEX idx_user_growth_metric_type ON user_growth(metric_type);
CREATE INDEX idx_user_growth_composite ON user_growth(metric_type, period_date DESC);

-- ============================================
-- 4. Subscription Breakdown Table
-- ============================================
CREATE TABLE IF NOT EXISTS subscription_breakdown (
    id SERIAL PRIMARY KEY,
    snapshot_date DATE NOT NULL,
    plan_name VARCHAR(50) NOT NULL CHECK (plan_name IN ('Free', 'Pro', 'Enterprise')),
    subscriber_count INTEGER NOT NULL DEFAULT 0,
    percentage DECIMAL(5,2) NOT NULL DEFAULT 0,
    created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    
    UNIQUE(snapshot_date, plan_name)
);

CREATE INDEX idx_subscription_breakdown_date ON subscription_breakdown(snapshot_date DESC);
CREATE INDEX idx_subscription_breakdown_plan ON subscription_breakdown(plan_name);

-- ============================================
-- Trigger for updated_at timestamp
-- ============================================
CREATE OR REPLACE FUNCTION update_updated_at_column()
RETURNS TRIGGER AS $$
BEGIN
    NEW.updated_at = CURRENT_TIMESTAMP;
    RETURN NEW;
END;
$$ language 'plpgsql';

CREATE TRIGGER update_kpi_metrics_updated_at 
    BEFORE UPDATE ON kpi_metrics
    FOR EACH ROW
    EXECUTE FUNCTION update_updated_at_column();

-- ============================================
-- Views for easier querying
-- ============================================

-- Latest KPI snapshot
CREATE OR REPLACE VIEW v_latest_kpi AS
SELECT * FROM kpi_metrics
ORDER BY metric_date DESC
LIMIT 1;

-- MRR trend (last 12 months)
CREATE OR REPLACE VIEW v_mrr_trend_12m AS
SELECT period, period_date, value
FROM revenue_trends
WHERE metric_type = 'MRR'
ORDER BY period_date DESC
LIMIT 12;

-- ARR trend (last 10 years)
CREATE OR REPLACE VIEW v_arr_trend_10y AS
SELECT period, period_date, value
FROM revenue_trends
WHERE metric_type = 'ARR'
ORDER BY period_date DESC
LIMIT 10;

-- MAU trend (last 12 months)
CREATE OR REPLACE VIEW v_mau_trend_12m AS
SELECT period, period_date, dau, mau, mobile, desktop, website
FROM user_growth
WHERE metric_type = 'MAU'
ORDER BY period_date DESC
LIMIT 12;

-- DAU trend (last 30 days)
CREATE OR REPLACE VIEW v_dau_trend_30d AS
SELECT period, period_date, dau, mobile, desktop, website
FROM user_growth
WHERE metric_type = 'DAU'
ORDER BY period_date DESC
LIMIT 30;

-- Latest subscription breakdown
CREATE OR REPLACE VIEW v_latest_subscriptions AS
SELECT plan_name, subscriber_count, percentage
FROM subscription_breakdown
WHERE snapshot_date = (SELECT MAX(snapshot_date) FROM subscription_breakdown)
ORDER BY subscriber_count DESC;

-- ============================================
-- Grant permissions (adjust username as needed)
-- ============================================
-- GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO weave_admin;
-- GRANT USAGE, SELECT ON ALL SEQUENCES IN SCHEMA public TO weave_admin;
-- GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA public TO weave_admin;

-- ============================================
-- Comments for documentation
-- ============================================
COMMENT ON TABLE kpi_metrics IS 'Key Performance Indicators - Daily snapshots';
COMMENT ON TABLE revenue_trends IS 'Revenue trends over time - MRR (monthly) and ARR (yearly)';
COMMENT ON TABLE user_growth IS 'User growth metrics with platform breakdown - MAU and DAU';
COMMENT ON TABLE subscription_breakdown IS 'Subscription plan distribution snapshots';

COMMENT ON COLUMN revenue_trends.metric_type IS 'Type of revenue metric: MRR or ARR';
COMMENT ON COLUMN user_growth.metric_type IS 'Type of user metric: MAU or DAU';
