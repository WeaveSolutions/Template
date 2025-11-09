-- Weave BI Dashboard - Seed Mock Data
-- PostgreSQL Seed Script
-- This script populates the database with mock data for development and testing
-- Created: November 9, 2024

-- Clean existing data (optional - comment out if you want to preserve data)
-- TRUNCATE TABLE kpi_metrics, revenue_trends, user_growth, subscription_breakdown RESTART IDENTITY CASCADE;

-- ============================================
-- 1. Seed KPI Metrics (Latest snapshot)
-- ============================================
INSERT INTO kpi_metrics (
    metric_date, mrr, arr, arpu, total_users, active_users, dau, mau,
    active_subscriptions, churn_rate, ltv, cac, ltv_cac_ratio, nps,
    uptime, avg_load_time, error_rate
) VALUES (
    '2024-11-09', 98750.00, 1185000.00, 11.05, 15420, 8930, 3245, 8930,
    1250, 3.2, 450.00, 120.00, 3.75, 42,
    99.94, 1.2, 0.03
)
ON CONFLICT (metric_date) DO UPDATE SET
    mrr = EXCLUDED.mrr,
    arr = EXCLUDED.arr,
    arpu = EXCLUDED.arpu,
    total_users = EXCLUDED.total_users,
    active_users = EXCLUDED.active_users,
    dau = EXCLUDED.dau,
    mau = EXCLUDED.mau,
    active_subscriptions = EXCLUDED.active_subscriptions,
    churn_rate = EXCLUDED.churn_rate,
    ltv = EXCLUDED.ltv,
    cac = EXCLUDED.cac,
    ltv_cac_ratio = EXCLUDED.ltv_cac_ratio,
    nps = EXCLUDED.nps,
    uptime = EXCLUDED.uptime,
    avg_load_time = EXCLUDED.avg_load_time,
    error_rate = EXCLUDED.error_rate,
    updated_at = CURRENT_TIMESTAMP;

-- ============================================
-- 2. Seed Revenue Trends - MRR (6 months)
-- ============================================
INSERT INTO revenue_trends (period, period_date, metric_type, value) VALUES
    ('Jan', '2024-01-01', 'MRR', 85000.00),
    ('Feb', '2024-02-01', 'MRR', 87500.00),
    ('Mar', '2024-03-01', 'MRR', 91000.00),
    ('Apr', '2024-04-01', 'MRR', 93500.00),
    ('May', '2024-05-01', 'MRR', 96000.00),
    ('Jun', '2024-06-01', 'MRR', 98750.00)
ON CONFLICT (period_date, metric_type) DO UPDATE SET
    value = EXCLUDED.value;

-- ============================================
-- 3. Seed Revenue Trends - ARR (10 years)
-- ============================================
INSERT INTO revenue_trends (period, period_date, metric_type, value) VALUES
    ('2015', '2015-01-01', 'ARR', 420000.00),
    ('2016', '2016-01-01', 'ARR', 485000.00),
    ('2017', '2017-01-01', 'ARR', 562000.00),
    ('2018', '2018-01-01', 'ARR', 648000.00),
    ('2019', '2019-01-01', 'ARR', 745000.00),
    ('2020', '2020-01-01', 'ARR', 856000.00),
    ('2021', '2021-01-01', 'ARR', 925000.00),
    ('2022', '2022-01-01', 'ARR', 1015000.00),
    ('2023', '2023-01-01', 'ARR', 1095000.00),
    ('2024', '2024-01-01', 'ARR', 1185000.00)
ON CONFLICT (period_date, metric_type) DO UPDATE SET
    value = EXCLUDED.value;

-- ============================================
-- 4. Seed User Growth - MAU (6 months)
-- ============================================
INSERT INTO user_growth (period, period_date, metric_type, dau, mau, mobile, desktop, website) VALUES
    ('Jan', '2024-01-01', 'MAU', 2850, 7200, 3600, 2400, 1200),
    ('Feb', '2024-02-01', 'MAU', 2920, 7550, 3775, 2513, 1262),
    ('Mar', '2024-03-01', 'MAU', 3050, 8100, 4050, 2700, 1350),
    ('Apr', '2024-04-01', 'MAU', 3120, 8450, 4225, 2817, 1408),
    ('May', '2024-05-01', 'MAU', 3180, 8720, 4360, 2907, 1453),
    ('Jun', '2024-06-01', 'MAU', 3245, 8930, 4465, 2977, 1488)
ON CONFLICT (period_date, metric_type) DO UPDATE SET
    dau = EXCLUDED.dau,
    mau = EXCLUDED.mau,
    mobile = EXCLUDED.mobile,
    desktop = EXCLUDED.desktop,
    website = EXCLUDED.website;

-- ============================================
-- 5. Seed User Growth - DAU (30 days, November 2024)
-- ============================================
INSERT INTO user_growth (period, period_date, metric_type, dau, mau, mobile, desktop, website) VALUES
    ('Nov 1', '2024-11-01', 'DAU', 2980, 0, 1490, 994, 496),
    ('Nov 2', '2024-11-02', 'DAU', 3020, 0, 1510, 1007, 503),
    ('Nov 3', '2024-11-03', 'DAU', 2850, 0, 1425, 950, 475),
    ('Nov 4', '2024-11-04', 'DAU', 2920, 0, 1460, 973, 487),
    ('Nov 5', '2024-11-05', 'DAU', 3100, 0, 1550, 1033, 517),
    ('Nov 6', '2024-11-06', 'DAU', 3180, 0, 1590, 1060, 530),
    ('Nov 7', '2024-11-07', 'DAU', 3150, 0, 1575, 1050, 525),
    ('Nov 8', '2024-11-08', 'DAU', 3050, 0, 1525, 1017, 508),
    ('Nov 9', '2024-11-09', 'DAU', 2980, 0, 1490, 993, 497),
    ('Nov 10', '2024-11-10', 'DAU', 3200, 0, 1600, 1067, 533),
    ('Nov 11', '2024-11-11', 'DAU', 3240, 0, 1620, 1080, 540),
    ('Nov 12', '2024-11-12', 'DAU', 3190, 0, 1595, 1063, 532),
    ('Nov 13', '2024-11-13', 'DAU', 3080, 0, 1540, 1027, 513),
    ('Nov 14', '2024-11-14', 'DAU', 3150, 0, 1575, 1050, 525),
    ('Nov 15', '2024-11-15', 'DAU', 3210, 0, 1605, 1070, 535),
    ('Nov 16', '2024-11-16', 'DAU', 2950, 0, 1475, 983, 492),
    ('Nov 17', '2024-11-17', 'DAU', 2880, 0, 1440, 960, 480),
    ('Nov 18', '2024-11-18', 'DAU', 3120, 0, 1560, 1040, 520),
    ('Nov 19', '2024-11-19', 'DAU', 3180, 0, 1590, 1060, 530),
    ('Nov 20', '2024-11-20', 'DAU', 3230, 0, 1615, 1077, 538),
    ('Nov 21', '2024-11-21', 'DAU', 3190, 0, 1595, 1063, 532),
    ('Nov 22', '2024-11-22', 'DAU', 3160, 0, 1580, 1053, 527),
    ('Nov 23', '2024-11-23', 'DAU', 3000, 0, 1500, 1000, 500),
    ('Nov 24', '2024-11-24', 'DAU', 2920, 0, 1460, 973, 487),
    ('Nov 25', '2024-11-25', 'DAU', 3140, 0, 1570, 1047, 523),
    ('Nov 26', '2024-11-26', 'DAU', 3200, 0, 1600, 1067, 533),
    ('Nov 27', '2024-11-27', 'DAU', 3250, 0, 1625, 1083, 542),
    ('Nov 28', '2024-11-28', 'DAU', 3210, 0, 1605, 1070, 535),
    ('Nov 29', '2024-11-29', 'DAU', 3180, 0, 1590, 1060, 530),
    ('Nov 30', '2024-11-30', 'DAU', 3245, 0, 1622, 1082, 541)
ON CONFLICT (period_date, metric_type) DO UPDATE SET
    dau = EXCLUDED.dau,
    mobile = EXCLUDED.mobile,
    desktop = EXCLUDED.desktop,
    website = EXCLUDED.website;

-- ============================================
-- 6. Seed Subscription Breakdown
-- ============================================
INSERT INTO subscription_breakdown (snapshot_date, plan_name, subscriber_count, percentage) VALUES
    ('2024-11-09', 'Free', 12500, 81.00),
    ('2024-11-09', 'Pro', 2100, 14.00),
    ('2024-11-09', 'Enterprise', 820, 5.00)
ON CONFLICT (snapshot_date, plan_name) DO UPDATE SET
    subscriber_count = EXCLUDED.subscriber_count,
    percentage = EXCLUDED.percentage;

-- ============================================
-- Verify seeded data
-- ============================================
SELECT 'KPI Metrics seeded:' AS status, COUNT(*) AS count FROM kpi_metrics
UNION ALL
SELECT 'Revenue Trends (MRR) seeded:', COUNT(*) FROM revenue_trends WHERE metric_type = 'MRR'
UNION ALL
SELECT 'Revenue Trends (ARR) seeded:', COUNT(*) FROM revenue_trends WHERE metric_type = 'ARR'
UNION ALL
SELECT 'User Growth (MAU) seeded:', COUNT(*) FROM user_growth WHERE metric_type = 'MAU'
UNION ALL
SELECT 'User Growth (DAU) seeded:', COUNT(*) FROM user_growth WHERE metric_type = 'DAU'
UNION ALL
SELECT 'Subscription Breakdown seeded:', COUNT(*) FROM subscription_breakdown;
