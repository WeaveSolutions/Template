# Dashboard Page Template

## Overview
Main application interface showing key metrics, data, and actions.

## Structure
```tsx
export default function Dashboard() {
  return (
    <div className="dashboard">
      <Header title="Dashboard" actions={[{ label: 'Export', onClick: handleExport }]} />
      
      {/* Stats Cards */}
      <div className="stats-grid">
        <StatsCard title="Total Users" value="1,234" change="+12%" />
        <StatsCard title="Revenue" value="$45.6K" change="+8%" />
        <StatsCard title="Active Sessions" value="567" change="-3%" />
      </div>

      {/* Charts */}
      <div className="charts">
        <ChartCard title="Revenue Over Time" />
        <ChartCard title="User Growth" />
      </div>

      {/* Recent Activity */}
      <section className="recent-activity">
        <h3>Recent Activity</h3>
        <ActivityList items={recentItems} />
      </section>
    </div>
  );
}
```

## Key Features
- Stats overview cards
- Interactive charts
- Recent activity feed
- Quick actions
- Real-time updates

## Data Loading
```tsx
const { data, loading, error } = useQuery(DASHBOARD_QUERY);

if (loading) return <Skeleton />;
if (error) return <ErrorState />;
```
