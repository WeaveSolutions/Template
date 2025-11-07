# Dashboard Page Template

## Overview
Main application interface showing key metrics, data, and actions.

## Structure
The dashboard should include:
- Page header with title and action buttons (Export, Refresh, etc.)
- Stats cards showing key metrics with trend indicators
- Interactive charts for data visualization
- Recent activity feed or timeline
- Quick action buttons for common tasks

## Key Features
- **Stats Overview Cards**: Display important metrics at a glance with change indicators
- **Interactive Charts**: Visualize data trends over time
- **Recent Activity Feed**: Show latest events, actions, or updates
- **Quick Actions**: Easy access to frequently used features
- **Real-time Updates**: Live data refresh without page reload
- **Filters & Date Ranges**: Allow users to customize data view
- **Responsive Layout**: Grid adapts to different screen sizes

## Data Loading
Implement proper loading states:
- Show skeleton loaders while fetching data
- Display error states with retry options
- Handle empty states gracefully
- Implement optimistic updates for better UX
- Cache data appropriately

## Best Practices
1. Prioritize most important metrics at the top
2. Use consistent card layouts
3. Provide data export functionality
4. Include time range selectors
5. Show loading states for all async data
6. Make charts interactive and responsive
7. Allow customization of dashboard layout
8. Implement proper error handling
