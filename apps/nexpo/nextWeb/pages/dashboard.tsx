import React, { useState } from 'react';
import { NextSeo } from 'next-seo';
import Navigation from '../components/Navigation';

// ✅ Using shared BI core!
import { 
  mockKPIData,
  mockMRRTrend,
  mockARRTrend,
  mockUserGrowth,
  mockDAUTrend,
  mockSubscriptionBreakdown
} from '@weave/bi-core';
import { LOOKER_STUDIO_URL } from '@weave/bi-core/config';

// Use shared mock data
const mrrTrend = mockMRRTrend;
const arrTrend = mockARRTrend;
const userGrowth = mockUserGrowth;
const dauTrend = mockDAUTrend;
const subscriptionBreakdown = mockSubscriptionBreakdown;

type TabType = 'overview' | 'revenue' | 'users' | 'looker';

// Helper function to get X-axis labels
// For MRR: returns month names (Jan, Feb, etc.)
// For ARR: returns the year directly from the data
const getXAxisLabel = (month: string, isARR: boolean): string => {
  return month; // ARR data already contains years (2015, 2016, etc.)
};

export default function DashboardPage() {
  const [activeTab, setActiveTab] = useState<TabType>('overview');
  const [revenueMetric, setRevenueMetric] = useState<'mrr' | 'arr'>('mrr');
  const [userMetric, setUserMetric] = useState<'mau' | 'dau'>('dau');
  const [hoveredRevenuePoint, setHoveredRevenuePoint] = useState<number | null>(null);
  const [hoveredUserPoint, setHoveredUserPoint] = useState<{
    series: 'total' | 'mobile' | 'desktop' | 'website';
    index: number;
  } | null>(null);

  return (
    <>
      <NextSeo
        title="Nexpo Web"
        description="Business Intelligence Dashboard with KPIs and Analytics"
      />
      <div className="min-h-screen bg-gradient-to-b from-gray-900 to-gray-800 text-white">
        <Navigation />
        <div className="container mx-auto px-4 pt-8 pb-8">
          {/* Header */}
          <div className="mb-8">
            <h1 className="text-4xl font-bold mb-2">Business Intelligence Dashboard</h1>
            <p className="text-gray-400">Real-time insights and key performance indicators</p>
          </div>

          {/* Tab Navigation */}
          <div className="flex space-x-2 mb-8 overflow-x-auto">
            {[
              { id: 'overview', label: '📊 Overview', icon: '📊' },
              { id: 'revenue', label: '💰 Revenue', icon: '💰' },
              { id: 'users', label: '👥 Users', icon: '👥' },
              { id: 'looker', label: '📈 Looker Studio', icon: '📈' }
            ].map((tab) => (
              <button
                key={tab.id}
                onClick={() => setActiveTab(tab.id as TabType)}
                className={`px-6 py-3 rounded-lg font-semibold transition-colors whitespace-nowrap ${
                  activeTab === tab.id
                    ? 'bg-blue-600 text-white'
                    : 'bg-gray-800 text-gray-400 hover:bg-gray-700'
                }`}
              >
                {tab.label}
              </button>
            ))}
          </div>

          {/* Overview Tab */}
          {activeTab === 'overview' && (
            <div className="space-y-8">
              {/* Executive KPIs */}
              <div>
                <h2 className="text-2xl font-bold mb-4">Executive KPIs</h2>
                <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
                  <KPICard
                    label="Monthly Recurring Revenue"
                    value={`$${mockKPIData.mrr.toLocaleString()}`}
                    change="+8.5%"
                    positive={true}
                    icon="💰"
                    tooltip="Predictable revenue generated each month from subscriptions. Essential for forecasting growth and measuring business stability."
                  />
                  <KPICard
                    label="Annual Recurring Revenue"
                    value={`$${mockKPIData.arr.toLocaleString()}`}
                    change="+8.5%"
                    positive={true}
                    icon="📈"
                    tooltip="Total value of recurring subscription revenue normalized to a year. Key metric for valuation and long-term planning."
                  />
                  <KPICard
                    label="Total Users"
                    value={mockKPIData.totalUsers.toLocaleString()}
                    change="+12.3%"
                    positive={true}
                    icon="👥"
                    tooltip="Total number of registered users on the platform. Shows overall reach and market penetration of your product."
                  />
                  <KPICard
                    label="Active Subscriptions"
                    value={mockKPIData.activeSubscriptions.toLocaleString()}
                    change="+5.2%"
                    positive={true}
                    icon="✅"
                    tooltip="Number of paying subscribers currently active. Directly correlates to revenue and shows product-market fit."
                  />
                </div>
              </div>

              {/* User Metrics */}
              <div>
                <h2 className="text-2xl font-bold mb-4">User Engagement</h2>
                <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-6">
                  <KPICard
                    label="Daily Active Users"
                    value={mockKPIData.dau.toLocaleString()}
                    change="+4.2%"
                    positive={true}
                    icon="📱"
                    tooltip="Users who engage with the platform daily. Measures product stickiness and how essential your service is to users."
                  />
                  <KPICard
                    label="Monthly Active Users"
                    value={mockKPIData.mau.toLocaleString()}
                    change="+6.8%"
                    positive={true}
                    icon="🌐"
                    tooltip="Unique users who logged in within the last 30 days. Indicates overall platform health and user retention."
                  />
                  <KPICard
                    label="DAU/MAU Ratio"
                    value={`${((mockKPIData.dau / mockKPIData.mau) * 100).toFixed(1)}%`}
                    change="Stable"
                    positive={true}
                    icon="📊"
                    tooltip="Percentage of monthly users who engage daily. Higher ratios indicate stronger user habits and product engagement."
                  />
                </div>
              </div>

              {/* Financial Health */}
              <div>
                <h2 className="text-2xl font-bold mb-4">Financial Health</h2>
                <div className="grid grid-cols-1 md:grid-cols-4 gap-6">
                  <KPICard
                    label="ARPU"
                    value={`$${mockKPIData.arpu.toFixed(2)}`}
                    change="+2.1%"
                    positive={true}
                    icon="💵"
                    tooltip="Average Revenue Per User. Shows how much revenue each user generates on average. Key for pricing strategy."
                  />
                  <KPICard
                    label="Customer LTV"
                    value={`$${mockKPIData.ltv.toFixed(2)}`}
                    change="+5.5%"
                    positive={true}
                    icon="🎯"
                    tooltip="Lifetime Value: total revenue expected from a customer over their entire relationship. Critical for acquisition budgets."
                  />
                  <KPICard
                    label="CAC"
                    value={`$${mockKPIData.cac.toFixed(2)}`}
                    change="-3.2%"
                    positive={true}
                    icon="📉"
                    tooltip="Customer Acquisition Cost: total marketing and sales spend to acquire one customer. Lower is better for profitability."
                  />
                  <KPICard
                    label="LTV:CAC Ratio"
                    value={mockKPIData.ltvCacRatio.toFixed(2)}
                    change="Healthy"
                    positive={true}
                    icon="⚖️"
                    tooltip="Ratio of customer lifetime value to acquisition cost. 3:1 or higher is ideal. Shows sustainable growth economics."
                  />
                </div>
              </div>

              {/* Other Metrics */}
              <div>
                <h2 className="text-2xl font-bold mb-4">Additional Metrics</h2>
                <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
                  <KPICard
                    label="Churn Rate"
                    value={`${mockKPIData.churnRate}%`}
                    change="-0.5%"
                    positive={true}
                    icon="📉"
                    tooltip="Percentage of customers who cancel their subscription. Lower churn means better retention and predictable revenue."
                  />
                  <KPICard
                    label="Net Promoter Score"
                    value={mockKPIData.nps.toString()}
                    change="+3 pts"
                    positive={true}
                    icon="⭐"
                    tooltip="Measures customer satisfaction and loyalty. Scores above 50 are excellent. Higher scores correlate with organic growth."
                  />
                  <KPICard
                    label="System Uptime"
                    value={`${mockKPIData.uptime}%`}
                    change="+0.02%"
                    positive={true}
                    icon="✅"
                    tooltip="Percentage of time the system is operational. 99.9% (three nines) is considered excellent for SaaS platforms."
                  />
                </div>
              </div>
            </div>
          )}

          {/* Revenue Tab */}
          {activeTab === 'revenue' && (
            <div>
              {/* Revenue KPIs */}
              <div className="mb-8">
                <h2 className="text-2xl font-bold mb-4">Revenue Overview</h2>
                <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
                  <KPICard
                    label="MRR"
                    value={`$${mockKPIData.mrr.toLocaleString()}`}
                    change="+$7,250"
                    positive={true}
                    icon="💰"
                    tooltip="Predictable revenue generated each month from subscriptions. Essential for forecasting growth and measuring business stability."
                    size="large"
                  />
                  <KPICard
                    label="ARR"
                    value={`$${mockKPIData.arr.toLocaleString()}`}
                    change="+$87,000"
                    positive={true}
                    icon="📈"
                    tooltip="Total value of recurring subscription revenue normalized to a year. Key metric for valuation and long-term planning."
                    size="large"
                  />
                  <KPICard
                    label="ARPU"
                    value={`$${mockKPIData.arpu.toFixed(2)}`}
                    change="+$0.23"
                    positive={true}
                    icon="💵"
                    tooltip="Average Revenue Per User. Shows how much revenue each user generates on average. Key for pricing strategy."
                    size="large"
                  />
                </div>
              </div>

              {/* Revenue Trend Chart with MRR/ARR Toggle */}
              <div className="bg-gray-800 rounded-lg border border-gray-700 p-6 mb-8">
                <div className="flex justify-between items-center mb-6">
                  <h3 className="text-xl font-bold">Revenue Trend</h3>
                  
                  {/* Toggle Switch */}
                  <div className="flex items-center gap-2 bg-gray-700 rounded-lg p-1">
                    <button
                      onClick={() => setRevenueMetric('mrr')}
                      className={`px-4 py-2 rounded-md text-sm font-semibold transition-all ${
                        revenueMetric === 'mrr'
                          ? 'bg-blue-600 text-white'
                          : 'text-gray-400 hover:text-white'
                      }`}
                    >
                      MRR
                    </button>
                    <button
                      onClick={() => setRevenueMetric('arr')}
                      className={`px-4 py-2 rounded-md text-sm font-semibold transition-all ${
                        revenueMetric === 'arr'
                          ? 'bg-blue-600 text-white'
                          : 'text-gray-400 hover:text-white'
                      }`}
                    >
                      ARR
                    </button>
                  </div>
                </div>

                <div className="flex gap-4" style={{ aspectRatio: '16 / 9' }}>
                  {/* Y-Axis Labels */}
                  <div className="flex flex-col justify-between text-right pr-3 border-r border-gray-700" style={{ height: 'calc(100% - 24px)' }}>
                    {(() => {
                      const currentTrend = revenueMetric === 'mrr' ? mrrTrend : arrTrend.slice(2);
                      const roundTo = revenueMetric === 'mrr' ? 500 : 10000;
                      // Compute padded range
                      const dataMin = Math.min(...currentTrend.map(d => d.value));
                      const dataMax = Math.max(...currentTrend.map(d => d.value));
                      const padRange = dataMax - dataMin;
                      const paddedMin = Math.max(0, dataMin - padRange * 0.1);
                      const paddedMax = dataMax + padRange * 0.1;
                      // Snap to nice ticks and build labels at exact 25% intervals
                      const tickStep = Math.max(roundTo, Math.ceil(((paddedMax - paddedMin) / 4) / roundTo) * roundTo);
                      const snapMin = Math.floor(paddedMin / roundTo) * roundTo;
                      const snapMax = snapMin + tickStep * 4;
                      return (
                        <>
                          <span className="text-sm text-gray-400">${(snapMax).toLocaleString()}</span>
                          <span className="text-sm text-gray-400">${(snapMax - tickStep).toLocaleString()}</span>
                          <span className="text-sm text-gray-400">${(snapMax - tickStep * 2).toLocaleString()}</span>
                          <span className="text-sm text-gray-400">${(snapMax - tickStep * 3).toLocaleString()}</span>
                          <span className="text-sm text-gray-400">${(snapMin).toLocaleString()}</span>
                        </>
                      );
                    })()}
                  </div>
                  
                  {/* Chart Area */}
                  <div className="flex-1">
                    <div className="h-full relative">
                      {/* Grid Lines */}
                      <div className="absolute inset-0 flex flex-col justify-between pointer-events-none" style={{ bottom: '24px' }}>
                        <div className="border-t border-gray-700/50"></div>
                        <div className="border-t border-gray-700/50"></div>
                        <div className="border-t border-gray-700/50"></div>
                        <div className="border-t border-gray-700/50"></div>
                        <div className="border-t border-gray-700"></div>
                      </div>
                      
                      <div className="absolute inset-0" style={{ bottom: '24px', overflow: 'hidden' }}>
                      {/* Line Graph */}
                      {(() => {
                        const currentTrend = revenueMetric === 'mrr' ? mrrTrend : arrTrend.slice(2);
                        // Auto-fit with snapped ticks so labels and plotting share the same range
                        const dataMin = Math.min(...currentTrend.map(d => d.value));
                        const dataMax = Math.max(...currentTrend.map(d => d.value));
                        const padRange = dataMax - dataMin;
                        const paddedMin = Math.max(0, dataMin - padRange * 0.1);
                        const paddedMax = dataMax + padRange * 0.1;
                        const roundTo = revenueMetric === 'mrr' ? 500 : 10000;
                        const tickStep = Math.max(roundTo, Math.ceil(((paddedMax - paddedMin) / 4) / roundTo) * roundTo);
                        const snapMin = Math.floor(paddedMin / roundTo) * roundTo;
                        const snapMax = snapMin + tickStep * 4;
                        const denom = Math.max(1, snapMax - snapMin);
                        
                        const padding = 3;
                        const usableWidth = 100 - (2 * padding);
                        const yPad = 0;
                        const clamp = (n: number, min: number, max: number) => Math.min(max, Math.max(min, n));
                        const points = currentTrend.map((item, i) => {
                          const x = padding + (i / (currentTrend.length - 1)) * usableWidth;
                          const yRaw = 100 - (((item.value - snapMin) / denom) * (100 - 2 * yPad)) - yPad;
                          const y = clamp(yRaw, yPad, 100 - yPad);
                          return `${x},${y}`;
                        }).join(' ');
                        
                        return (
                          <>
                            <svg className="absolute inset-0" style={{ zIndex: 20 }} preserveAspectRatio="none" viewBox="0 0 100 100">
                              <defs>
                                <clipPath id="revenueChartClipNexpo">
                                  <rect x="0" y="0" width="100" height="100" />
                                </clipPath>
                              </defs>
                              <g clipPath="url(#revenueChartClipNexpo)">
                              <polyline
                                points={points}
                                fill="none"
                                stroke="#60a5fa"
                                strokeWidth="1.5"
                                vectorEffect="non-scaling-stroke"
                                pointerEvents="none"
                              />
                              {currentTrend.map((item, i) => {
                                const x = padding + (i / (currentTrend.length - 1)) * usableWidth;
                                const y = clamp(100 - (((item.value - snapMin) / denom) * (100 - 2 * yPad)) - yPad, yPad, 100 - yPad);
                                return (
                                  <g key={i}>
                                    <circle
                                      cx={x}
                                      cy={y}
                                      r="0.7"
                                      fill="#60a5fa"
                                      vectorEffect="non-scaling-stroke"
                                      pointerEvents="auto"
                                      style={{ cursor: 'pointer' }}
                                      onMouseEnter={() => setHoveredRevenuePoint(i)}
                                      onMouseLeave={() => setHoveredRevenuePoint(null)}
                                    />
                                  </g>
                                );
                              })}
                              </g>
                              {hoveredRevenuePoint !== null && (() => {
                                const i = hoveredRevenuePoint;
                                const item = currentTrend[i];
                                const x = padding + (i / (currentTrend.length - 1)) * usableWidth;
                                const y = clamp(100 - (((item.value - snapMin) / denom) * (100 - 2 * yPad)) - yPad, yPad, 100 - yPad);
                                return (
                                  <foreignObject x={x - 15} y={y - 10} width="30" height="12" overflow="visible">
                                    <div style={{ position: 'absolute', transform: 'translate(-50%, -100%)', whiteSpace: 'nowrap', pointerEvents: 'none' }}>
                                      <div style={{ backgroundColor: '#000', color: '#fff', fontSize: '10px', lineHeight: 1, fontWeight: 600, padding: '2px 6px', borderRadius: 4 }}>
                                        ${item.value.toLocaleString()}
                                      </div>
                                    </div>
                                  </foreignObject>
                                );
                              })()}
                            </svg>
                          </>
                        );
                      })()}
                      </div>
                      
                      {/* X-Axis Labels */}
                      {(() => {
                        const currentTrend = revenueMetric === 'mrr' ? mrrTrend : arrTrend.slice(2);
                        return (
                          <div className="absolute left-0 right-0 bottom-0 h-6 flex justify-between px-2">
                            {currentTrend.map((item, i) => (
                              <div key={i} className="text-xs text-gray-400 font-medium text-center">
                                {getXAxisLabel(item.month, revenueMetric === 'arr')}
                              </div>
                            ))}
                          </div>
                        );
                      })()}
                    </div>
                  </div>
                </div>
                
                {/* Metric Label */}
                <div className="mt-4 pt-4 border-t border-gray-700 text-center">
                  <span className="text-sm text-gray-400">
                    Showing <span className="font-semibold text-blue-400">{revenueMetric === 'mrr' ? 'Monthly' : 'Annual'} Recurring Revenue</span> trend
                  </span>
                </div>
              </div>

              {/* Subscription Breakdown */}
              <div className="bg-gray-800 rounded-lg border border-gray-700 p-6">
                <h3 className="text-xl font-bold mb-6">Subscription Plan Distribution</h3>
                <div className="space-y-4">
                  {subscriptionBreakdown.map((sub) => (
                    <div key={sub.plan}>
                      <div className="flex justify-between mb-2">
                        <span className="font-semibold">{sub.plan}</span>
                        <span className="text-gray-400">{sub.count.toLocaleString()} ({sub.percentage}%)</span>
                      </div>
                      <div className="w-full bg-gray-700 rounded-full h-3">
                        <div
                          className={`h-3 rounded-full ${
                            sub.plan === 'Enterprise' ? 'bg-purple-500' :
                            sub.plan === 'Pro' ? 'bg-blue-500' : 'bg-gray-500'
                          }`}
                          style={{ width: `${sub.percentage}%` }}
                        />
                      </div>
                    </div>
                  ))}
                </div>
              </div>
            </div>
          )}

          {/* Users Tab */}
          {activeTab === 'users' && (
            <div>
              {/* User KPIs */}
              <div className="mb-8">
                <h2 className="text-2xl font-bold mb-4">User Metrics</h2>
                <div className="grid grid-cols-1 md:grid-cols-4 gap-6">
                  <KPICard
                    label="Total Users"
                    value={mockKPIData.totalUsers.toLocaleString()}
                    change="+1,245"
                    positive={true}
                    icon="👥"
                    tooltip="Total number of registered users on the platform. Shows overall reach and market penetration of your product."
                  />
                  <KPICard
                    label="Active Users"
                    value={mockKPIData.activeUsers.toLocaleString()}
                    change="+578"
                    positive={true}
                    icon="✅"
                    tooltip="Users who have engaged with the platform recently. Indicates active user base and product relevance."
                  />
                  <KPICard
                    label="DAU"
                    value={mockKPIData.dau.toLocaleString()}
                    change="+132"
                    positive={true}
                    icon="📱"
                    tooltip="Users who engage with the platform daily. Measures product stickiness and how essential your service is to users."
                  />
                  <KPICard
                    label="MAU"
                    value={mockKPIData.mau.toLocaleString()}
                    change="+578"
                    positive={true}
                    icon="🌐"
                    tooltip="Unique users who logged in within the last 30 days. Indicates overall platform health and user retention."
                  />
                </div>
              </div>

              {/* User Growth Chart with MAU/DAU Toggle */}
              <div className="bg-gray-800 rounded-lg border border-gray-700 p-6">
                <div className="flex justify-between items-center mb-6">
                  <h3 className="text-xl font-bold">User Growth Trend</h3>
                  
                  {/* Toggle Switch */}
                  <div className="flex items-center gap-2 bg-gray-700 rounded-lg p-1">
                    <button
                      onClick={() => setUserMetric('dau')}
                      className={`px-4 py-2 rounded-md text-sm font-semibold transition-all ${
                        userMetric === 'dau'
                          ? 'bg-blue-600 text-white'
                          : 'text-gray-400 hover:text-white'
                      }`}
                    >
                      DAU
                    </button>
                    <button
                      onClick={() => setUserMetric('mau')}
                      className={`px-4 py-2 rounded-md text-sm font-semibold transition-all ${
                        userMetric === 'mau'
                          ? 'bg-blue-600 text-white'
                          : 'text-gray-400 hover:text-white'
                      }`}
                    >
                      MAU
                    </button>
                  </div>
                </div>

                <div className="flex gap-4 pb-8" style={{ aspectRatio: '16 / 9' }}>
                  {/* Y-Axis Labels */}
                  <div className="flex flex-col justify-between text-right pr-3 border-r border-gray-700" style={{ height: 'calc(100% - 24px)' }}>
                    {(() => {
                      const currentData = userMetric === 'mau' ? userGrowth : dauTrend;
                      // Auto-fit labels to match plotted range
                      const allValues: number[] = [];
                      currentData.forEach((item: any) => {
                        allValues.push(item.mobile, item.desktop, item.website, item.mobile + item.desktop + item.website);
                      });
                      const dataMin = Math.min(...allValues);
                      const dataMax = Math.max(...allValues);
                      const range = dataMax - dataMin;
                      const minValue = Math.max(0, dataMin - range * 0.1);
                      const maxValue = dataMax + range * 0.1;
                      const roundTo500 = (val: number) => Math.round(val / 500) * 500;
                      return (
                        <>
                          <span className="text-sm text-gray-400">{roundTo500(maxValue).toLocaleString()}</span>
                          <span className="text-sm text-gray-400">{roundTo500(maxValue * 0.75 + minValue * 0.25).toLocaleString()}</span>
                          <span className="text-sm text-gray-400">{roundTo500(maxValue * 0.5 + minValue * 0.5).toLocaleString()}</span>
                          <span className="text-sm text-gray-400">{roundTo500(maxValue * 0.25 + minValue * 0.75).toLocaleString()}</span>
                          <span className="text-sm text-gray-400">{roundTo500(minValue).toLocaleString()}</span>
                        </>
                      );
                    })()}
                  </div>
                  
                  {/* Chart Area */}
                  <div className="flex-1">
                    <div className="h-full relative">
                      {/* Grid Lines */}
                      <div className="absolute inset-0 flex flex-col justify-between pointer-events-none" style={{ bottom: '24px' }}>
                        <div className="border-t border-gray-700/50"></div>
                        <div className="border-t border-gray-700/50"></div>
                        <div className="border-t border-gray-700/50"></div>
                        <div className="border-t border-gray-700/50"></div>
                        <div className="border-t border-gray-700"></div>
                      </div>
                      
                      <div className="absolute inset-0" style={{ bottom: '24px', overflow: 'hidden' }}>
                      {/* Line Graph */}
                      {(() => {
                        const currentData = userMetric === 'mau' ? userGrowth : dauTrend;
                        
                        // Auto-fit: Calculate min/max from all series data
                        const allValues: number[] = [];
                        currentData.forEach((item: any) => {
                          allValues.push(item.mobile, item.desktop, item.website, item.mobile + item.desktop + item.website);
                        });
                        const dataMin = Math.min(...allValues);
                        const dataMax = Math.max(...allValues);
                        
                        // Add 10% padding for better visualization
                        const range = dataMax - dataMin;
                        const minValue = Math.max(0, dataMin - range * 0.1);
                        const maxValue = dataMax + range * 0.1;
                        const denom = Math.max(1, maxValue - minValue);
                        
                        const padding = 3;
                        const usableWidth = 100 - (2 * padding);
                        const yPad = 0;
                        const clamp = (n: number, min: number, max: number) => Math.min(max, Math.max(min, n));

                        const colorBySeries: Record<'total' | 'mobile' | 'desktop' | 'website', string> = {
                          total: '#ffffff',
                          mobile: '#60a5fa',
                          desktop: '#ef4444',
                          website: '#f59e0b',
                        };

                        const getValue = (series: 'total' | 'mobile' | 'desktop' | 'website', item: any) => {
                          if (series === 'total') return item.mobile + item.desktop + item.website;
                          if (series === 'mobile') return item.mobile;
                          if (series === 'desktop') return item.desktop;
                          return item.website;
                        };

                        const buildPoints = (series: 'total' | 'mobile' | 'desktop' | 'website') => (
                          currentData.map((item: any, i: number) => {
                            const value = getValue(series, item);
                            const x = padding + (i / (currentData.length - 1)) * usableWidth;
                            const yRaw = 100 - (((value - minValue) / denom) * (100 - 2 * yPad)) - yPad;
                            const y = clamp(yRaw, yPad, 100 - yPad);
                            return `${x},${y}`;
                          }).join(' ')
                        );

                        const seriesOrder: Array<'total' | 'mobile' | 'desktop' | 'website'> = ['total', 'mobile', 'desktop', 'website'];

                        return (
                          <>
                            <svg className="absolute inset-0" style={{ zIndex: 20 }} preserveAspectRatio="none" viewBox="0 0 100 100">
                              <defs>
                                <clipPath id="userChartClipNexpo">
                                  <rect x="0" y="0" width="100" height="100" />
                                </clipPath>
                              </defs>
                              <g clipPath="url(#userChartClipNexpo)">
                              {seriesOrder.map((series) => (
                                <polyline
                                  key={series}
                                  points={buildPoints(series)}
                                  fill="none"
                                  stroke={colorBySeries[series]}
                                  strokeWidth="1.5"
                                  vectorEffect="non-scaling-stroke"
                                  pointerEvents="none"
                                />
                              ))}
                              {seriesOrder.map((series) => (
                                <g key={`dots-${series}`}>
                                  {currentData.map((item: any, i: number) => {
                                    const value = getValue(series, item);
                                    const x = padding + (i / (currentData.length - 1)) * usableWidth;
                                    const y = clamp(100 - (((value - minValue) / (maxValue - minValue)) * (100 - 2 * yPad)) - yPad, yPad, 100 - yPad);
                                    return (
                                      <circle
                                        key={`${series}-${i}`}
                                        cx={x}
                                        cy={y}
                                        r="0.7"
                                        fill={colorBySeries[series]}
                                        vectorEffect="non-scaling-stroke"
                                        pointerEvents="auto"
                                        style={{ cursor: 'pointer' }}
                                        onMouseEnter={() => setHoveredUserPoint({ series, index: i })}
                                        onMouseLeave={() => setHoveredUserPoint(null)}
                                      />
                                    );
                                  })}
                                </g>
                              ))}
                              </g>
                              {hoveredUserPoint !== null && (() => {
                                const { series, index } = hoveredUserPoint;
                                const item: any = currentData[index];
                                const value = getValue(series, item);
                                const x = padding + (index / (currentData.length - 1)) * usableWidth;
                                const y = clamp(100 - (((value - minValue) / (maxValue - minValue)) * (100 - 2 * yPad)) - yPad, yPad, 100 - yPad);
                                return (
                                  <foreignObject x={x - 15} y={y - 10} width="30" height="12" overflow="visible">
                                    <div style={{ position: 'absolute', transform: 'translate(-50%, -100%)', whiteSpace: 'nowrap', pointerEvents: 'none' }}>
                                      <div style={{ backgroundColor: '#000', color: '#fff', fontSize: '10px', lineHeight: 1, fontWeight: 600, padding: '2px 6px', borderRadius: 4 }}>
                                        {series.charAt(0).toUpperCase() + series.slice(1)}: {Math.round(value).toLocaleString()}
                                      </div>
                                    </div>
                                  </foreignObject>
                                );
                              })()}
                            </svg>
                          </>
                        );
                      })()}
                      </div>
                      
                      {/* X-Axis Labels */}
                      {(() => {
                        const currentData = userMetric === 'mau' ? userGrowth : dauTrend;
                        // Show fewer labels for DAU (30 data points) to avoid crowding
                        const labelInterval = userMetric === 'dau' ? 5 : 1;
                        return (
                          <div className="absolute left-0 right-0 bottom-0 h-6 flex justify-between px-2">
                            {currentData.map((item, i) => 
                              i % labelInterval === 0 ? (
                                <div key={i} className="text-xs text-gray-400 font-medium text-center">
                                  {item.month}
                                </div>
                              ) : null
                            )}
                          </div>
                        );
                      })()}
                    </div>
                  </div>
                </div>
                
                <div className="mt-2 flex items-center justify-center gap-4 text-xs text-gray-300">
                  <span className="flex items-center gap-2"><span className="inline-block w-3 h-1.5 rounded-full" style={{ backgroundColor: '#ffffff' }}></span> Total</span>
                  <span className="flex items-center gap-2"><span className="inline-block w-3 h-1.5 rounded-full" style={{ backgroundColor: '#60a5fa' }}></span> Mobile</span>
                  <span className="flex items-center gap-2"><span className="inline-block w-3 h-1.5 rounded-full" style={{ backgroundColor: '#ef4444' }}></span> Desktop</span>
                  <span className="flex items-center gap-2"><span className="inline-block w-3 h-1.5 rounded-full" style={{ backgroundColor: '#f59e0b' }}></span> Website</span>
                </div>
                
                {/* Metric Label */}
                <div className="mt-4 pt-4 border-t border-gray-700 text-center">
                  <span className="text-sm text-gray-400">
                    Showing <span className={`font-semibold ${userMetric === 'mau' ? 'text-blue-400' : 'text-blue-400'}`}>
                      {userMetric === 'mau' ? 'Monthly Active Users' : 'Daily Active Users'}
                    </span> trend
                  </span>
                </div>
              </div>
            </div>
          )}

          {/* Looker Studio Tab */}
          {activeTab === 'looker' && (
            <div className="space-y-6">
              <div className="bg-gray-800 rounded-lg border border-gray-700 p-6">
                <div className="flex items-center justify-between mb-4">
                  <h2 className="text-2xl font-bold">Looker Studio Dashboard</h2>
                  <button
                    onClick={() => window.open('https://lookerstudio.google.com', '_blank')}
                    className="px-4 py-2 bg-blue-600 hover:bg-blue-700 rounded-lg transition-colors"
                  >
                    Open in New Tab
                  </button>
                </div>
                <p className="text-gray-400 mb-6">
                  Interactive business intelligence dashboard powered by Google Looker Studio.
                  Replace the URL below with your actual Looker Studio embed link.
                </p>
                
                {/* Looker Studio Embed */}
                <div className="bg-gray-900 rounded-lg overflow-hidden" style={{ height: '800px' }}>
                  <iframe
                    src={LOOKER_STUDIO_URL}
                    width="100%"
                    height="100%"
                    style={{ border: 0 }}
                    allowFullScreen
                    title="Looker Studio BI Dashboard"
                  />
                </div>
                
                <div className="mt-4 p-4 bg-blue-900/30 border border-blue-700 rounded-lg">
                  <p className="text-sm text-blue-300">
                    <strong>Note:</strong> To use a real Looker Studio dashboard:
                  </p>
                  <ol className="text-sm text-blue-200 mt-2 space-y-1 list-decimal list-inside">
                    <li>Create a dashboard in Looker Studio</li>
                    <li>Click "Share" → "Embed"</li>
                    <li>Copy the embed URL</li>
                    <li>Update LOOKER_STUDIO_URL in @weave/bi-core/config</li>
                  </ol>
                </div>
              </div>
            </div>
          )}
        </div>
      </div>
    </>
  );
}

// KPI Card Component
interface KPICardProps {
  label: string;
  value: string;
  change: string;
  positive: boolean;
  icon: string;
  tooltip?: string;
  size?: 'normal' | 'large';
}

function KPICard({ label, value, change, positive, icon, tooltip, size = 'normal' }: KPICardProps) {
  return (
    <div className="bg-gray-800 p-6 rounded-lg border border-gray-700 hover:border-gray-600 transition-colors">
      <div className="flex items-center justify-between mb-3">
        <div className="text-gray-400 text-sm">{label}</div>
        <div className="relative group">
          <span className="text-2xl cursor-help">{icon}</span>
          {tooltip && (
            <div className="absolute right-0 top-8 w-64 p-3 bg-black text-white text-xs rounded-lg shadow-lg opacity-0 invisible group-hover:opacity-100 group-hover:visible transition-all duration-200 z-50 pointer-events-none">
              {tooltip}
              <div className="absolute -top-1 right-4 w-2 h-2 bg-black transform rotate-45"></div>
            </div>
          )}
        </div>
      </div>
      <div className={`font-bold mb-2 ${
        size === 'large' ? 'text-4xl' : 'text-3xl'
      }`}>{value}</div>
      <div className={`text-sm font-semibold ${
        positive ? 'text-green-400' : 'text-red-400'
      }`}>
        {change}
      </div>
    </div>
  );
}
