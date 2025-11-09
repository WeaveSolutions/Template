<script lang="ts">
  // ✅ Using shared BI core!
  import { 
    mockKPIData,
    mockMRRTrend,
    mockARRTrend,
    mockUserGrowth,
    mockDAUTrend,
    mockSubscriptionBreakdown,
    type KPIData,
    type TrendData,
    type UserGrowthData,
    type SubscriptionBreakdown,
    type TabType
  } from '@weave/bi-core';
  
  import { LOOKER_STUDIO_URL } from '@weave/bi-core/config';

  // State
  let activeTab: TabType = $state('overview');
  let revenueMetric: 'mrr' | 'arr' = $state('mrr');
  let userMetric: 'mau' | 'dau' = $state('dau');
  let hoveredRevenuePoint: number | null = $state(null);
  type UserSeries = 'total' | 'mobile' | 'desktop' | 'website';
  let hoveredUserPoint: { series: UserSeries; index: number } | null = $state(null);
  let kpiData: KPIData = $state(mockKPIData);
  let mrrTrend: TrendData[] = $state(mockMRRTrend);
  let arrTrend: TrendData[] = $state(mockARRTrend);
  let userGrowth: UserGrowthData[] = $state(mockUserGrowth);
  let dauTrend: TrendData[] = $state(mockDAUTrend);
  let subscriptionBreakdown: SubscriptionBreakdown[] = $state(mockSubscriptionBreakdown);

  // Computed values for chart scaling
  const roundTo500 = (val: number) => Math.round(val / 500) * 500;
  const clamp = (n: number, min: number, max: number) => Math.min(max, Math.max(min, n));

  // Helper function to get X-axis labels
  // For MRR: returns month names (Jan, Feb, etc.)
  // For ARR: returns the year directly from the data (2015, 2016, etc.)
  function getXAxisLabel(month: string): string {
    return month; // ARR data already contains years
  }

  function openLookerStudio() {
    window.open('https://lookerstudio.google.com', '_blank');
  }
</script>

<div class="dashboard-container">
  <!-- Header -->
  <div class="header">
    <h1 class="title">Business Intelligence Dashboard</h1>
    <p class="subtitle">Real-time insights and key performance indicators</p>
  </div>

  <!-- Tab Navigation -->
  <div class="tabs">
    <button
      class="tab"
      class:active={activeTab === 'overview'}
      onclick={() => activeTab = 'overview'}
    >
      📊 Overview
    </button>
    <button
      class="tab"
      class:active={activeTab === 'revenue'}
      onclick={() => activeTab = 'revenue'}
    >
      💰 Revenue
    </button>
    <button
      class="tab"
      class:active={activeTab === 'users'}
      onclick={() => activeTab = 'users'}
    >
      👥 Users
    </button>
    <button
      class="tab"
      class:active={activeTab === 'looker'}
      onclick={() => activeTab = 'looker'}
    >
      📈 Looker Studio
    </button>
  </div>

  <!-- Overview Tab -->
  {#if activeTab === 'overview'}
    <div class="tab-content">
      <!-- Executive KPIs -->
      <section class="section">
        <h2 class="section-title">Executive KPIs</h2>
        <div class="kpi-grid-4">
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">Monthly Recurring Revenue</span>
              <span class="kpi-icon" data-tooltip="Predictable revenue generated each month from subscriptions. Essential for forecasting growth and measuring business stability.">💰</span>
            </div>
            <div class="kpi-value">${kpiData.mrr.toLocaleString()}</div>
            <div class="kpi-change positive">+8.5%</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">Annual Recurring Revenue</span>
              <span class="kpi-icon" data-tooltip="Total value of recurring subscription revenue normalized to a year. Key metric for valuation and long-term planning.">📈</span>
            </div>
            <div class="kpi-value">${kpiData.arr.toLocaleString()}</div>
            <div class="kpi-change positive">+8.5%</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">Total Users</span>
              <span class="kpi-icon" data-tooltip="Total number of registered users on the platform. Shows overall reach and market penetration of your product.">👥</span>
            </div>
            <div class="kpi-value">{kpiData.totalUsers.toLocaleString()}</div>
            <div class="kpi-change positive">+12.3%</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">Active Subscriptions</span>
              <span class="kpi-icon" data-tooltip="Number of paying subscribers currently active. Directly correlates to revenue and shows product-market fit.">✅</span>
            </div>
            <div class="kpi-value">{kpiData.activeSubscriptions.toLocaleString()}</div>
            <div class="kpi-change positive">+5.2%</div>
          </div>
        </div>
      </section>

      <!-- User Engagement -->
      <section class="section">
        <h2 class="section-title">User Engagement</h2>
        <div class="kpi-grid-3">
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">Daily Active Users</span>
              <span class="kpi-icon" data-tooltip="Users who engage with the platform daily. Measures product stickiness and how essential your service is to users.">📱</span>
            </div>
            <div class="kpi-value">{kpiData.dau.toLocaleString()}</div>
            <div class="kpi-change positive">+4.2%</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">Monthly Active Users</span>
              <span class="kpi-icon" data-tooltip="Unique users who logged in within the last 30 days. Indicates overall platform health and user retention.">🌐</span>
            </div>
            <div class="kpi-value">{kpiData.mau.toLocaleString()}</div>
            <div class="kpi-change positive">+6.8%</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">DAU/MAU Ratio</span>
              <span class="kpi-icon" data-tooltip="Percentage of monthly users who engage daily. Higher ratios indicate stronger user habits and product engagement.">📊</span>
            </div>
            <div class="kpi-value">{((kpiData.dau / kpiData.mau) * 100).toFixed(1)}%</div>
            <div class="kpi-change positive">Stable</div>
          </div>
        </div>
      </section>

      <!-- Financial Health -->
      <section class="section">
        <h2 class="section-title">Financial Health</h2>
        <div class="kpi-grid-4">
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">ARPU</span>
              <span class="kpi-icon" data-tooltip="Average Revenue Per User. Shows how much revenue each user generates on average. Key for pricing strategy.">💵</span>
            </div>
            <div class="kpi-value">${kpiData.arpu.toFixed(2)}</div>
            <div class="kpi-change positive">+2.1%</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">Customer LTV</span>
              <span class="kpi-icon" data-tooltip="Lifetime Value: total revenue expected from a customer over their entire relationship. Critical for acquisition budgets.">🎯</span>
            </div>
            <div class="kpi-value">${kpiData.ltv.toFixed(2)}</div>
            <div class="kpi-change positive">+5.5%</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">CAC</span>
              <span class="kpi-icon" data-tooltip="Customer Acquisition Cost: total marketing and sales spend to acquire one customer. Lower is better for profitability.">📉</span>
            </div>
            <div class="kpi-value">${kpiData.cac.toFixed(2)}</div>
            <div class="kpi-change positive">-3.2%</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">LTV:CAC Ratio</span>
              <span class="kpi-icon" data-tooltip="Ratio of customer lifetime value to acquisition cost. 3:1 or higher is ideal. Shows sustainable growth economics.">⚖️</span>
            </div>
            <div class="kpi-value">{kpiData.ltvCacRatio.toFixed(2)}</div>
            <div class="kpi-change positive">Healthy</div>
          </div>
        </div>
      </section>

      <!-- Additional Metrics -->
      <section class="section">
        <h2 class="section-title">Additional Metrics</h2>
        <div class="kpi-grid-3">
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">Churn Rate</span>
              <span class="kpi-icon" data-tooltip="Percentage of customers who cancel their subscription. Lower churn means better retention and predictable revenue.">📉</span>
            </div>
            <div class="kpi-value">{kpiData.churnRate}%</div>
            <div class="kpi-change positive">-0.5%</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">Net Promoter Score</span>
              <span class="kpi-icon" data-tooltip="Measures customer satisfaction and loyalty. Scores above 50 are excellent. Higher scores correlate with organic growth.">⭐</span>
            </div>
            <div class="kpi-value">{kpiData.nps}</div>
            <div class="kpi-change positive">+3 pts</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">System Uptime</span>
              <span class="kpi-icon" data-tooltip="Percentage of time the system is operational. 99.9% (three nines) is considered excellent for SaaS platforms.">✅</span>
            </div>
            <div class="kpi-value">{kpiData.uptime}%</div>
            <div class="kpi-change positive">+0.02%</div>
          </div>
        </div>
      </section>
    </div>
  {/if}

  <!-- Revenue Tab -->
  {#if activeTab === 'revenue'}
    <div class="tab-content">
      <!-- Revenue KPIs -->
      <section class="section">
        <h2 class="section-title">Revenue Overview</h2>
        <div class="kpi-grid-3">
          <div class="kpi-card large">
            <div class="kpi-header">
              <span class="kpi-label">MRR</span>
              <span class="kpi-icon" data-tooltip="Predictable revenue generated each month from subscriptions. Essential for forecasting growth and measuring business stability.">💰</span>
            </div>
            <div class="kpi-value-large">${kpiData.mrr.toLocaleString()}</div>
            <div class="kpi-change positive">+$7,250</div>
          </div>
          
          <div class="kpi-card large">
            <div class="kpi-header">
              <span class="kpi-label">ARR</span>
              <span class="kpi-icon" data-tooltip="Total value of recurring subscription revenue normalized to a year. Key metric for valuation and long-term planning.">📈</span>
            </div>
            <div class="kpi-value-large">${kpiData.arr.toLocaleString()}</div>
            <div class="kpi-change positive">+$87,000</div>
          </div>
          
          <div class="kpi-card large">
            <div class="kpi-header">
              <span class="kpi-label">ARPU</span>
              <span class="kpi-icon" data-tooltip="Average Revenue Per User. Shows how much revenue each user generates on average. Key for pricing strategy.">💵</span>
            </div>
            <div class="kpi-value-large">${kpiData.arpu.toFixed(2)}</div>
            <div class="kpi-change positive">+$0.23</div>
          </div>
        </div>
      </section>

      <!-- Revenue Trend Chart with MRR/ARR Toggle -->
      <section class="chart-section">
        <div class="chart-header-with-toggle">
          <h3 class="chart-title">Revenue Trend</h3>
          
          <!-- Toggle Switch -->
          <div class="toggle-group">
            <button
              class="toggle-btn"
              class:active={revenueMetric === 'mrr'}
              onclick={() => revenueMetric = 'mrr'}
            >
              MRR
            </button>
            <button
              class="toggle-btn"
              class:active={revenueMetric === 'arr'}
              onclick={() => revenueMetric = 'arr'}
            >
              ARR
            </button>
          </div>
        </div>

        {#key revenueMetric}
          {@const currentTrend = revenueMetric === 'mrr' ? mrrTrend : arrTrend.slice(2)}
          
          {@const dataMin = Math.min(...currentTrend.map(d => d.value))}
          {@const dataMax = Math.max(...currentTrend.map(d => d.value))}
          {@const padRange = dataMax - dataMin}
          {@const paddedMin = Math.max(0, dataMin - padRange * 0.1)}
          {@const paddedMax = dataMax + padRange * 0.1}
          
          {@const roundTo = revenueMetric === 'mrr' ? 500 : 10000}
          {@const tickStep = Math.max(roundTo, Math.ceil(((paddedMax - paddedMin) / 4) / roundTo) * roundTo)}
          {@const snapMin = Math.floor(paddedMin / roundTo) * roundTo}
          {@const snapMax = snapMin + tickStep * 4}
          {@const denom = Math.max(1, snapMax - snapMin)}
          
          {@const revLabels = [
            `$${(snapMax).toLocaleString()}`,
            `$${(snapMax - tickStep).toLocaleString()}`,
            `$${(snapMax - tickStep * 2).toLocaleString()}`,
            `$${(snapMax - tickStep * 3).toLocaleString()}`,
            `$${(snapMin).toLocaleString()}`
          ]}
          {@const padding = 3}
          {@const usableWidth = 100 - (2 * padding)}
          {@const yPad = 0}
          {@const linePoints = currentTrend.map((item, i) => {
            const x = padding + (i / (currentTrend.length - 1)) * usableWidth;
            const yRaw = 100 - (((item.value - snapMin) / denom) * (100 - 2 * yPad)) - yPad;
            const y = clamp(yRaw, yPad, 100 - yPad);
            return `${x},${y}`;
          }).join(' ')}

          <div class="chart-with-axis">
            <!-- Y-Axis Labels -->
            <div class="y-axis" style="height: calc(100% - 24px);">
              {#each revLabels as label}
                <span class="y-axis-label">{label}</span>
              {/each}
            </div>
            
            <!-- Chart Area -->
            <div class="chart-area" style="position: relative;">
              <div class="chart-grid" style="bottom: 24px;">
              {#each Array(5) as _}
                <div class="grid-line"></div>
              {/each}
            </div>
              
              <!-- Trimmed plot area to reserve space for x-axis labels -->
              <div class="absolute inset-0" style="bottom: 24px; overflow: hidden;" role="presentation" onmouseleave={() => hoveredRevenuePoint = null}>
              <!-- Line Graph -->
              <svg class="line-graph-overlay" preserveAspectRatio="none" viewBox="0 0 100 100">
                <defs>
                  <clipPath id="revenueChartClip">
                    <rect x="0" y="0" width="100" height="100" />
                  </clipPath>
                </defs>
                <g clip-path="url(#revenueChartClip)">
                <polyline
                  points={linePoints}
                  fill="none"
                  stroke="#60a5fa"
                  stroke-width="1.5"
                  vector-effect="non-scaling-stroke"
                  pointer-events="none"
                />
                {#each currentTrend as item, i}
                  {@const x = padding + (i / (currentTrend.length - 1)) * usableWidth}
                  {@const y = clamp(100 - (((item.value - snapMin) / denom) * (100 - 2 * yPad)) - yPad, yPad, 100 - yPad)}
                  <g>
                    <circle
                      cx={x}
                      cy={y}
                      r="0.7"
                      fill="#60a5fa"
                      vector-effect="non-scaling-stroke"
                      pointer-events="auto"
                      onmouseenter={() => hoveredRevenuePoint = i}
                      onmouseleave={() => hoveredRevenuePoint = null}
                      style="cursor: pointer;"
                      role="button"
                      tabindex="0"
                      aria-label={`Data point ${i + 1}: $${item.value.toLocaleString()}`}
                    />
                  </g>
                {/each}
                </g>
                {#if hoveredRevenuePoint !== null}
                  {@const i = hoveredRevenuePoint}
                  {@const x = padding + (i / (currentTrend.length - 1)) * usableWidth}
                  {@const y = clamp(100 - (((currentTrend[i].value - snapMin) / denom) * (100 - 2 * yPad)) - yPad, yPad, 100 - yPad)}
                  <foreignObject x={x - 15} y={y - 10} width="30" height="12" overflow="visible">
                    <div style="position: absolute; transform: translate(-50%, -100%); white-space: nowrap; pointer-events: none;">
                      <div style="background-color: #000000; color: #ffffff; font-size: 10px; line-height: 1; font-weight: 600; padding: 2px 6px; border-radius: 4px;">
                        ${currentTrend[i].value.toLocaleString()}
                      </div>
                    </div>
                  </foreignObject>
                {/if}
              </svg>
              </div>
              
              <!-- X-Axis Labels aligned to plot coordinates -->
              <div style="position: absolute; left: 0; right: 0; bottom: 0; height: 24px;">
                {#each currentTrend as item, i}
                  {@const x = padding + (i / (currentTrend.length - 1)) * usableWidth}
                  {@const xLabel = getXAxisLabel(item.month)}
                  <div class="text-xs text-gray-400 font-medium text-center" style="position: absolute; left: {x}%; transform: translateX(-50%); bottom: 0; white-space: nowrap;">{xLabel}</div>
                {/each}
              </div>
            </div>
          </div>
          
          <!-- Metric Label -->
          <div class="metric-label">
            Showing <span class="metric-highlight">{revenueMetric === 'mrr' ? 'Monthly' : 'Annual'} Recurring Revenue</span> trend
          </div>
        {/key}
      </section>

      <!-- Subscription Breakdown -->
      <section class="chart-section">
        <h3 class="chart-title">Subscription Plan Distribution</h3>
        <div class="progress-list">
          {#each subscriptionBreakdown as sub}
            <div class="progress-item">
              <div class="progress-header">
                <span class="progress-label">{sub.plan}</span>
                <span class="progress-value">{sub.count.toLocaleString()} ({sub.percentage}%)</span>
              </div>
              <div class="progress-bar-bg">
                <div 
                  class="progress-bar-fill {sub.plan.toLowerCase()}"
                  style="width: {sub.percentage}%"
                ></div>
              </div>
            </div>
          {/each}
        </div>
      </section>
    </div>
  {/if}

  <!-- Users Tab -->
  {#if activeTab === 'users'}
    <div class="tab-content">
      <!-- User KPIs -->
      <section class="section">
        <h2 class="section-title">User Metrics</h2>
        <div class="kpi-grid-4">
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">Total Users</span>
              <span class="kpi-icon" data-tooltip="Total number of registered users on the platform. Shows overall reach and market penetration of your product.">👥</span>
            </div>
            <div class="kpi-value">{kpiData.totalUsers.toLocaleString()}</div>
            <div class="kpi-change positive">+1,245</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">Active Users</span>
              <span class="kpi-icon" data-tooltip="Users who have engaged with the platform recently. Indicates active user base and product relevance.">✅</span>
            </div>
            <div class="kpi-value">{kpiData.activeUsers.toLocaleString()}</div>
            <div class="kpi-change positive">+578</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">DAU</span>
              <span class="kpi-icon" data-tooltip="Users who engage with the platform daily. Measures product stickiness and how essential your service is to users.">📱</span>
            </div>
            <div class="kpi-value">{kpiData.dau.toLocaleString()}</div>
            <div class="kpi-change positive">+132</div>
          </div>
          
          <div class="kpi-card">
            <div class="kpi-header">
              <span class="kpi-label">MAU</span>
              <span class="kpi-icon" data-tooltip="Unique users who logged in within the last 30 days. Indicates overall platform health and user retention.">🌐</span>
            </div>
            <div class="kpi-value">{kpiData.mau.toLocaleString()}</div>
            <div class="kpi-change positive">+578</div>
          </div>
        </div>
      </section>

      <!-- User Growth Chart with MAU/DAU Toggle -->
      <section class="chart-section">
        <div class="chart-header-with-toggle">
          <h3 class="chart-title">User Growth Trend</h3>
          
          <!-- Toggle Switch -->
          <div class="toggle-group">
            <button
              class="toggle-btn"
              class:active={userMetric === 'dau'}
              onclick={() => userMetric = 'dau'}
            >
              DAU
            </button>
            <button
              class="toggle-btn"
              class:active={userMetric === 'mau'}
              onclick={() => userMetric = 'mau'}
            >
              MAU
            </button>
          </div>
        </div>

        {#key userMetric}
          {@const currentData = userMetric === 'mau' ? userGrowth : dauTrend}
          
          {@const allValues = currentData.flatMap((item: any) => [
            item.mobile,
            item.desktop,
            item.website,
            item.mobile + item.desktop + item.website
          ])}
          {@const dataMin = Math.min(...allValues)}
          {@const dataMax = Math.max(...allValues)}
          {@const range = dataMax - dataMin}
          {@const minValue = Math.max(0, dataMin - range * 0.1)}
          {@const maxValue = dataMax + range * 0.1}
          
          {@const userYAxisLabels = [
            roundTo500(maxValue),
            roundTo500(maxValue * 0.75 + minValue * 0.25),
            roundTo500(maxValue * 0.5 + minValue * 0.5),
            roundTo500(maxValue * 0.25 + minValue * 0.75),
            roundTo500(minValue)
          ]}
          {@const userPadding = 3}
          {@const userUsableWidth = 100 - (2 * userPadding)}
          {@const userYPad = 6}
          {@const colorBySeries: Record<UserSeries, string> = ({ total: '#ffffff', mobile: '#60a5fa', desktop: '#ef4444', website: '#f59e0b' })}
          {@const getSeriesValue = (series: UserSeries, item: any) => {
            if (series === 'total') return item.mobile + item.desktop + item.website;
            if (series === 'mobile') return item.mobile;
            if (series === 'desktop') return item.desktop;
            return item.website;
          }}
          {@const seriesOrder: UserSeries[] = ['total','mobile','desktop','website']}

          <div class="chart-with-axis" style="padding-bottom: 24px;">
            <!-- Y-Axis Labels -->
            <div class="y-axis" style="height: calc(100% - 24px);">
              {#each userYAxisLabels as label}
                <span class="y-axis-label">{label.toLocaleString()}</span>
              {/each}
            </div>
            
            <!-- Chart Area -->
            <div class="chart-area" style="position: relative;">
              <div class="chart-grid" style="bottom: 24px;">
                {#each Array(5) as _}
                  <div class="grid-line"></div>
                {/each}
              </div>
              
              <!-- Trimmed plot area to reserve space for x-axis labels -->
              <div class="absolute inset-0" style="bottom: 24px; overflow: hidden;" role="presentation" onmouseleave={() => hoveredUserPoint = null}>
              <!-- Line Graph -->
              <svg class="line-graph-overlay" preserveAspectRatio="none" viewBox="0 0 100 100" style="pointer-events: auto;">
                <defs>
                  <clipPath id="userChartClip">
                    <rect x="0" y="0" width="100" height="92" />
                  </clipPath>
                </defs>
                <g clip-path="url(#userChartClip)">
                {#each seriesOrder as series (series)}
                  {@const points = currentData.map((item, i) => {
                    const value = getSeriesValue(series as UserSeries, item);
                    const x = userPadding + (i / (currentData.length - 1)) * userUsableWidth;
                    const yRaw = 100 - (((value - minValue) / (maxValue - minValue)) * (100 - 2 * userYPad)) - userYPad;
                    const y = clamp(yRaw, userYPad, 100 - userYPad);
                    return `${x},${y}`;
                  }).join(' ')}
                  <polyline
                    points={points}
                    fill="none"
                    stroke={colorBySeries[series as UserSeries]}
                    stroke-width="1.5"
                    vector-effect="non-scaling-stroke"
                    pointer-events="none"
                  />
                {/each}
                {#each seriesOrder as series (series)}
                  {#each currentData as item, i}
                    {@const value = getSeriesValue(series as UserSeries, item)}
                    {@const x = userPadding + (i / (currentData.length - 1)) * userUsableWidth}
                    {@const y = clamp(100 - (((value - minValue) / (maxValue - minValue)) * (100 - 2 * userYPad)) - userYPad, userYPad, 100 - userYPad)}
                    <circle
                      cx={x}
                      cy={y}
                      r="0.7"
                      fill={colorBySeries[series as UserSeries]}
                      vector-effect="non-scaling-stroke"
                      pointer-events="auto"
                      onmouseenter={() => hoveredUserPoint = { series: series as UserSeries, index: i }}
                      onmouseleave={() => hoveredUserPoint = null}
                      style="cursor: pointer;"
                      role="button"
                      tabindex="0"
                      aria-label={`${series} data point ${i + 1}`}
                    />
                  {/each}
                {/each}
                </g>
                {#if hoveredUserPoint !== null}
                  {@const i = hoveredUserPoint.index}
                  {@const series = hoveredUserPoint.series}
                  {@const item = currentData[i]}
                  {@const value = getSeriesValue(series, item)}
                  {@const x = userPadding + (i / (currentData.length - 1)) * userUsableWidth}
                  {@const y = clamp(100 - (((value - minValue) / (maxValue - minValue)) * (100 - 2 * userYPad)) - userYPad, userYPad, 100 - userYPad)}
                  <foreignObject x={x - 10} y={y - 8} width="20" height="8" overflow="visible">
                    <div style="position: absolute; transform: translate(-50%, -100%); white-space: nowrap; pointer-events: none;">
                      <div class="px-3 py-1.5 text-white text-xs font-semibold rounded shadow-lg" style="background-color: #000000;">
                        {series.charAt(0).toUpperCase() + series.slice(1)}: {Math.round(value).toLocaleString()}
                      </div>
                    </div>
                  </foreignObject>
                {/if}
              </svg>
              </div>
              
              <!-- X-Axis Labels aligned to plot coordinates -->
              <div style="position: absolute; left: 0; right: 0; bottom: 0; height: 24px;">
                {#each currentData as item, i}
                  {#if userMetric === 'dau' ? i % 5 === 0 : true}
                    {@const x = userPadding + (i / (currentData.length - 1)) * userUsableWidth}
                    <div class="text-xs text-gray-400 font-medium text-center" style="position: absolute; left: {x}%; transform: translateX(-50%); bottom: 0; white-space: nowrap;">{item.month}</div>
                  {/if}
                {/each}
              </div>
            </div>
          </div>
          
          <!-- Users Legend -->
          <div style="margin-top: 0.5rem; display: flex; align-items: center; justify-content: center; gap: 1rem; font-size: 0.75rem; color: #d1d5db; pointer-events: none;">
            <span style="display: inline-flex; align-items: center; gap: 0.5rem;"><span style="display:inline-block; width: 12px; height: 6px; border-radius: 9999px; background: #ffffff;"></span> Total</span>
            <span style="display: inline-flex; align-items: center; gap: 0.5rem;"><span style="display:inline-block; width: 12px; height: 6px; border-radius: 9999px; background: #60a5fa;"></span> Mobile</span>
            <span style="display: inline-flex; align-items: center; gap: 0.5rem;"><span style="display:inline-block; width: 12px; height: 6px; border-radius: 9999px; background: #ef4444;"></span> Desktop</span>
            <span style="display: inline-flex; align-items: center; gap: 0.5rem;"><span style="display:inline-block; width: 12px; height: 6px; border-radius: 9999px; background: #f59e0b;"></span> Website</span>
          </div>

          <!-- Metric Label -->
          <div class="metric-label">
            Showing <span class="metric-highlight" class:mau-color={userMetric === 'mau'} class:dau-color={userMetric === 'dau'}>
              {userMetric === 'mau' ? 'Monthly Active Users' : 'Daily Active Users'}
            </span> trend
          </div>
        {/key}
      </section>
    </div>
  {/if}

  <!-- Looker Studio Tab -->
  {#if activeTab === 'looker'}
    <div class="tab-content">
      <section class="looker-section">
        <div class="looker-header">
          <h2 class="section-title">Looker Studio Dashboard</h2>
          <button class="btn-primary" onclick={openLookerStudio}>
            Open in New Tab
          </button>
        </div>
        <p class="looker-description">
          Interactive business intelligence dashboard powered by Google Looker Studio.
          Replace the URL below with your actual Looker Studio embed link.
        </p>
        
        <!-- Looker Studio Embed -->
        <div class="looker-embed">
          <iframe
            src={LOOKER_STUDIO_URL}
            width="100%"
            height="800"
            style="border: 0; border-radius: 8px;"
            allowfullscreen
            title="Looker Studio BI Dashboard"
          ></iframe>
        </div>
        
        <div class="looker-note">
          <p><strong>Note:</strong> To use a real Looker Studio dashboard:</p>
          <ol>
            <li>Create a dashboard in Looker Studio</li>
            <li>Click "Share" → "Embed"</li>
            <li>Copy the embed URL</li>
            <li>Update LOOKER_STUDIO_URL in @weave/bi-core/config</li>
          </ol>
        </div>
      </section>
    </div>
  {/if}
</div>

<style>
  .dashboard-container {
    max-width: 1400px;
    margin: 0 auto;
    padding: 2rem;
    background: linear-gradient(to bottom, #111827 0%, #1f2937 100%);
    min-height: 100vh;
    color: #ffffff;
  }

  /* Header */
  .header {
    margin-bottom: 2rem;
  }

  .title {
    font-size: 2.25rem;
    font-weight: 700;
    color: #ffffff;
    margin-bottom: 0.5rem;
  }

  .subtitle {
    color: #9ca3af;
    font-size: 1rem;
  }

  /* Tabs */
  .tabs {
    display: flex;
    gap: 0.5rem;
    margin-bottom: 2rem;
    overflow-x: auto;
    padding-bottom: 0.5rem;
  }

  .tab {
    padding: 0.75rem 1.5rem;
    background: #1f2937;
    border: none;
    border-radius: 0.5rem;
    color: #9ca3af;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.2s;
    white-space: nowrap;
  }

  .tab:hover {
    background: #374151;
    color: #ffffff;
  }

  .tab.active {
    background: #2563eb;
    color: white;
  }

  /* Sections */
  .section {
    margin-bottom: 3rem;
  }

  .section-title {
    font-size: 1.5rem;
    font-weight: 700;
    color: #ffffff;
    margin-bottom: 1.5rem;
  }

  /* KPI Cards */
  .kpi-grid-3 {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(280px, 1fr));
    gap: 1.5rem;
  }

  .kpi-grid-4 {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(240px, 1fr));
    gap: 1.5rem;
  }

  .kpi-card {
    background: #1f2937;
    border: 1px solid #374151;
    border-radius: 0.5rem;
    padding: 1.5rem;
    transition: all 0.2s;
  }

  .kpi-card:hover {
    border-color: #4b5563;
    transform: translateY(-2px);
  }

  .kpi-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 0.75rem;
  }

  .kpi-label {
    font-size: 0.875rem;
    color: #9ca3af;
    font-weight: 500;
  }

  .kpi-icon {
    font-size: 1.5rem;
    position: relative;
    cursor: help;
  }

  .kpi-icon[data-tooltip]:hover::after {
    content: attr(data-tooltip);
    position: absolute;
    right: 0;
    top: 2rem;
    width: 16rem;
    padding: 0.75rem;
    background: #000000;
    color: #ffffff;
    font-size: 0.75rem;
    border-radius: 0.5rem;
    box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.5);
    z-index: 50;
    pointer-events: none;
    font-weight: 400;
    line-height: 1.4;
  }

  .kpi-icon[data-tooltip]:hover::before {
    content: "";
    position: absolute;
    right: 1rem;
    top: 1.75rem;
    width: 0.5rem;
    height: 0.5rem;
    background: #000000;
    transform: rotate(45deg);
    z-index: 51;
  }

  .kpi-value {
    font-size: 1.875rem;
    font-weight: 700;
    color: #ffffff;
    margin-bottom: 0.5rem;
  }

  .kpi-value-large {
    font-size: 2.5rem;
    font-weight: 700;
    color: #ffffff;
    margin-bottom: 0.5rem;
  }

  .kpi-change {
    font-size: 0.875rem;
    font-weight: 600;
  }

  .kpi-change.positive {
    color: #10b981;
  }

  .kpi-change.negative {
    color: #ef4444;
  }

  /* Charts */
  .chart-section {
    background: #1f2937;
    border: 1px solid #374151;
    border-radius: 0.5rem;
    padding: 2rem;
    margin-bottom: 2rem;
  }

  .chart-title {
    font-size: 1.25rem;
    font-weight: 600;
    color: #ffffff;
    margin-bottom: 0;
  }

  .chart-header-with-toggle {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1.5rem;
  }

  /* Toggle Switch */
  .toggle-group {
    display: flex;
    align-items: center;
    gap: 0.25rem;
    background: #374151;
    border-radius: 0.5rem;
    padding: 0.25rem;
  }

  .toggle-btn {
    padding: 0.5rem 1rem;
    border-radius: 0.375rem;
    font-size: 0.875rem;
    font-weight: 600;
    color: #9ca3af;
    background: transparent;
    border: none;
    cursor: pointer;
    transition: all 0.2s;
  }

  .toggle-btn:hover {
    color: #ffffff;
  }

  .toggle-btn.active {
    background: #2563eb;
    color: #ffffff;
  }

  /* Metric Label */
  .metric-label {
    margin-top: 1rem;
    padding-top: 1rem;
    border-top: 1px solid #374151;
    text-align: center;
    font-size: 0.875rem;
    color: #9ca3af;
  }

  .metric-highlight {
    font-weight: 600;
  }

  .metric-highlight.mau-color {
    color: #60a5fa;
  }

  .metric-highlight.dau-color {
    color: #60a5fa;
  }

  /* Improved Chart with Y-Axis */
  .chart-with-axis {
    display: flex;
    gap: 1rem;
    aspect-ratio: 16 / 9;
  }

  .y-axis {
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    height: 100%;
    text-align: right;
    padding-right: 0.75rem;
    border-right: 1px solid #374151;
  }

  .y-axis-label {
    font-size: 0.875rem;
    color: #9ca3af;
  }

  .chart-area {
    flex: 1;
    position: relative;
    height: 100%;
    overflow: visible;
  }

  .chart-grid {
    position: absolute;
    inset: 0;
    display: flex;
    flex-direction: column;
    justify-content: space-between;
    pointer-events: none;
  }

  .grid-line {
    border-top: 1px solid #374151;
    opacity: 0.5;
  }

  .grid-line:last-child {
    opacity: 1;
  }

  .line-graph-overlay {
    position: absolute;
    inset: 0;
    pointer-events: none;
    z-index: 20;
  }

  .bar-chart-improved {
    height: 100%;
    display: flex;
    align-items: flex-end;
    justify-content: space-between;
    gap: 0.75rem;
    position: relative;
    z-index: 10;
  }

  .bar-group {
    flex: 1;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.5rem;
  }

  .bars-container {
    width: 100%;
    height: 100%;
    display: flex;
    justify-content: center;
    align-items: flex-end;
    gap: 0.25rem;
  }

  .bar-with-tooltip {
    flex: 1;
    position: relative;
  }

  .bar {
    width: 100%;
    border-radius: 0.375rem 0.375rem 0 0;
    transition: all 0.2s;
    cursor: pointer;
    position: relative;
  }

  .bar:hover {
    filter: brightness(1.2);
  }

  .tooltip {
    position: absolute;
    bottom: 100%;
    left: 50%;
    transform: translateX(-50%);
    background: #111827;
    color: white;
    font-size: 0.75rem;
    padding: 0.375rem 0.625rem;
    border-radius: 0.25rem;
    white-space: nowrap;
    opacity: 0;
    pointer-events: none;
    transition: opacity 0.2s;
    margin-bottom: 0.5rem;
    z-index: 20;
  }

  .mrr-bar {
    background: linear-gradient(to top, #3b82f6, #60a5fa);
  }

  .mau-bar {
    background: linear-gradient(to top, #10b981, #34d399);
  }

  .dau-bar {
    background: linear-gradient(to top, #3b82f6, #60a5fa);
  }

  /* Old bar chart styles for MRR chart */
  .bar-chart {
    height: 16rem;
    display: flex;
    align-items: flex-end;
    justify-content: space-between;
    gap: 0.5rem;
  }

  .bar-wrapper {
    flex: 1;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.5rem;
  }

  .bar-label {
    font-size: 0.875rem;
    color: #9ca3af;
    margin-top: 0.5rem;
  }

  .chart-legend {
    display: flex;
    justify-content: center;
    gap: 2rem;
    margin-top: 1.5rem;
  }

  .legend-item {
    display: flex;
    align-items: center;
    gap: 0.5rem;
    font-size: 0.875rem;
    color: #9ca3af;
  }

  .legend-color {
    width: 1rem;
    height: 1rem;
    border-radius: 0.25rem;
  }

  .legend-color.mau {
    background: linear-gradient(135deg, #10b981, #34d399);
  }

  .legend-color.dau {
    background: linear-gradient(135deg, #3b82f6, #60a5fa);
  }

  /* Progress Bars */
  .progress-list {
    display: flex;
    flex-direction: column;
    gap: 1.5rem;
  }

  .progress-item {
    width: 100%;
  }

  .progress-header {
    display: flex;
    justify-content: space-between;
    margin-bottom: 0.5rem;
  }

  .progress-label {
    font-weight: 600;
    color: #ffffff;
  }

  .progress-value {
    color: #9ca3af;
    font-size: 0.875rem;
  }

  .progress-bar-bg {
    width: 100%;
    height: 0.75rem;
    background: #374151;
    border-radius: 9999px;
    overflow: hidden;
  }

  .progress-bar-fill {
    height: 100%;
    border-radius: 9999px;
    transition: width 0.3s;
  }

  .progress-bar-fill.free {
    background: linear-gradient(90deg, #6b7280, #9ca3af);
  }

  .progress-bar-fill.pro {
    background: linear-gradient(90deg, #3b82f6, #60a5fa);
  }

  .progress-bar-fill.enterprise {
    background: linear-gradient(90deg, #8b5cf6, #a78bfa);
  }

  /* User Metrics Mini Chart */
  .user-metrics-mini-chart {
    height: 12rem;
    display: flex;
    align-items: flex-end;
    justify-content: space-between;
    gap: 0.75rem;
  }

  .mini-bar-group {
    flex: 1;
    display: flex;
    flex-direction: column;
    align-items: center;
    gap: 0.5rem;
  }

  .mini-bars-container {
    width: 100%;
    height: 12rem;
    display: flex;
    justify-content: center;
    align-items: flex-end;
    gap: 0.25rem;
  }

  .mini-bar-label {
    font-size: 0.75rem;
    color: #9ca3af;
  }

  /* Looker Studio */
  .looker-section {
    background: #1f2937;
    border: 1px solid #374151;
    border-radius: 0.5rem;
    padding: 2rem;
  }

  .looker-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1rem;
  }

  .btn-primary {
    padding: 0.75rem 1.5rem;
    background: #2563eb;
    color: white;
    border: none;
    border-radius: 0.5rem;
    font-weight: 600;
    cursor: pointer;
    transition: all 0.2s;
  }

  .btn-primary:hover {
    background: #1d4ed8;
  }

  .looker-description {
    color: #9ca3af;
    margin-bottom: 1.5rem;
    line-height: 1.6;
  }

  .looker-embed {
    background: #111827;
    border-radius: 0.5rem;
    overflow: hidden;
    margin-bottom: 1.5rem;
  }

  .looker-note {
    background: rgba(59, 130, 246, 0.1);
    border: 1px solid rgba(59, 130, 246, 0.3);
    border-radius: 0.5rem;
    padding: 1rem;
  }

  .looker-note p {
    color: #93c5fd;
    font-weight: 600;
    margin-bottom: 0.5rem;
  }

  .looker-note ol {
    color: #bfdbfe;
    font-size: 0.875rem;
    margin-left: 1.5rem;
    line-height: 1.8;
  }

  /* Responsive */
  @media (max-width: 768px) {
    .dashboard-container {
      padding: 1rem;
    }

    .title {
      font-size: 2rem;
    }

    .tabs {
      flex-wrap: nowrap;
      -webkit-overflow-scrolling: touch;
    }

    .kpi-grid-3,
    .kpi-grid-4 {
      grid-template-columns: 1fr;
    }

    .looker-header {
      flex-direction: column;
      align-items: flex-start;
      gap: 1rem;
    }
  }
</style>
