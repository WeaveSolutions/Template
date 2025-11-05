import React from 'react';
import { NextSeo } from 'next-seo';
import Navigation from '../components/Navigation';

export default function DashboardPage() {
  return (
    <>
      <NextSeo
        title="Dashboard - Nexpo Enterprise"
        description="Your personal dashboard"
      />
      <div className="min-h-screen bg-gradient-to-b from-gray-900 to-gray-800 text-white">
        <Navigation />
        <div className="container mx-auto px-4 py-8">
          <h1 className="text-4xl font-bold mb-8">Dashboard</h1>
          
          {/* Stats Grid */}
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6 mb-8">
            {[
              { label: 'Total Users', value: '2,543', change: '+12%', positive: true },
              { label: 'Active Projects', value: '18', change: '+3', positive: true },
              { label: 'API Calls', value: '1.2M', change: '-5%', positive: false },
              { label: 'Uptime', value: '99.9%', change: 'Stable', positive: true },
            ].map((stat) => (
              <div key={stat.label} className="bg-gray-800 p-6 rounded-lg border border-gray-700">
                <div className="text-gray-400 text-sm mb-2">{stat.label}</div>
                <div className="text-3xl font-bold mb-2">{stat.value}</div>
                <div className={`text-sm ${stat.positive ? 'text-green-400' : 'text-red-400'}`}>
                  {stat.change}
                </div>
              </div>
            ))}
          </div>

          {/* Recent Activity */}
          <div className="bg-gray-800 rounded-lg border border-gray-700 p-6 mb-8">
            <h2 className="text-2xl font-bold mb-6">Recent Activity</h2>
            <div className="space-y-4">
              {[
                { action: 'New deployment', project: 'Web App', time: '5 minutes ago', icon: '🚀' },
                { action: 'Database backup', project: 'PostgreSQL', time: '1 hour ago', icon: '💾' },
                { action: 'API updated', project: 'Auth Service', time: '3 hours ago', icon: '🔄' },
                { action: 'User registered', project: 'Production', time: '5 hours ago', icon: '👤' },
              ].map((activity, i) => (
                <div key={i} className="flex items-center justify-between py-3 border-b border-gray-700 last:border-0">
                  <div className="flex items-center space-x-4">
                    <span className="text-2xl">{activity.icon}</span>
                    <div>
                      <div className="font-semibold">{activity.action}</div>
                      <div className="text-sm text-gray-400">{activity.project}</div>
                    </div>
                  </div>
                  <div className="text-sm text-gray-400">{activity.time}</div>
                </div>
              ))}
            </div>
          </div>

          {/* Quick Actions */}
          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            <button className="bg-blue-600 hover:bg-blue-700 p-6 rounded-lg transition-colors text-left">
              <div className="text-3xl mb-3">➕</div>
              <div className="font-semibold mb-1">New Project</div>
              <div className="text-sm text-blue-200">Create a new project</div>
            </button>
            <button className="bg-green-600 hover:bg-green-700 p-6 rounded-lg transition-colors text-left">
              <div className="text-3xl mb-3">📊</div>
              <div className="font-semibold mb-1">View Analytics</div>
              <div className="text-sm text-green-200">Check performance metrics</div>
            </button>
            <button className="bg-purple-600 hover:bg-purple-700 p-6 rounded-lg transition-colors text-left">
              <div className="text-3xl mb-3">⚙️</div>
              <div className="font-semibold mb-1">Settings</div>
              <div className="text-sm text-purple-200">Configure your workspace</div>
            </button>
          </div>
        </div>
      </div>
    </>
  );
}
