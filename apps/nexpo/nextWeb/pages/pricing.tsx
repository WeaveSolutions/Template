import React from 'react';
import Link from 'next/link';
import Navigation from '../components/Navigation';

export default function Pricing() {
  return (
    <div className="min-h-screen bg-gradient-to-b from-gray-900 to-gray-800 text-white">
      <Navigation />
      <div className="container mx-auto px-4 py-16">
        <h1 className="text-5xl font-bold mb-8 text-center">Pricing</h1>
        <p className="text-xl text-gray-400 text-center mb-16 max-w-3xl mx-auto">
          Choose the plan that fits your needs
        </p>
        
        <div className="grid grid-cols-1 md:grid-cols-3 gap-8 max-w-5xl mx-auto">
          {/* Starter */}
          <div className="bg-gray-800 p-8 rounded-lg border border-gray-700">
            <h3 className="text-2xl font-bold mb-4">Starter</h3>
            <div className="mb-6">
              <span className="text-4xl font-bold">$0</span>
              <span className="text-gray-400">/month</span>
            </div>
            <ul className="space-y-3 mb-8">
              <li className="flex items-center text-gray-300">
                <span className="text-green-500 mr-2">✓</span>
                Next.js + Expo
              </li>
              <li className="flex items-center text-gray-300">
                <span className="text-green-500 mr-2">✓</span>
                Basic Auth
              </li>
              <li className="flex items-center text-gray-300">
                <span className="text-green-500 mr-2">✓</span>
                Community Support
              </li>
            </ul>
            <Link href="/login" className="block w-full bg-gray-700 hover:bg-gray-600 text-center py-3 rounded-lg font-semibold transition-colors">
              Get Started
            </Link>
          </div>

          {/* Professional */}
          <div className="bg-gradient-to-b from-blue-900 to-blue-800 p-8 rounded-lg border-2 border-blue-500 transform scale-105">
            <div className="bg-blue-500 text-white text-xs font-bold px-3 py-1 rounded-full inline-block mb-4">
              POPULAR
            </div>
            <h3 className="text-2xl font-bold mb-4">Professional</h3>
            <div className="mb-6">
              <span className="text-4xl font-bold">$99</span>
              <span className="text-gray-300">/month</span>
            </div>
            <ul className="space-y-3 mb-8">
              <li className="flex items-center text-gray-100">
                <span className="text-green-400 mr-2">✓</span>
                Everything in Starter
              </li>
              <li className="flex items-center text-gray-100">
                <span className="text-green-400 mr-2">✓</span>
                Kong API Gateway
              </li>
              <li className="flex items-center text-gray-100">
                <span className="text-green-400 mr-2">✓</span>
                Multi-cloud Deploy
              </li>
              <li className="flex items-center text-gray-100">
                <span className="text-green-400 mr-2">✓</span>
                Priority Support
              </li>
            </ul>
            <Link href="/login" className="block w-full bg-blue-600 hover:bg-blue-500 text-center py-3 rounded-lg font-semibold transition-colors">
              Start Free Trial
            </Link>
          </div>

          {/* Enterprise */}
          <div className="bg-gray-800 p-8 rounded-lg border border-gray-700">
            <h3 className="text-2xl font-bold mb-4">Enterprise</h3>
            <div className="mb-6">
              <span className="text-4xl font-bold">Custom</span>
            </div>
            <ul className="space-y-3 mb-8">
              <li className="flex items-center text-gray-300">
                <span className="text-green-500 mr-2">✓</span>
                Everything in Pro
              </li>
              <li className="flex items-center text-gray-300">
                <span className="text-green-500 mr-2">✓</span>
                Dedicated Support
              </li>
              <li className="flex items-center text-gray-300">
                <span className="text-green-500 mr-2">✓</span>
                Custom SLAs
              </li>
              <li className="flex items-center text-gray-300">
                <span className="text-green-500 mr-2">✓</span>
                On-premise Option
              </li>
            </ul>
            <Link href="/login" className="block w-full bg-gray-700 hover:bg-gray-600 text-center py-3 rounded-lg font-semibold transition-colors">
              Contact Sales
            </Link>
          </div>
        </div>
      </div>
    </div>
  );
}
