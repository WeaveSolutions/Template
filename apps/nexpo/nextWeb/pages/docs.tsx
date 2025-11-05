import React from 'react';
import Link from 'next/link';
import Navigation from '../components/Navigation';

export default function Docs() {
  return (
    <div className="min-h-screen bg-gradient-to-b from-gray-900 to-gray-800 text-white">
      <Navigation />
      <div className="container mx-auto px-4 py-16">
        <h1 className="text-5xl font-bold mb-8 text-center">Documentation</h1>
        <p className="text-xl text-gray-400 text-center mb-16 max-w-3xl mx-auto">
          Learn how to build enterprise applications with Nexpo
        </p>
        
        <div className="grid grid-cols-1 md:grid-cols-2 gap-8 max-w-4xl mx-auto">
          {/* Getting Started */}
          <div className="bg-gray-800 p-8 rounded-lg border border-gray-700 hover:border-blue-500 transition-colors">
            <div className="text-4xl mb-4">🚀</div>
            <h3 className="text-2xl font-bold mb-4">Getting Started</h3>
            <p className="text-gray-400 mb-6">
              Quick start guide to set up your development environment and run your first app
            </p>
            <Link href="#" className="text-blue-400 hover:text-blue-300 font-semibold">
              Read Guide →
            </Link>
          </div>

          {/* Architecture */}
          <div className="bg-gray-800 p-8 rounded-lg border border-gray-700 hover:border-blue-500 transition-colors">
            <div className="text-4xl mb-4">🏗️</div>
            <h3 className="text-2xl font-bold mb-4">Architecture</h3>
            <p className="text-gray-400 mb-6">
              Understand the microservices architecture, Kong Gateway, and multi-cloud setup
            </p>
            <Link href="#" className="text-blue-400 hover:text-blue-300 font-semibold">
              Learn More →
            </Link>
          </div>

          {/* API Reference */}
          <div className="bg-gray-800 p-8 rounded-lg border border-gray-700 hover:border-blue-500 transition-colors">
            <div className="text-4xl mb-4">📚</div>
            <h3 className="text-2xl font-bold mb-4">API Reference</h3>
            <p className="text-gray-400 mb-6">
              Complete API documentation for all microservices and shared components
            </p>
            <Link href="#" className="text-blue-400 hover:text-blue-300 font-semibold">
              View APIs →
            </Link>
          </div>

          {/* Deployment */}
          <div className="bg-gray-800 p-8 rounded-lg border border-gray-700 hover:border-blue-500 transition-colors">
            <div className="text-4xl mb-4">☁️</div>
            <h3 className="text-2xl font-bold mb-4">Deployment</h3>
            <p className="text-gray-400 mb-6">
              Deploy to AWS, GCP, Azure, and other cloud providers with Terraform
            </p>
            <Link href="#" className="text-blue-400 hover:text-blue-300 font-semibold">
              Deploy Now →
            </Link>
          </div>

          {/* Authentication */}
          <div className="bg-gray-800 p-8 rounded-lg border border-gray-700 hover:border-blue-500 transition-colors">
            <div className="text-4xl mb-4">🔐</div>
            <h3 className="text-2xl font-bold mb-4">Authentication</h3>
            <p className="text-gray-400 mb-6">
              Set up Auth0, configure JWT validation, and implement role-based access
            </p>
            <Link href="#" className="text-blue-400 hover:text-blue-300 font-semibold">
              Configure Auth →
            </Link>
          </div>

          {/* Best Practices */}
          <div className="bg-gray-800 p-8 rounded-lg border border-gray-700 hover:border-blue-500 transition-colors">
            <div className="text-4xl mb-4">✨</div>
            <h3 className="text-2xl font-bold mb-4">Best Practices</h3>
            <p className="text-gray-400 mb-6">
              Learn best practices for code organization, testing, and production deployment
            </p>
            <Link href="#" className="text-blue-400 hover:text-blue-300 font-semibold">
              Read Tips →
            </Link>
          </div>
        </div>

        {/* Quick Links */}
        <div className="mt-16 text-center">
          <h2 className="text-2xl font-bold mb-6">Quick Links</h2>
          <div className="flex flex-wrap justify-center gap-4">
            <Link href="#" className="px-6 py-2 bg-gray-800 hover:bg-gray-700 rounded-lg border border-gray-700 transition-colors">
              GitHub Repository
            </Link>
            <Link href="#" className="px-6 py-2 bg-gray-800 hover:bg-gray-700 rounded-lg border border-gray-700 transition-colors">
              Community Forum
            </Link>
            <Link href="#" className="px-6 py-2 bg-gray-800 hover:bg-gray-700 rounded-lg border border-gray-700 transition-colors">
              Video Tutorials
            </Link>
            <Link href="#" className="px-6 py-2 bg-gray-800 hover:bg-gray-700 rounded-lg border border-gray-700 transition-colors">
              FAQ
            </Link>
          </div>
        </div>
      </div>
    </div>
  );
}
