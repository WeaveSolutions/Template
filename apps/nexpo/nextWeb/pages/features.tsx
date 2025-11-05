import React from 'react';
import Navigation from '../components/Navigation';

export default function Features() {
  return (
    <div className="min-h-screen bg-gradient-to-b from-gray-900 to-gray-800 text-white">
      <Navigation />
      <div className="container mx-auto px-4 py-16">
        <h1 className="text-5xl font-bold mb-8 text-center">Features</h1>
        <p className="text-xl text-gray-400 text-center mb-16 max-w-3xl mx-auto">
          Explore the comprehensive feature set of the Nexpo Enterprise Template
        </p>
        
        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
          {[
            { title: 'Multi-Cloud Deployment', desc: 'Deploy to AWS, GCP, Azure, and more with Terraform' },
            { title: 'Cross-Platform Apps', desc: 'Build for Web, iOS, Android, and Desktop from one codebase' },
            { title: 'Microservices Ready', desc: 'Production-grade microservices architecture' },
            { title: 'Kong API Gateway', desc: 'Enterprise API management with TLS 1.3' },
            { title: 'Auth0 Integration', desc: 'Secure authentication and authorization' },
            { title: 'Real-time Sync', desc: 'Offline-first with Ditto and Supabase' },
          ].map((feature) => (
            <div key={feature.title} className="bg-gray-800 p-6 rounded-lg border border-gray-700">
              <h3 className="text-xl font-semibold mb-3">{feature.title}</h3>
              <p className="text-gray-400">{feature.desc}</p>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
}
