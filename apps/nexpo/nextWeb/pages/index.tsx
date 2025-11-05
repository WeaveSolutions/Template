import React from 'react'
import Link from 'next/link'
import Navigation from '../components/Navigation'

export default function Index() {
  return (
    <div className="min-h-screen bg-gradient-to-b from-gray-900 to-gray-800 text-white">
      <Navigation />
      <div className="container mx-auto px-4 py-16">
        {/* Hero Section */}
        <div className="text-center mb-16">
          <h1 className="text-6xl font-bold mb-4">
            Nexpo Enterprise Template
          </h1>
          <p className="text-2xl text-gray-300 mb-8">
            The Ultimate Cross-Platform Development Stack
          </p>
          <p className="text-lg text-gray-400 mb-8 max-w-3xl mx-auto">
            Enterprise-grade monorepo with multi-cloud microservices, Kong API Gateway, 
            and Bank Grade TLS security. Build once, deploy everywhere.
          </p>
          <div className="flex gap-4 justify-center">
            <Link href="/login" className="bg-blue-600 hover:bg-blue-700 px-8 py-3 rounded-lg font-semibold">
              Get Started
            </Link>
            <Link href="/dashboard" className="border-2 border-gray-600 hover:border-gray-500 px-8 py-3 rounded-lg font-semibold">
              View Dashboard
            </Link>
          </div>
        </div>

        {/* Stats */}
        <div className="grid grid-cols-4 gap-8 mb-16 text-center">
          <div>
            <div className="text-4xl font-bold">9</div>
            <div className="text-gray-400">Microservices</div>
          </div>
          <div>
            <div className="text-4xl font-bold">8</div>
            <div className="text-gray-400">Cloud Providers</div>
          </div>
          <div>
            <div className="text-4xl font-bold">15+</div>
            <div className="text-gray-400">Databases</div>
          </div>
          <div>
            <div className="text-4xl font-bold">100%</div>
            <div className="text-gray-400">TypeScript</div>
          </div>
        </div>

        {/* Features */}
        <div className="mb-16">
          <h2 className="text-4xl font-bold text-center mb-12">Enterprise Features</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
            {[
              { icon: '🏢', title: 'Enterprise Ready', desc: 'Production-grade architecture with microservices and multi-cloud deployment' },
              { icon: '🌍', title: 'Multi-Cloud Native', desc: 'Deploy on AWS, GCP, Azure, OCI, IBM Cloud, and Cloudflare with Terraform' },
              { icon: '🔒', title: 'Kong API Gateway', desc: 'Bank Grade TLS 1.3, JWT validation, caching, and rate limiting' },
              { icon: '📱', title: 'Cross-Platform', desc: 'One codebase for Web, iOS, and Android with shared components via Solito' },
              { icon: '💳', title: 'Polar Payments', desc: 'Open-source billing platform with MoR support, Stripe optional via feature flag' },
              { icon: '⚡', title: 'Real-time Features', desc: 'WebSocket support, Supabase real-time subscriptions, and reactive data sync' },
              { icon: '🚀', title: 'DevOps Automation', desc: 'CI/CD pipelines, Terraform IaC, Docker containers, and automated deployments' },
              { icon: '🔧', title: 'Developer Experience', desc: 'Hot reload, TypeScript, ESLint, Jest testing, and comprehensive documentation' },
              { icon: '🤖', title: 'AI-Powered Tools', desc: 'MindsDB AI/ML gateway, CodeRabbit code review, and n8n workflow automation' },
              { icon: '📊', title: 'Business Intelligence', desc: 'KPIs and Reports microservices, Retool dashboards, and Sentry monitoring' },
              { icon: '🔄', title: 'Offline-First Sync', desc: 'Ditto local-first sync engine with mesh network support for edge devices' },
              { icon: '📧', title: 'Communication Suite', desc: 'Notifications service, email/SMS delivery, and push messaging' },
            ].map((feature) => (
              <div key={feature.title} className="bg-gray-800 p-6 rounded-lg border border-gray-700 hover:border-blue-500 transition-colors">
                <div className="text-4xl mb-3">{feature.icon}</div>
                <h3 className="text-xl font-semibold mb-2">{feature.title}</h3>
                <p className="text-gray-400 text-sm">{feature.desc}</p>
              </div>
            ))}
          </div>
        </div>

        {/* Cloud Providers */}
        <div className="mb-16">
          <h2 className="text-4xl font-bold text-center mb-4">Multi-Cloud Ready</h2>
          <p className="text-center text-gray-400 mb-12 max-w-2xl mx-auto">
            Deploy seamlessly across all major cloud providers with Terraform IaC
          </p>
          <div className="flex flex-wrap justify-center gap-4">
            {[
              { name: 'AWS', icon: '☁️' },
              { name: 'Google Cloud', icon: '🌐' },
              { name: 'Azure', icon: '🔷' },
              { name: 'Oracle Cloud', icon: '🟠' },
              { name: 'IBM Cloud', icon: '💙' },
              { name: 'DigitalOcean', icon: '🌊' },
              { name: 'Heroku', icon: '🟣' },
              { name: 'Cloudflare', icon: '🔶' },
            ].map((provider) => (
              <div key={provider.name} className="bg-gray-800 px-6 py-3 rounded-full border border-gray-700 hover:border-blue-500 transition-colors">
                <span className="text-2xl mr-2">{provider.icon}</span>
                <span className="text-gray-300">{provider.name}</span>
              </div>
            ))}
          </div>
        </div>

        {/* Microservices */}
        <div className="mb-16 bg-gray-800 bg-opacity-50 p-12 rounded-lg">
          <h2 className="text-4xl font-bold text-center mb-4">Microservices Architecture</h2>
          <p className="text-center text-gray-400 mb-12 max-w-2xl mx-auto">
            Production-ready microservices with Kong API Gateway orchestration
          </p>
          <div className="flex flex-wrap justify-center gap-3">
            {[
              'Kong API Gateway', 'User Management', 'Authentication', 'Polar Payments', 
              'Notifications', 'Analytics', 'File Storage', 'Real-time Messaging', 
              'n8n Workflows', 'MindsDB AI/ML', 'Background Jobs', 'Email Service'
            ].map((service) => (
              <div key={service} className="bg-gray-900 px-4 py-2 rounded-lg border border-gray-700 text-sm">
                {service}
              </div>
            ))}
          </div>
        </div>

        {/* Tech Stack */}
        <div className="mb-16">
          <h2 className="text-4xl font-bold text-center mb-12">Technology Stack</h2>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
            {/* Frontend */}
            <div>
              <h3 className="text-2xl font-semibold mb-4 text-blue-400">Frontend & Mobile</h3>
              <div className="flex flex-wrap gap-2">
                {['React Native', 'Next.js 14', 'Expo', 'Tauri Desktop', 'Svelte', 'TypeScript', 'Tailwind CSS', 'Solito'].map((tech) => (
                  <span key={tech} className="bg-blue-900 bg-opacity-30 px-3 py-1 rounded text-sm border border-blue-700">
                    {tech}
                  </span>
                ))}
              </div>
            </div>
            {/* Backend */}
            <div>
              <h3 className="text-2xl font-semibold mb-4 text-green-400">Backend & APIs</h3>
              <div className="flex flex-wrap gap-2">
                {['Node.js + Express', 'FastAPI (Python)', 'Rust Actix', 'tRPC', 'Prisma ORM', 'PostgreSQL', 'MongoDB', 'Redis'].map((tech) => (
                  <span key={tech} className="bg-green-900 bg-opacity-30 px-3 py-1 rounded text-sm border border-green-700">
                    {tech}
                  </span>
                ))}
              </div>
            </div>
            {/* DevOps */}
            <div>
              <h3 className="text-2xl font-semibold mb-4 text-orange-400">DevOps & Infrastructure</h3>
              <div className="flex flex-wrap gap-2">
                {['Docker', 'Terraform', 'GitHub Actions', 'Kong Gateway', 'Nomad', 'Vercel', 'Sentry', 'Prometheus'].map((tech) => (
                  <span key={tech} className="bg-orange-900 bg-opacity-30 px-3 py-1 rounded text-sm border border-orange-700">
                    {tech}
                  </span>
                ))}
              </div>
            </div>
            {/* Data & AI */}
            <div>
              <h3 className="text-2xl font-semibold mb-4 text-purple-400">Data & AI/ML</h3>
              <div className="flex flex-wrap gap-2">
                {['MindsDB', 'Ditto Sync', 'Supabase', 'Firebase', 'Auth0', 'n8n Automation', 'Retool', 'Looker Studio'].map((tech) => (
                  <span key={tech} className="bg-purple-900 bg-opacity-30 px-3 py-1 rounded text-sm border border-purple-700">
                    {tech}
                  </span>
                ))}
              </div>
            </div>
          </div>
        </div>

        {/* Security Section */}
        <div className="mb-16 bg-gradient-to-r from-blue-900 to-purple-900 p-12 rounded-lg">
          <h2 className="text-4xl font-bold text-center mb-4">🔒 Bank Grade Security</h2>
          <p className="text-center text-gray-300 mb-8 max-w-3xl mx-auto">
            Kong API Gateway with TLS 1.3, Auth0 JWT validation, enterprise-grade encryption, 
            and comprehensive security headers for production deployments
          </p>
          <div className="grid grid-cols-2 md:grid-cols-3 gap-4 max-w-3xl mx-auto">
            {['✅ TLS 1.3 Encryption', '✅ JWT Authentication', '✅ Rate Limiting', '✅ CORS Protection', '✅ HSTS Headers', '✅ Multi-Factor Auth'].map((feature) => (
              <div key={feature} className="text-center py-2 text-gray-200">
                {feature}
              </div>
            ))}
          </div>
        </div>

        {/* CTA */}
        <div className="text-center bg-gray-800 p-12 rounded-lg">
          <h2 className="text-3xl font-bold mb-4">Ready to Build Enterprise Apps?</h2>
          <p className="text-gray-400 mb-8">
            Join developers building production-ready applications with the Nexpo Enterprise Template
          </p>
          <Link href="/login" className="inline-block bg-blue-600 hover:bg-blue-700 px-12 py-4 rounded-lg font-semibold text-lg">
            Start Building Today
          </Link>
        </div>
      </div>
    </div>
  )
}
