import React from 'react';
import { useRef, useEffect, useState } from 'react';
import { View, Text, ScrollView, TouchableOpacity, StyleSheet, Animated } from 'react-native';
import { useTheme, Footer } from '@nexpo/shared-components';
import { useRouter } from 'solito/router';

export default function HomeScreen() {
  const { theme } = useTheme();
  const { colors } = theme;
  const router = useRouter();
  const fadeAnim = useRef(new Animated.Value(0)).current;
  const slideAnim = useRef(new Animated.Value(50)).current;

  useEffect(() => {
    Animated.parallel([
      Animated.timing(fadeAnim, {
        toValue: 1,
        duration: 800,
        useNativeDriver: true,
      }),
      Animated.timing(slideAnim, {
        toValue: 0,
        duration: 800,
        useNativeDriver: true,
      }),
    ]).start();
  }, []);

  const styles = createStyles(colors);
  
  const techStack = [
    'React Native', 'Next.js 15+', 'Expo', 'TypeScript', 'Tailwind CSS', 
    'Solito', 'tRPC', 'Prisma', 'Docker', 'Terraform'
  ];

  const features = [
    { 
      icon: '🏢', 
      title: 'Enterprise Ready', 
      description: 'Production-grade architecture with microservices and multi-cloud deployment' 
    },
    { 
      icon: '🌍', 
      title: 'Multi-Cloud Native', 
      description: 'Deploy on AWS, GCP, Azure, OCI, IBM Cloud, and Cloudflare with Terraform' 
    },
    { 
      icon: '🔒', 
      title: 'Kong API Gateway', 
      description: 'Enterprise gateway with Bank Grade TLS 1.3, JWT validation, caching, and rate limiting' 
    },
    { 
      icon: '📱', 
      title: 'Cross-Platform', 
      description: 'One codebase for Web, iOS, and Android with shared components via Solito' 
    },
    { 
      icon: '💳', 
      title: 'Polar Payments', 
      description: 'Open-source billing platform with MoR support, Stripe optional via feature flag' 
    },
    { 
      icon: '⚡', 
      title: 'Real-time Features', 
      description: 'WebSocket support, Supabase real-time subscriptions, and reactive data sync' 
    },
    { 
      icon: '🚀', 
      title: 'DevOps Automation', 
      description: 'CI/CD pipelines, Terraform IaC, Docker containers, and automated deployments' 
    },
    { 
      icon: '🔧', 
      title: 'Developer Experience', 
      description: 'Hot reload, TypeScript, ESLint, Jest testing, and comprehensive documentation' 
    },
    { 
      icon: '🤖', 
      title: 'AI-Powered Tools', 
      description: 'WebNLP content discovery, CodeRabbit code review, and n8n workflow automation' 
    },
    { 
      icon: '📊', 
      title: 'Business Intelligence', 
      description: 'KPIs and Reports microservices, Retool dashboards, and Sentry monitoring' 
    },
    { 
      icon: '🔄', 
      title: 'Offline-First Sync', 
      description: 'Ditto local-first sync engine with mesh network support for edge devices' 
    },
    { 
      icon: '📧', 
      title: 'Communication Suite', 
      description: 'Notifications service, Brevo email/SMS campaigns, and push messaging' 
    },
  ];

  const cloudProviders = [
    { name: 'AWS', icon: '☁️' },
    { name: 'Google Cloud', icon: '🌐' },
    { name: 'Azure', icon: '🔷' },
    { name: 'Oracle Cloud', icon: '🟠' },
    { name: 'IBM Cloud', icon: '💙' },
    { name: 'DigitalOcean', icon: '🌊' },
    { name: 'Heroku', icon: '🟣' },
    { name: 'Cloudflare', icon: '🔶' },
  ];

  const microservices = [
    'Kong API Gateway', 'User Management', 'Authentication', 'Polar Payments', 
    'Notifications', 'Analytics', 'File Storage', 'Real-time Messaging', 'n8n Workflows'
  ];

  const backendTech = [
    'Node.js + Express', 'FastAPI (Python)', 'gRPC (Go)', 'tRPC', 
    'Firebase', 'Supabase', 'RabbitMQ', 'PostgreSQL'
  ];

  const devOpsTools = [
    'Docker', 'Terraform', 'Nomad', 'Vercel', 'Jest', 'ESLint', 
    'Sentry', 'Postman', 'Swagger/OpenAPI'
  ];

  return (
    <ScrollView style={styles.container}>
      {/* Hero Section */}
      <Animated.View 
        style={[
          styles.hero,
          {
            opacity: fadeAnim,
            transform: [{ translateY: slideAnim }],
          },
        ]}
      >
        <Text style={styles.heroTitle}>
          Nexpo Enterprise Template
        </Text>
        <Text style={styles.heroSubline}>
          The Ultimate Cross-Platform Development Stack
        </Text>
        <Text style={styles.heroSubtitle}>
          Enterprise-grade monorepo with multi-cloud microservices, Kong API Gateway, 
          and Bank Grade TLS security. Build once, deploy everywhere.
        </Text>
        <View style={styles.buttonContainer}>
          <TouchableOpacity 
            style={[styles.button, styles.primaryButton]} 
            onPress={() => router.push('/login')}
          >
            <Text style={styles.primaryButtonText}>Get Started</Text>
          </TouchableOpacity>
          <TouchableOpacity 
            style={[styles.button, styles.secondaryButton]} 
            onPress={() => router.push('/dashboard')}
          >
            <Text style={styles.secondaryButtonText}>View Dashboard</Text>
          </TouchableOpacity>
        </View>
      </Animated.View>

      {/* Stats Section */}
      <View style={styles.statsSection}>
        <View style={styles.stat}>
          <Text style={styles.statNumber}>9</Text>
          <Text style={styles.statLabel}>Microservices</Text>
        </View>
        <View style={styles.stat}>
          <Text style={styles.statNumber}>8</Text>
          <Text style={styles.statLabel}>Cloud Providers</Text>
        </View>
        <View style={styles.stat}>
          <Text style={styles.statNumber}>15+</Text>
          <Text style={styles.statLabel}>Databases</Text>
        </View>
        <View style={styles.stat}>
          <Text style={styles.statNumber}>100%</Text>
          <Text style={styles.statLabel}>TypeScript</Text>
        </View>
      </View>

      {/* Cloud Providers */}
      <View style={styles.section}>
        <Text style={styles.sectionTitle}>Multi-Cloud Ready</Text>
        <Text style={styles.sectionSubtitle}>
          Deploy seamlessly across all major cloud providers with Terraform
        </Text>
        <View style={styles.cloudGrid}>
          {cloudProviders.map((provider, index) => (
            <Animated.View
              key={provider.name}
              style={[
                styles.cloudCard,
                {
                  opacity: fadeAnim,
                  transform: [{
                    translateY: Animated.multiply(slideAnim, ((index % 3) + 1) * 0.3),
                  }],
                },
              ]}
            >
              <Text style={styles.cloudIcon}>{provider.icon}</Text>
              <Text style={styles.cloudName}>{provider.name}</Text>
            </Animated.View>
          ))}
        </View>
      </View>

      {/* Features Grid */}
      <View style={styles.section}>
        <Text style={styles.sectionTitle}>Enterprise Features</Text>
        <Text style={styles.sectionSubtitle}>
          Everything you need for production-ready applications
        </Text>
        <View style={styles.featuresGrid}>
          {features.map((feature, index) => (
            <Animated.View
              key={feature.title}
              style={[
                styles.featureCard,
                {
                  opacity: fadeAnim,
                  transform: [{
                    translateY: Animated.multiply(slideAnim, ((index % 2) + 1) * 0.5),
                  }],
                },
              ]}
            >
              <Text style={styles.featureIcon}>{feature.icon}</Text>
              <Text style={styles.featureTitle}>{feature.title}</Text>
              <Text style={styles.featureDescription}>{feature.description}</Text>
            </Animated.View>
          ))}
        </View>
      </View>

      {/* Microservices Architecture */}
      <View style={styles.section}>
        <Text style={styles.sectionTitle}>Microservices Architecture</Text>
        <Text style={styles.sectionSubtitle}>
          Production-ready microservices with Kong API Gateway
        </Text>
        <View style={styles.microservicesGrid}>
          {microservices.map((service, index) => (
            <Animated.View
              key={service}
              style={[
                styles.serviceCard,
                {
                  opacity: fadeAnim,
                  transform: [{
                    translateY: Animated.multiply(slideAnim, ((index % 3) + 1) * 0.4),
                  }],
                },
              ]}
            >
              <Text style={styles.serviceText}>{service}</Text>
            </Animated.View>
          ))}
        </View>
      </View>

      {/* Tech Stack */}
      <View style={styles.section}>
        <Text style={styles.sectionTitle}>Frontend Technologies</Text>
        <Text style={styles.sectionSubtitle}>
          Modern cross-platform frontend stack
        </Text>
        <View style={styles.techGrid}>
          {techStack.map((tech, index) => (
            <Animated.View
              key={tech}
              style={[
                styles.techCard,
                {
                  opacity: fadeAnim,
                  transform: [{
                    translateY: Animated.multiply(slideAnim, ((index % 4) + 1) * 0.3),
                  }],
                },
              ]}
            >
              <Text style={styles.techText}>{tech}</Text>
            </Animated.View>
          ))}
        </View>
      </View>

      {/* Backend Technologies */}
      <View style={styles.section}>
        <Text style={styles.sectionTitle}>Backend & Data Stack</Text>
        <Text style={styles.sectionSubtitle}>
          High-performance backend with multiple language support
        </Text>
        <View style={styles.techGrid}>
          {backendTech.map((tech, index) => (
            <Animated.View
              key={tech}
              style={[
                styles.techCard,
                {
                  opacity: fadeAnim,
                  transform: [{
                    translateY: Animated.multiply(slideAnim, ((index % 4) + 1) * 0.3),
                  }],
                },
              ]}
            >
              <Text style={styles.techText}>{tech}</Text>
            </Animated.View>
          ))}
        </View>
      </View>

      {/* DevOps Tools */}
      <View style={styles.section}>
        <Text style={styles.sectionTitle}>DevOps & Monitoring</Text>
        <Text style={styles.sectionSubtitle}>
          Complete CI/CD, testing, and observability toolkit
        </Text>
        <View style={styles.techGrid}>
          {devOpsTools.map((tech, index) => (
            <Animated.View
              key={tech}
              style={[
                styles.techCard,
                {
                  opacity: fadeAnim,
                  transform: [{
                    translateY: Animated.multiply(slideAnim, ((index % 4) + 1) * 0.3),
                  }],
                },
              ]}
            >
              <Text style={styles.techText}>{tech}</Text>
            </Animated.View>
          ))}
        </View>
      </View>

      {/* Security Highlight */}
      <View style={styles.securitySection}>
        <Text style={styles.securityTitle}>🔒 Bank Grade Security</Text>
        <Text style={styles.securityDescription}>
          Kong API Gateway with TLS 1.3, Auth0 JWT validation, enterprise-grade encryption, 
          and comprehensive security headers for production deployments.
        </Text>
        <View style={styles.securityFeatures}>
          <Text style={styles.securityFeature}>✅ TLS 1.3 Encryption</Text>
          <Text style={styles.securityFeature}>✅ JWT Authentication</Text>
          <Text style={styles.securityFeature}>✅ Rate Limiting</Text>
          <Text style={styles.securityFeature}>✅ CORS Protection</Text>
          <Text style={styles.securityFeature}>✅ HSTS Headers</Text>
          <Text style={styles.securityFeature}>✅ Multi-Factor Auth</Text>
        </View>
      </View>

      {/* CTA Section */}
      <View style={styles.ctaSection}>
        <Text style={styles.ctaTitle}>Ready to Build Enterprise Apps?</Text>
        <Text style={styles.ctaSubtitle}>
          Join developers building production-ready applications with the Nexpo Enterprise Template
        </Text>
        <TouchableOpacity 
          style={[styles.button, styles.primaryButton, styles.ctaButton]} 
          onPress={() => router.push('/login')}
        >
          <Text style={styles.primaryButtonText}>Start Building Today</Text>
        </TouchableOpacity>
      </View>

      <Footer />
    </ScrollView>
  );
};

const createStyles = (colors: any) => StyleSheet.create({
  container: {
    flex: 1,
    backgroundColor: colors.background,
  },
  hero: {
    paddingHorizontal: 20,
    paddingVertical: 60,
    alignItems: 'center',
  },
  heroTitle: {
    fontSize: 48,
    fontWeight: 'bold',
    color: colors.text,
    textAlign: 'center',
    marginBottom: 16,
  },
  heroSubline: {
    fontSize: 20,
    color: colors.mutedText,
    textAlign: 'center',
    marginBottom: 8,
  },
  heroSubtitle: {
    fontSize: 20,
    color: colors.mutedText,
    textAlign: 'center',
    marginBottom: 32,
    maxWidth: 600,
  },
  buttonContainer: {
    flexDirection: 'row',
    gap: 16,
  },
  button: {
    paddingHorizontal: 24,
    paddingVertical: 12,
    borderRadius: 8,
  },
  primaryButton: {
    backgroundColor: colors.primary,
  },
  primaryButtonText: {
    color: '#FFFFFF',
    fontSize: 16,
    fontWeight: '600',
  },
  secondaryButton: {
    backgroundColor: 'transparent',
    borderWidth: 2,
    borderColor: colors.border,
  },
  secondaryButtonText: {
    color: colors.text,
    fontSize: 16,
    fontWeight: '600',
  },
  statsSection: {
    paddingHorizontal: 20,
    paddingVertical: 40,
    flexDirection: 'row',
    justifyContent: 'space-between',
  },
  stat: {
    alignItems: 'center',
  },
  statNumber: {
    fontSize: 32,
    fontWeight: 'bold',
    color: colors.text,
  },
  statLabel: {
    fontSize: 16,
    color: colors.mutedText,
  },
  section: {
    paddingHorizontal: 20,
    paddingVertical: 40,
  },
  sectionTitle: {
    fontSize: 32,
    fontWeight: 'bold',
    color: colors.text,
    textAlign: 'center',
    marginBottom: 16,
  },
  sectionSubtitle: {
    fontSize: 18,
    color: colors.mutedText,
    textAlign: 'center',
    marginBottom: 32,
  },
  cloudGrid: {
    flexDirection: 'row',
    flexWrap: 'wrap',
    justifyContent: 'center',
    gap: 16,
  },
  cloudCard: {
    backgroundColor: colors.surface,
    paddingHorizontal: 20,
    paddingVertical: 12,
    borderRadius: 24,
    borderWidth: 1,
    borderColor: colors.border,
  },
  cloudIcon: {
    fontSize: 24,
    marginBottom: 8,
  },
  cloudName: {
    fontSize: 16,
    color: colors.text,
  },
  featuresGrid: {
    flexDirection: 'row',
    flexWrap: 'wrap',
    gap: 20,
    justifyContent: 'center',
  },
  featureCard: {
    backgroundColor: colors.surface,
    padding: 24,
    borderRadius: 12,
    borderWidth: 1,
    borderColor: colors.border,
    width: 300,
    maxWidth: '100%',
  },
  featureIcon: {
    fontSize: 32,
    marginBottom: 12,
  },
  featureTitle: {
    fontSize: 20,
    fontWeight: '600',
    color: colors.text,
    marginBottom: 8,
  },
  featureDescription: {
    fontSize: 14,
    color: colors.mutedText,
    lineHeight: 20,
  },
  microservicesGrid: {
    flexDirection: 'row',
    flexWrap: 'wrap',
    justifyContent: 'center',
    gap: 16,
  },
  serviceCard: {
    backgroundColor: colors.surface,
    paddingHorizontal: 20,
    paddingVertical: 12,
    borderRadius: 24,
    borderWidth: 1,
    borderColor: colors.border,
  },
  serviceText: {
    fontSize: 16,
    color: colors.text,
  },
  techGrid: {
    flexDirection: 'row',
    flexWrap: 'wrap',
    justifyContent: 'center',
    gap: 16,
  },
  techCard: {
    backgroundColor: colors.surface,
    paddingHorizontal: 20,
    paddingVertical: 12,
    borderRadius: 24,
    borderWidth: 1,
    borderColor: colors.border,
  },
  techText: {
    fontSize: 14,
    color: colors.text,
  },
  securitySection: {
    paddingHorizontal: 20,
    paddingVertical: 40,
    backgroundColor: colors.surface,
  },
  securityTitle: {
    fontSize: 32,
    fontWeight: 'bold',
    color: colors.text,
    textAlign: 'center',
    marginBottom: 16,
  },
  securityDescription: {
    fontSize: 18,
    color: colors.mutedText,
    textAlign: 'center',
    marginBottom: 32,
  },
  securityFeatures: {
    flexDirection: 'row',
    flexWrap: 'wrap',
    justifyContent: 'center',
    gap: 16,
  },
  securityFeature: {
    fontSize: 16,
    color: colors.text,
    marginBottom: 8,
  },
  ctaSection: {
    paddingHorizontal: 20,
    paddingVertical: 60,
    alignItems: 'center',
    backgroundColor: colors.surface,
  },
  ctaTitle: {
    fontSize: 32,
    fontWeight: 'bold',
    color: colors.text,
    textAlign: 'center',
    marginBottom: 16,
  },
  ctaSubtitle: {
    fontSize: 18,
    color: colors.mutedText,
    textAlign: 'center',
    marginBottom: 32,
  },
  ctaButton: {
    paddingHorizontal: 32,
    paddingVertical: 16,
  },
});
