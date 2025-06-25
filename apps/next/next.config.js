/* eslint-disable @typescript-eslint/no-var-requires */
const path = require('path');
let withExpo;
try {
  withExpo = require('@expo/next-adapter').withExpo;
} catch (e) {
  // Fallback for pnpm hoisted structure
  try {
    withExpo = require(require.resolve('@expo/next-adapter', { paths: [path.join(__dirname, '../../node_modules')] })).withExpo;
  } catch (e2) {
    console.warn('Could not load @expo/next-adapter, using plain Next.js config');
    withExpo = (config) => config;
  }
}

/** @type {import('next').NextConfig} */
const nextConfig = withExpo({
  reactStrictMode: false,
  transpilePackages: [
    'solito',
    'react-native',
    'react-native-web',
    'expo',
    'moti',
    'app',
    'react-native-reanimated',
    'nativewind',
    'react-native-gesture-handler',
    '@shared/components',
    '@shared/utils',
    '@shared/pages',
    '@shared/provider',
    '@shared/ui',
  ],
  experimental: {
    forceSwcTransforms: true,
  },
  async redirects() {
    return [
      {
        source: '/home',
        destination: '/',
        permanent: true,
      },
    ];
  },
  webpack: (config, { buildId, dev, isServer, defaultLoaders, nextRuntime, webpack }) => {
    config.resolve.alias = {
      ...(config.resolve.alias || {}),
      'react-native$': 'react-native-web',
      // Force all modules to use the same instance of React and React DOM
      react: path.resolve(__dirname, 'node_modules/react'),
      'react-dom': path.resolve(__dirname, 'node_modules/react-dom'),
    };
    config.resolve.extensions = [
      '.web.js',
      '.web.ts',
      '.web.tsx',
      ...config.resolve.extensions,
    ];
    return config;
  },
});

module.exports = nextConfig;
