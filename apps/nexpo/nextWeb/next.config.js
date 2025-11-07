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
  output: process.env.NEXT_OUTPUT === 'export' ? 'export' : undefined,
  // Suppress verbose Next.js logs
  logging: {
    fetches: {
      fullUrl: false,
    },
  },
  onDemandEntries: {
    maxInactiveAge: 25 * 1000,
    pagesBufferLength: 2,
  },
  // Empty turbopack config to silence webpack warning
  turbopack: {},
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
    '@nexpo/shared-components',
    '@nexpo/shared-utils',
    '@nexpo/shared-pages',
    '@nexpo/shared-provider',
    '@nexpo/shared-ui',
    '@nexpo/shared-db',
  ],
  experimental: {
    externalDir: true,
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
      react: path.resolve(__dirname, 'node_modules/react'),
      'react-dom': path.resolve(__dirname, 'node_modules/react-dom'),
      '@dittolive/ditto': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-stub.js'),
      // Prevent Prisma and database sync from being bundled in client-side code
      ...(!isServer && {
        '@prisma/client': false,
        '.prisma/client': false,
        [path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-sync-factory.js')]: 
          path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-sync-factory.client.js'),
        [path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-aws-rds-sync.js')]: 
          path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-aws-rds-sync.client.js'),
        [path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-postgres-sync.js')]: 
          path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-aws-rds-sync.client.js'),
        [path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-mongodb-sync.js')]: 
          path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-aws-rds-sync.client.js'),
        [path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-cosmosdb-sync.js')]: 
          path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-aws-rds-sync.client.js'),
        [path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-sqlserver-sync.js')]: 
          path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-aws-rds-sync.client.js'),
        [path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-ibmcloud-sync.js')]: 
          path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-aws-rds-sync.client.js'),
        [path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-supabase-sync.js')]: 
          path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-aws-rds-sync.client.js'),
        [path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-firebase-sync.js')]: 
          path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-aws-rds-sync.client.js'),
        [path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-aws-s3-sync.js')]: 
          path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src/ditto-aws-rds-sync.client.js'),
      }),
      '@nexpo/shared-components': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-components/src/index.ts'),
      '@nexpo/shared-utils': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src'),
      '@nexpo/shared-pages': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-pages/src'),
      '@nexpo/shared-provider': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-provider/src'),
      '@nexpo/shared-ui': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-ui/src'),
      '@nexpo/shared-db': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-db/src'),
      '@shared/components': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-components/src/index.ts'),
      '@shared/utils': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-utils/src'),
      '@shared/pages': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-pages/src'),
      '@shared/provider': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-provider/src'),
      '@shared/ui': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-ui/src'),
      '@shared/db': path.resolve(__dirname, '../../../packages/shared-nexpo/shared-db/src'),
    };
    // Ensure declaration files are never executed at runtime
    config.plugins = config.plugins || [];
    config.plugins.push(new webpack.IgnorePlugin({ resourceRegExp: /\.d\.ts$/ }));
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
