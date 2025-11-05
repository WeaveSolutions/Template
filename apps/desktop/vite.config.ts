import { defineConfig } from 'vite';
import { svelte } from '@sveltejs/vite-plugin-svelte';
import path from 'path';

export default defineConfig(async () => ({
  plugins: [svelte()],
  
  // Resolve aliases for shared packages and Tauri Mobile resources
  resolve: {
    alias: {
      // Shared packages
      '@shared/components': path.resolve(__dirname, '../../packages/shared-nexpo/shared-components/src'),
      '@shared/ui': path.resolve(__dirname, '../../packages/shared-nexpo/shared-ui/src'),
      '@shared/utils': path.resolve(__dirname, '../../packages/shared-nexpo/shared-utils/src'),
      '@shared/provider': path.resolve(__dirname, '../../packages/shared-nexpo/shared-provider/src'),
      '@shared/fonts': path.resolve(__dirname, '../../packages/shared-nexpo/shared-fonts'),
      '@shared/tauri': path.resolve(__dirname, '../../packages/shared-taurte/src'),
      // Local Svelte aliases
      '$lib': path.resolve(__dirname, './src/lib'),
      '$components': path.resolve(__dirname, './src/components'),
      '$stores': path.resolve(__dirname, './src/stores'),
      '$utils': path.resolve(__dirname, './src/utils'),
    },
  },
  
  clearScreen: false,
  server: {
    port: 1420,
    strictPort: true,
    watch: {
      ignored: ['**/src-tauri/**'],
    },
  },
  envPrefix: ['VITE_', 'TAURI_'],
  build: {
    target: process.env.TAURI_PLATFORM === 'windows' ? 'chrome105' : 'safari13',
    minify: !process.env.TAURI_DEBUG ? 'esbuild' : false,
    sourcemap: !!process.env.TAURI_DEBUG,
  },
}));
