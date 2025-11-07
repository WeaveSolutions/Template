import { defineConfig } from 'vite';
import { svelte } from '@sveltejs/vite-plugin-svelte';
import path from 'path';

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [svelte()],
  
  // Reduce console noise
  logLevel: 'info',
  
  server: {
    port: 5173,
    host: true
  },
  preview: {
    port: 5173,
    host: true
  },
  resolve: {
    alias: {
      '@': path.resolve(__dirname, 'src'),
      '@shared/components': path.resolve(__dirname, '../../packages/shared-components/src'),
      '@shared/ui': path.resolve(__dirname, '../../packages/shared-ui/src'),
      '@shared/utils': path.resolve(__dirname, '../../packages/shared-utils/src'),
      '@shared/provider': path.resolve(__dirname, '../../packages/shared-provider/src'),
      '@shared/hooks': path.resolve(__dirname, '../../packages/shared-hooks/src')
    }
  },
  build: {
    outDir: 'dist',
    sourcemap: true
  },
  define: {
    global: 'globalThis'
  }
});
