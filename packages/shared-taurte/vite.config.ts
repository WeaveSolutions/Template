import { defineConfig } from 'vite';
import { svelte } from '@sveltejs/vite-plugin-svelte';
import path from 'path';

export default defineConfig({
  plugins: [svelte()],
  
  resolve: {
    alias: {
      '$lib': path.resolve(__dirname, './src/lib'),
      '$components': path.resolve(__dirname, './src/components'),
      '$stores': path.resolve(__dirname, './src/stores'),
      '$utils': path.resolve(__dirname, './src/utils'),
      '$types': path.resolve(__dirname, './src/types'),
    },
  },

  build: {
    lib: {
      entry: path.resolve(__dirname, 'src/index.ts'),
      name: 'SharedTauri',
      fileName: 'index',
      formats: ['es']
    },
    rollupOptions: {
      external: ['svelte', '@tauri-apps/api', '@tauri-apps/plugin-shell'],
      output: {
        globals: {
          svelte: 'Svelte'
        }
      }
    }
  },

  test: {
    environment: 'jsdom'
  }
});
