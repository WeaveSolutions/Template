import { vitePreprocess } from '@sveltejs/vite-plugin-svelte';

export default {
  // Consult https://svelte.dev/docs/kit for more information about preprocessors
  preprocess: vitePreprocess(),
  compilerOptions: {
    // Enable run-time checks when not in production
    dev: process.env.NODE_ENV !== 'production'
  }
};
