// vite.config.ts
import { defineConfig } from "file:///E:/Workspace/Templates/Weave/node_modules/.pnpm/vite@5.4.21_@types+node@22._5376eb9bdd14cf65c5e2e3cc7f37959e/node_modules/vite/dist/node/index.js";
import { svelte } from "file:///E:/Workspace/Templates/Weave/node_modules/.pnpm/@sveltejs+vite-plugin-svelt_b0e1660ae0278e6bc74b5a5db19af1d3/node_modules/@sveltejs/vite-plugin-svelte/src/index.js";
import path from "path";
var __vite_injected_original_dirname = "E:\\Workspace\\Templates\\Weave\\apps\\taurte\\svelteWeb";
var vite_config_default = defineConfig({
  plugins: [svelte()],
  // Reduce console noise
  logLevel: "info",
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
      "@": path.resolve(__vite_injected_original_dirname, "src"),
      "@shared/components": path.resolve(__vite_injected_original_dirname, "../../packages/shared-components/src"),
      "@shared/ui": path.resolve(__vite_injected_original_dirname, "../../packages/shared-ui/src"),
      "@shared/utils": path.resolve(__vite_injected_original_dirname, "../../packages/shared-utils/src"),
      "@shared/provider": path.resolve(__vite_injected_original_dirname, "../../packages/shared-provider/src"),
      "@shared/hooks": path.resolve(__vite_injected_original_dirname, "../../packages/shared-hooks/src")
    }
  },
  build: {
    outDir: "dist",
    sourcemap: true
  },
  define: {
    global: "globalThis"
  }
});
export {
  vite_config_default as default
};
//# sourceMappingURL=data:application/json;base64,ewogICJ2ZXJzaW9uIjogMywKICAic291cmNlcyI6IFsidml0ZS5jb25maWcudHMiXSwKICAic291cmNlc0NvbnRlbnQiOiBbImNvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9kaXJuYW1lID0gXCJFOlxcXFxXb3Jrc3BhY2VcXFxcVGVtcGxhdGVzXFxcXFdlYXZlXFxcXGFwcHNcXFxcdGF1cnRlXFxcXHN2ZWx0ZVdlYlwiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9maWxlbmFtZSA9IFwiRTpcXFxcV29ya3NwYWNlXFxcXFRlbXBsYXRlc1xcXFxXZWF2ZVxcXFxhcHBzXFxcXHRhdXJ0ZVxcXFxzdmVsdGVXZWJcXFxcdml0ZS5jb25maWcudHNcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfaW1wb3J0X21ldGFfdXJsID0gXCJmaWxlOi8vL0U6L1dvcmtzcGFjZS9UZW1wbGF0ZXMvV2VhdmUvYXBwcy90YXVydGUvc3ZlbHRlV2ViL3ZpdGUuY29uZmlnLnRzXCI7aW1wb3J0IHsgZGVmaW5lQ29uZmlnIH0gZnJvbSAndml0ZSc7XG5pbXBvcnQgeyBzdmVsdGUgfSBmcm9tICdAc3ZlbHRlanMvdml0ZS1wbHVnaW4tc3ZlbHRlJztcbmltcG9ydCBwYXRoIGZyb20gJ3BhdGgnO1xuXG4vLyBodHRwczovL3ZpdGVqcy5kZXYvY29uZmlnL1xuZXhwb3J0IGRlZmF1bHQgZGVmaW5lQ29uZmlnKHtcbiAgcGx1Z2luczogW3N2ZWx0ZSgpXSxcbiAgXG4gIC8vIFJlZHVjZSBjb25zb2xlIG5vaXNlXG4gIGxvZ0xldmVsOiAnaW5mbycsXG4gIFxuICBzZXJ2ZXI6IHtcbiAgICBwb3J0OiA1MTczLFxuICAgIGhvc3Q6IHRydWVcbiAgfSxcbiAgcHJldmlldzoge1xuICAgIHBvcnQ6IDUxNzMsXG4gICAgaG9zdDogdHJ1ZVxuICB9LFxuICByZXNvbHZlOiB7XG4gICAgYWxpYXM6IHtcbiAgICAgICdAJzogcGF0aC5yZXNvbHZlKF9fZGlybmFtZSwgJ3NyYycpLFxuICAgICAgJ0BzaGFyZWQvY29tcG9uZW50cyc6IHBhdGgucmVzb2x2ZShfX2Rpcm5hbWUsICcuLi8uLi9wYWNrYWdlcy9zaGFyZWQtY29tcG9uZW50cy9zcmMnKSxcbiAgICAgICdAc2hhcmVkL3VpJzogcGF0aC5yZXNvbHZlKF9fZGlybmFtZSwgJy4uLy4uL3BhY2thZ2VzL3NoYXJlZC11aS9zcmMnKSxcbiAgICAgICdAc2hhcmVkL3V0aWxzJzogcGF0aC5yZXNvbHZlKF9fZGlybmFtZSwgJy4uLy4uL3BhY2thZ2VzL3NoYXJlZC11dGlscy9zcmMnKSxcbiAgICAgICdAc2hhcmVkL3Byb3ZpZGVyJzogcGF0aC5yZXNvbHZlKF9fZGlybmFtZSwgJy4uLy4uL3BhY2thZ2VzL3NoYXJlZC1wcm92aWRlci9zcmMnKSxcbiAgICAgICdAc2hhcmVkL2hvb2tzJzogcGF0aC5yZXNvbHZlKF9fZGlybmFtZSwgJy4uLy4uL3BhY2thZ2VzL3NoYXJlZC1ob29rcy9zcmMnKVxuICAgIH1cbiAgfSxcbiAgYnVpbGQ6IHtcbiAgICBvdXREaXI6ICdkaXN0JyxcbiAgICBzb3VyY2VtYXA6IHRydWVcbiAgfSxcbiAgZGVmaW5lOiB7XG4gICAgZ2xvYmFsOiAnZ2xvYmFsVGhpcydcbiAgfVxufSk7XG4iXSwKICAibWFwcGluZ3MiOiAiO0FBQXNWLFNBQVMsb0JBQW9CO0FBQ25YLFNBQVMsY0FBYztBQUN2QixPQUFPLFVBQVU7QUFGakIsSUFBTSxtQ0FBbUM7QUFLekMsSUFBTyxzQkFBUSxhQUFhO0FBQUEsRUFDMUIsU0FBUyxDQUFDLE9BQU8sQ0FBQztBQUFBO0FBQUEsRUFHbEIsVUFBVTtBQUFBLEVBRVYsUUFBUTtBQUFBLElBQ04sTUFBTTtBQUFBLElBQ04sTUFBTTtBQUFBLEVBQ1I7QUFBQSxFQUNBLFNBQVM7QUFBQSxJQUNQLE1BQU07QUFBQSxJQUNOLE1BQU07QUFBQSxFQUNSO0FBQUEsRUFDQSxTQUFTO0FBQUEsSUFDUCxPQUFPO0FBQUEsTUFDTCxLQUFLLEtBQUssUUFBUSxrQ0FBVyxLQUFLO0FBQUEsTUFDbEMsc0JBQXNCLEtBQUssUUFBUSxrQ0FBVyxzQ0FBc0M7QUFBQSxNQUNwRixjQUFjLEtBQUssUUFBUSxrQ0FBVyw4QkFBOEI7QUFBQSxNQUNwRSxpQkFBaUIsS0FBSyxRQUFRLGtDQUFXLGlDQUFpQztBQUFBLE1BQzFFLG9CQUFvQixLQUFLLFFBQVEsa0NBQVcsb0NBQW9DO0FBQUEsTUFDaEYsaUJBQWlCLEtBQUssUUFBUSxrQ0FBVyxpQ0FBaUM7QUFBQSxJQUM1RTtBQUFBLEVBQ0Y7QUFBQSxFQUNBLE9BQU87QUFBQSxJQUNMLFFBQVE7QUFBQSxJQUNSLFdBQVc7QUFBQSxFQUNiO0FBQUEsRUFDQSxRQUFRO0FBQUEsSUFDTixRQUFRO0FBQUEsRUFDVjtBQUNGLENBQUM7IiwKICAibmFtZXMiOiBbXQp9Cg==
