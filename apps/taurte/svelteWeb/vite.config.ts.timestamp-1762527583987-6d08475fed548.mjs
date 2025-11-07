// vite.config.ts
import { defineConfig } from "file:///E:/Workspace/Templates/Weave/node_modules/.pnpm/vite@5.4.21_@types+node@22._5376eb9bdd14cf65c5e2e3cc7f37959e/node_modules/vite/dist/node/index.js";
import { svelte } from "file:///E:/Workspace/Templates/Weave/node_modules/.pnpm/@sveltejs+vite-plugin-svelt_b0e1660ae0278e6bc74b5a5db19af1d3/node_modules/@sveltejs/vite-plugin-svelte/src/index.js";
import path from "path";
var __vite_injected_original_dirname = "E:\\Workspace\\Templates\\Weave\\apps\\taurte\\svelteWeb";
var vite_config_default = defineConfig({
  plugins: [svelte()],
  server: {
    port: 1420,
    host: true
  },
  preview: {
    port: 1420,
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
//# sourceMappingURL=data:application/json;base64,ewogICJ2ZXJzaW9uIjogMywKICAic291cmNlcyI6IFsidml0ZS5jb25maWcudHMiXSwKICAic291cmNlc0NvbnRlbnQiOiBbImNvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9kaXJuYW1lID0gXCJFOlxcXFxXb3Jrc3BhY2VcXFxcVGVtcGxhdGVzXFxcXFdlYXZlXFxcXGFwcHNcXFxcdGF1cnRlXFxcXHN2ZWx0ZVdlYlwiO2NvbnN0IF9fdml0ZV9pbmplY3RlZF9vcmlnaW5hbF9maWxlbmFtZSA9IFwiRTpcXFxcV29ya3NwYWNlXFxcXFRlbXBsYXRlc1xcXFxXZWF2ZVxcXFxhcHBzXFxcXHRhdXJ0ZVxcXFxzdmVsdGVXZWJcXFxcdml0ZS5jb25maWcudHNcIjtjb25zdCBfX3ZpdGVfaW5qZWN0ZWRfb3JpZ2luYWxfaW1wb3J0X21ldGFfdXJsID0gXCJmaWxlOi8vL0U6L1dvcmtzcGFjZS9UZW1wbGF0ZXMvV2VhdmUvYXBwcy90YXVydGUvc3ZlbHRlV2ViL3ZpdGUuY29uZmlnLnRzXCI7aW1wb3J0IHsgZGVmaW5lQ29uZmlnIH0gZnJvbSAndml0ZSc7XG5pbXBvcnQgeyBzdmVsdGUgfSBmcm9tICdAc3ZlbHRlanMvdml0ZS1wbHVnaW4tc3ZlbHRlJztcbmltcG9ydCBwYXRoIGZyb20gJ3BhdGgnO1xuXG4vLyBodHRwczovL3ZpdGVqcy5kZXYvY29uZmlnL1xuZXhwb3J0IGRlZmF1bHQgZGVmaW5lQ29uZmlnKHtcbiAgcGx1Z2luczogW3N2ZWx0ZSgpXSxcbiAgc2VydmVyOiB7XG4gICAgcG9ydDogMTQyMCxcbiAgICBob3N0OiB0cnVlXG4gIH0sXG4gIHByZXZpZXc6IHtcbiAgICBwb3J0OiAxNDIwLFxuICAgIGhvc3Q6IHRydWVcbiAgfSxcbiAgcmVzb2x2ZToge1xuICAgIGFsaWFzOiB7XG4gICAgICAnQCc6IHBhdGgucmVzb2x2ZShfX2Rpcm5hbWUsICdzcmMnKSxcbiAgICAgICdAc2hhcmVkL2NvbXBvbmVudHMnOiBwYXRoLnJlc29sdmUoX19kaXJuYW1lLCAnLi4vLi4vcGFja2FnZXMvc2hhcmVkLWNvbXBvbmVudHMvc3JjJyksXG4gICAgICAnQHNoYXJlZC91aSc6IHBhdGgucmVzb2x2ZShfX2Rpcm5hbWUsICcuLi8uLi9wYWNrYWdlcy9zaGFyZWQtdWkvc3JjJyksXG4gICAgICAnQHNoYXJlZC91dGlscyc6IHBhdGgucmVzb2x2ZShfX2Rpcm5hbWUsICcuLi8uLi9wYWNrYWdlcy9zaGFyZWQtdXRpbHMvc3JjJyksXG4gICAgICAnQHNoYXJlZC9wcm92aWRlcic6IHBhdGgucmVzb2x2ZShfX2Rpcm5hbWUsICcuLi8uLi9wYWNrYWdlcy9zaGFyZWQtcHJvdmlkZXIvc3JjJyksXG4gICAgICAnQHNoYXJlZC9ob29rcyc6IHBhdGgucmVzb2x2ZShfX2Rpcm5hbWUsICcuLi8uLi9wYWNrYWdlcy9zaGFyZWQtaG9va3Mvc3JjJylcbiAgICB9XG4gIH0sXG4gIGJ1aWxkOiB7XG4gICAgb3V0RGlyOiAnZGlzdCcsXG4gICAgc291cmNlbWFwOiB0cnVlXG4gIH0sXG4gIGRlZmluZToge1xuICAgIGdsb2JhbDogJ2dsb2JhbFRoaXMnXG4gIH1cbn0pO1xuIl0sCiAgIm1hcHBpbmdzIjogIjtBQUFzVixTQUFTLG9CQUFvQjtBQUNuWCxTQUFTLGNBQWM7QUFDdkIsT0FBTyxVQUFVO0FBRmpCLElBQU0sbUNBQW1DO0FBS3pDLElBQU8sc0JBQVEsYUFBYTtBQUFBLEVBQzFCLFNBQVMsQ0FBQyxPQUFPLENBQUM7QUFBQSxFQUNsQixRQUFRO0FBQUEsSUFDTixNQUFNO0FBQUEsSUFDTixNQUFNO0FBQUEsRUFDUjtBQUFBLEVBQ0EsU0FBUztBQUFBLElBQ1AsTUFBTTtBQUFBLElBQ04sTUFBTTtBQUFBLEVBQ1I7QUFBQSxFQUNBLFNBQVM7QUFBQSxJQUNQLE9BQU87QUFBQSxNQUNMLEtBQUssS0FBSyxRQUFRLGtDQUFXLEtBQUs7QUFBQSxNQUNsQyxzQkFBc0IsS0FBSyxRQUFRLGtDQUFXLHNDQUFzQztBQUFBLE1BQ3BGLGNBQWMsS0FBSyxRQUFRLGtDQUFXLDhCQUE4QjtBQUFBLE1BQ3BFLGlCQUFpQixLQUFLLFFBQVEsa0NBQVcsaUNBQWlDO0FBQUEsTUFDMUUsb0JBQW9CLEtBQUssUUFBUSxrQ0FBVyxvQ0FBb0M7QUFBQSxNQUNoRixpQkFBaUIsS0FBSyxRQUFRLGtDQUFXLGlDQUFpQztBQUFBLElBQzVFO0FBQUEsRUFDRjtBQUFBLEVBQ0EsT0FBTztBQUFBLElBQ0wsUUFBUTtBQUFBLElBQ1IsV0FBVztBQUFBLEVBQ2I7QUFBQSxFQUNBLFFBQVE7QUFBQSxJQUNOLFFBQVE7QUFBQSxFQUNWO0FBQ0YsQ0FBQzsiLAogICJuYW1lcyI6IFtdCn0K
