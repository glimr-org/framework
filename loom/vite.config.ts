/// <reference types="vitest" />
import { defineConfig } from "vite";
import { resolve } from "path";

export default defineConfig({
  resolve: {
    alias: {
      "~": resolve(__dirname, "src"),
    },
  },
  build: {
    lib: {
      entry: resolve(__dirname, "src/index.ts"),
      formats: ["iife"],
      name: "Loom",
      fileName: () => "loom.js",
    },
    outDir: resolve(__dirname, "../priv/static"),
    emptyOutDir: false,
    minify: true,
  },
  test: {
    environment: "happy-dom",
  },
});
