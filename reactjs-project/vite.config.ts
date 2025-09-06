import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'

// https://vite.dev/config/
export default defineConfig({
  plugins: [react()],
  server: {
    proxy: {
<<<<<<< HEAD
      '/api': 'http://localhost:8080'
    }
=======
      '/api': {
        target: 'http://localhost:8080',
        changeOrigin: true,
        secure: false,
      },
    },
>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00
  },
})
