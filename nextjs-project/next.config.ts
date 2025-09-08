import type { NextConfig } from "next";

const nextConfig: NextConfig = {
  // Disable static export to avoid prerendering issues with Next.js 15
  output: 'standalone',
  
  async rewrites() {
    return [
      {
        source: "/api/:path*",
        destination: "http://localhost:8080/api/:path*", // Spring Boot backend
      },
    ];
  },
  
  // Experimental features for better performance
  experimental: {
    optimizePackageImports: ['antd', 'react-icons'],
  },
};

export default nextConfig;
