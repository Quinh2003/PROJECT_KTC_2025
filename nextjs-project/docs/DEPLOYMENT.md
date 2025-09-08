# 🚀 Deployment & Setup - Fast Route Logistics

## 📋 Tổng quan Deployment

Hướng dẫn chi tiết cách thiết lập, phát triển và triển khai ứng dụng Fast Route Logistics Next.js trên các môi trường khác nhau.

## 🛠️ Development Setup

### 1. Yêu cầu hệ thống
```bash
# Node.js version
node --version  # >= 18.0.0

# Package managers (chọn 1)
npm --version   # >= 8.0.0
yarn --version  # >= 1.22.0
pnpm --version  # >= 7.0.0
```

### 2. Clone và cài đặt
```bash
# Clone repository
git clone https://github.com/Quinh2003/PROJECT_KTC_2025.git
cd PROJECT_KTC_2025/nextjs-project

# Cài đặt dependencies
npm install
# or
yarn install
# or
pnpm install
```

### 3. Environment Configuration
```bash
# Tạo file .env.local
cp .env.example .env.local
```

```env
# .env.local
# Application
NEXT_PUBLIC_APP_NAME=Fast Route Logistics
NEXT_PUBLIC_APP_VERSION=1.0.0

# API Configuration
NEXT_PUBLIC_API_BASE_URL=http://localhost:8080
NEXT_PUBLIC_API_TIMEOUT=10000

# Authentication
NEXTAUTH_URL=http://localhost:3000
NEXTAUTH_SECRET=your-super-secret-jwt-secret-key

# Firebase Configuration
NEXT_PUBLIC_FIREBASE_API_KEY=AIzaSyC8sEJfuwq68qG8_pDcQ_3KtLK6OatgAjk
NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN=fastrouter.firebaseapp.com
NEXT_PUBLIC_FIREBASE_PROJECT_ID=fastrouter
NEXT_PUBLIC_FIREBASE_STORAGE_BUCKET=fastrouter.appspot.com
NEXT_PUBLIC_FIREBASE_MESSAGING_SENDER_ID=815309877404
NEXT_PUBLIC_FIREBASE_APP_ID=1:815309877404:web:727163009f18188c0ffd59

# Development
NODE_ENV=development
NEXT_PUBLIC_DEBUG=true
```

### 4. Start Development Server
```bash
# Development với turbopack (faster)
npm run dev

# Development thông thường
npm run dev -- --no-turbopack

# Specify port
npm run dev -- --port 3001
```

### 5. Verify Setup
```bash
# Check if server is running
curl http://localhost:3000/api/health

# Run linting
npm run lint

# Run type checking
npx tsc --noEmit
```

## 🧪 Testing Setup

### 1. Test Dependencies
```json
{
  "devDependencies": {
    "@testing-library/react": "^13.4.0",
    "@testing-library/jest-dom": "^5.16.5",
    "@testing-library/user-event": "^14.4.3",
    "jest": "^29.5.0",
    "jest-environment-jsdom": "^29.5.0"
  }
}
```

### 2. Jest Configuration
```javascript
// jest.config.js
module.exports = {
  testEnvironment: 'jsdom',
  setupFilesAfterEnv: ['<rootDir>/jest.setup.js'],
  moduleNameMapping: {
    '^@/(.*)$': '<rootDir>/src/$1',
  },
  testPathIgnorePatterns: ['<rootDir>/.next/', '<rootDir>/node_modules/'],
  collectCoverageFrom: [
    'src/**/*.{js,jsx,ts,tsx}',
    '!src/**/*.d.ts',
  ],
}
```

### 3. Test Setup File
```javascript
// jest.setup.js
import '@testing-library/jest-dom'

// Mock Next.js router
jest.mock('next/router', () => ({
  useRouter() {
    return {
      route: '/',
      pathname: '/',
      query: {},
      asPath: '/',
      push: jest.fn(),
      pop: jest.fn(),
      reload: jest.fn(),
      back: jest.fn(),
      prefetch: jest.fn(),
      beforePopState: jest.fn(),
      events: {
        on: jest.fn(),
        off: jest.fn(),
        emit: jest.fn(),
      },
    }
  },
}))

// Mock environment variables
process.env.NEXT_PUBLIC_API_BASE_URL = 'http://localhost:8080'
```

### 4. Run Tests
```bash
# Run all tests
npm run test

# Run tests in watch mode
npm run test:watch

# Run tests with coverage
npm run test:coverage

# Run specific test file
npm run test LoginForm.test.tsx
```

## 🏗️ Build Process

### 1. Production Build
```bash
# Build application
npm run build

# Analyze bundle size
npm run analyze

# Start production server
npm run start
```

### 2. Build Optimization
```typescript
// next.config.ts
import type { NextConfig } from 'next'

const nextConfig: NextConfig = {
  // Output settings
  output: 'standalone', // For Docker deployment
  
  // Performance optimizations
  experimental: {
    optimizeCss: true,
    optimizePackageImports: ['antd', 'react-icons'],
  },
  
  // Bundle analyzer
  webpack: (config, { isServer }) => {
    if (!isServer) {
      config.resolve.fallback = {
        ...config.resolve.fallback,
        fs: false,
      }
    }
    return config
  },
  
  // Image optimization
  images: {
    domains: ['localhost', 'api.example.com'],
    formats: ['image/webp', 'image/avif'],
  },
  
  // Compression
  compress: true,
  
  // Security headers
  async headers() {
    return [
      {
        source: '/(.*)',
        headers: [
          {
            key: 'X-Frame-Options',
            value: 'DENY',
          },
          {
            key: 'X-Content-Type-Options',
            value: 'nosniff',
          },
          {
            key: 'Referrer-Policy',
            value: 'origin-when-cross-origin',
          },
        ],
      },
    ]
  },
}

export default nextConfig
```

### 3. Bundle Analysis
```bash
# Install bundle analyzer
npm install --save-dev @next/bundle-analyzer

# Add script to package.json
"analyze": "ANALYZE=true npm run build"

# Run analysis
npm run analyze
```

## 🐳 Docker Deployment

### 1. Dockerfile
```dockerfile
# Dockerfile
FROM node:18-alpine AS base

# Install dependencies
FROM base AS deps
RUN apk add --no-cache libc6-compat
WORKDIR /app

# Install dependencies based on the preferred package manager
COPY package.json yarn.lock* package-lock.json* pnpm-lock.yaml* ./
RUN \
  if [ -f yarn.lock ]; then yarn --frozen-lockfile; \
  elif [ -f package-lock.json ]; then npm ci; \
  elif [ -f pnpm-lock.yaml ]; then yarn global add pnpm && pnpm i --frozen-lockfile; \
  else echo "Lockfile not found." && exit 1; \
  fi

# Build the app
FROM base AS builder
WORKDIR /app
COPY --from=deps /app/node_modules ./node_modules
COPY . .

# Set environment variables
ENV NEXT_TELEMETRY_DISABLED 1
ENV NODE_ENV production

# Build application
RUN npm run build

# Production image
FROM base AS runner
WORKDIR /app

ENV NODE_ENV production
ENV NEXT_TELEMETRY_DISABLED 1

RUN addgroup --system --gid 1001 nodejs
RUN adduser --system --uid 1001 nextjs

# Copy built application
COPY --from=builder /app/public ./public
COPY --from=builder --chown=nextjs:nodejs /app/.next/standalone ./
COPY --from=builder --chown=nextjs:nodejs /app/.next/static ./.next/static

USER nextjs

EXPOSE 3000

ENV PORT 3000
ENV HOSTNAME "0.0.0.0"

CMD ["node", "server.js"]
```

### 2. Docker Compose
```yaml
# docker-compose.yml
version: '3.8'

services:
  nextjs-app:
    build:
      context: .
      dockerfile: Dockerfile
    ports:
      - "3000:3000"
    environment:
      - NODE_ENV=production
      - NEXT_PUBLIC_API_BASE_URL=http://spring-api:8080
    depends_on:
      - spring-api
    networks:
      - app-network

  spring-api:
    image: fanglee2003/ktc-logistics-backend
    ports:
      - "8080:8080"
    environment:
      - SPRING_PROFILES_ACTIVE=production
    networks:
      - app-network

networks:
  app-network:
    driver: bridge
```

### 3. Docker Commands
```bash
# Build image
docker build -t fast-route-frontend .

# Run container
docker run -p 3000:3000 fast-route-frontend

# Run with docker-compose
docker-compose up -d

# View logs
docker-compose logs -f nextjs-app

# Stop services
docker-compose down
```

## ☁️ Vercel Deployment

### 1. Vercel Configuration
```json
// vercel.json
{
  "buildCommand": "npm run build",
  "outputDirectory": ".next",
  "framework": "nextjs",
  "installCommand": "npm install",
  "functions": {
    "app/api/**/*.ts": {
      "maxDuration": 30
    }
  },
  "regions": ["hnd1", "sin1"],
  "env": {
    "NEXT_PUBLIC_API_BASE_URL": "https://api.fastroute.com"
  },
  "build": {
    "env": {
      "NEXT_TELEMETRY_DISABLED": "1"
    }
  }
}
```

### 2. Deploy Commands
```bash
# Install Vercel CLI
npm install -g vercel

# Login to Vercel
vercel login

# Deploy to preview
vercel

# Deploy to production
vercel --prod

# Set environment variables
vercel env add NEXT_PUBLIC_API_BASE_URL production
vercel env add NEXTAUTH_SECRET production
```

### 3. GitHub Integration
```yaml
# .github/workflows/vercel.yml
name: Vercel Production Deployment
env:
  VERCEL_ORG_ID: ${{ secrets.VERCEL_ORG_ID }}
  VERCEL_PROJECT_ID: ${{ secrets.VERCEL_PROJECT_ID }}

on:
  push:
    branches:
      - main

jobs:
  Deploy-Production:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      
      - name: Install Vercel CLI
        run: npm install --global vercel@latest
        
      - name: Pull Vercel Environment Information
        run: vercel pull --yes --environment=production --token=${{ secrets.VERCEL_TOKEN }}
        
      - name: Build Project Artifacts
        run: vercel build --prod --token=${{ secrets.VERCEL_TOKEN }}
        
      - name: Deploy Project Artifacts to Vercel
        run: vercel deploy --prebuilt --prod --token=${{ secrets.VERCEL_TOKEN }}
```

## 🌐 Production Deployment

### 1. Server Requirements
```bash
# Minimum server specifications
CPU: 2 cores
RAM: 4GB
Storage: 20GB SSD
Network: 100Mbps

# Recommended for production
CPU: 4 cores
RAM: 8GB
Storage: 50GB SSD
Network: 1Gbps
```

### 2. Nginx Configuration
```nginx
# /etc/nginx/sites-available/fastroute
server {
    listen 80;
    server_name yourdomain.com www.yourdomain.com;
    
    # Redirect to HTTPS
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name yourdomain.com www.yourdomain.com;
    
    # SSL Configuration
    ssl_certificate /path/to/certificate.crt;
    ssl_certificate_key /path/to/private.key;
    ssl_protocols TLSv1.2 TLSv1.3;
    ssl_ciphers ECDHE-RSA-AES256-GCM-SHA512:DHE-RSA-AES256-GCM-SHA512;
    
    # Security headers
    add_header X-Frame-Options DENY;
    add_header X-Content-Type-Options nosniff;
    add_header X-XSS-Protection "1; mode=block";
    add_header Strict-Transport-Security "max-age=31536000; includeSubDomains";
    
    # Proxy to Next.js app
    location / {
        proxy_pass http://localhost:3000;
        proxy_http_version 1.1;
        proxy_set_header Upgrade $http_upgrade;
        proxy_set_header Connection 'upgrade';
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        proxy_cache_bypass $http_upgrade;
    }
    
    # Static files caching
    location /_next/static/ {
        proxy_pass http://localhost:3000;
        add_header Cache-Control "public, max-age=31536000, immutable";
    }
    
    # API proxy
    location /api/ {
        proxy_pass http://localhost:8080;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

### 3. Process Manager (PM2)
```javascript
// ecosystem.config.js
module.exports = {
  apps: [{
    name: 'fast-route-frontend',
    script: 'server.js',
    instances: 'max',
    exec_mode: 'cluster',
    env: {
      NODE_ENV: 'production',
      PORT: 3000,
    },
    env_production: {
      NODE_ENV: 'production',
      PORT: 3000,
      NEXT_PUBLIC_API_BASE_URL: 'https://api.yourdomain.com'
    },
    error_file: './logs/err.log',
    out_file: './logs/out.log',
    log_file: './logs/combined.log',
    time: true
  }]
}
```

```bash
# Install PM2
npm install -g pm2

# Start application
pm2 start ecosystem.config.js --env production

# Monitor
pm2 monit

# Logs
pm2 logs fast-route-frontend

# Restart
pm2 restart fast-route-frontend

# Auto-start on boot
pm2 startup
pm2 save
```

## 📊 Monitoring & Analytics

### 1. Application Monitoring
```typescript
// lib/monitoring.ts
import { getCLS, getFID, getFCP, getLCP, getTTFB } from 'web-vitals'

export function reportWebVitals(metric: any) {
  // Send to analytics service
  if (process.env.NODE_ENV === 'production') {
    // Example: Google Analytics
    gtag('event', metric.name, {
      custom_parameter_1: metric.value,
      custom_parameter_2: metric.label,
    })
    
    // Example: Custom API
    fetch('/api/analytics', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(metric)
    })
  }
}

// Measure Core Web Vitals
export function measurePerformance() {
  getCLS(reportWebVitals)
  getFID(reportWebVitals)
  getFCP(reportWebVitals)
  getLCP(reportWebVitals)
  getTTFB(reportWebVitals)
}
```

### 2. Error Tracking (Sentry)
```typescript
// lib/sentry.ts
import * as Sentry from '@sentry/nextjs'

Sentry.init({
  dsn: process.env.NEXT_PUBLIC_SENTRY_DSN,
  environment: process.env.NODE_ENV,
  tracesSampleRate: 1.0,
  beforeSend(event) {
    // Filter out non-production errors
    if (process.env.NODE_ENV !== 'production') {
      return null
    }
    return event
  }
})
```

### 3. Health Check Endpoint
```typescript
// app/api/health/route.ts
export async function GET() {
  try {
    // Check database connection
    const dbStatus = await checkDatabaseConnection()
    
    // Check external API
    const apiStatus = await checkExternalAPI()
    
    const health = {
      status: 'ok',
      timestamp: new Date().toISOString(),
      version: process.env.NEXT_PUBLIC_APP_VERSION,
      checks: {
        database: dbStatus,
        api: apiStatus,
      }
    }
    
    return Response.json(health)
  } catch (error) {
    return Response.json(
      { status: 'error', message: error.message },
      { status: 500 }
    )
  }
}
```

## 🔒 Security Considerations

### 1. Environment Variables Security
```bash
# Use secrets management
export NEXTAUTH_SECRET=$(openssl rand -base64 32)

# Avoid hardcoding in .env files for production
# Use cloud provider secrets (AWS Secrets Manager, etc.)
```

### 2. CSP Headers
```typescript
// next.config.ts
const ContentSecurityPolicy = `
  default-src 'self';
  script-src 'self' 'unsafe-eval' 'unsafe-inline' *.googletagmanager.com;
  child-src *.youtube.com *.google.com *.twitter.com;
  style-src 'self' 'unsafe-inline' *.googleapis.com;
  img-src * blob: data:;
  media-src 'none';
  connect-src *;
  font-src 'self' *.gstatic.com;
`

const nextConfig = {
  async headers() {
    return [
      {
        source: '/(.*)',
        headers: [
          {
            key: 'Content-Security-Policy',
            value: ContentSecurityPolicy.replace(/\n/g, ''),
          },
        ],
      },
    ]
  },
}
```

## 📚 Troubleshooting

### 1. Common Issues
```bash
# Build fails
rm -rf .next node_modules
npm install
npm run build

# Port already in use
lsof -ti:3000 | xargs kill -9
npm run dev

# Environment variables not loading
# Check .env.local exists and has correct format
# Restart development server

# TypeScript errors
npx tsc --noEmit
npm run lint -- --fix
```

### 2. Performance Issues
```typescript
// Bundle size analysis
npm run analyze

// Check for unused dependencies
npm install -g depcheck
depcheck

// Optimize images
npm install sharp
// Next.js will automatically use sharp for image optimization
```

### 3. Deployment Issues
```bash
# Check logs
docker logs container-name
pm2 logs

# Test API connectivity
curl -X GET https://api.yourdomain.com/health

# Check SSL certificate
openssl s_client -connect yourdomain.com:443
```

---

Hướng dẫn này cung cấp đầy đủ thông tin để setup, phát triển và deploy ứng dụng Fast Route Logistics một cách professional và scalable.