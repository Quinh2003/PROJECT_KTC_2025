# 🚀 Fast Route Logistics - Customer Portal (Next.js)

Ứng dụng web dành cho khách hàng của hệ thống logistics Fast Route, được xây dựng với Next.js 15 và TypeScript.

## 📋 Tổng quan

Fast Route Customer Portal là một ứng dụng web hiện đại cho phép khách hàng:
- Đăng ký và quản lý tài khoản
- Tạo và theo dõi đơn hàng
- Tính toán chi phí vận chuyển
- Quản lý thông tin cá nhân
- Xem lịch sử giao dịch

## 🏗️ Kiến trúc và Công nghệ

### Tech Stack
- **Framework**: Next.js 15.4.2 (App Router)
- **Language**: TypeScript 5.x
- **UI Framework**: Ant Design 5.27.1
- **Styling**: Tailwind CSS 4.x
- **State Management**: TanStack React Query 5.85.9
- **Authentication**: Firebase Auth + Custom JWT
- **HTTP Client**: Axios 1.11.0
- **Icons**: React Icons 5.5.0 + Ant Design Icons 6.0.0

### Kiến trúc App Router
```
src/app/
├── (auth)/           # Authentication routes (login, register)
├── (public)/         # Public landing pages
├── account/          # Protected customer area
├── api/             # API routes (NextAuth)
├── unauthorized/    # Access denied page
└── page.tsx         # Root page
```

## 🔗 Tích hợp với Spring Boot Backend

### Base URL
```
Production: http://localhost:8080
API Prefix: /api
```

### Authentication Flow
1. **Login**: `POST /api/auth/login`
2. **Google OAuth**: `POST /api/auth/google-login`  
3. **Register**: `POST /api/auth/register`
4. **2FA Verification**: Built-in TOTP support

### API Endpoints Integration
```typescript
// Authentication
POST /api/auth/login
POST /api/auth/google-login
POST /api/auth/register

// Orders
GET /api/orders
POST /api/orders
GET /api/orders/{id}
PATCH /api/orders/{id}
DELETE /api/orders/{id}
GET /api/orders/{id}/tracking
GET /api/orders/user/{userId}/summary

// Stores
GET /api/stores
POST /api/stores
GET /api/stores/{id}
PUT /api/stores/{id}
DELETE /api/stores/{id}

// Users
GET /api/users/profile
PUT /api/users/profile
```

## 📁 Cấu trúc thư mục chi tiết

```
nextjs-project/
├── public/
│   ├── favicon.ico
│   └── login.webp           # Login background image
├── src/
│   ├── app/                 # App Router pages
│   │   ├── (auth)/         
│   │   │   ├── login/
│   │   │   │   └── page.tsx
│   │   │   ├── register/
│   │   │   │   └── page.tsx
│   │   │   └── layout.tsx
│   │   ├── (public)/       
│   │   │   └── page.tsx     # Landing page
│   │   ├── account/         # Protected customer area
│   │   │   ├── orders/
│   │   │   │   ├── new/
│   │   │   │   ├── [id]/
│   │   │   │   └── components/
│   │   │   ├── profile/
│   │   │   ├── estimate/
│   │   │   ├── page.tsx     # Dashboard
│   │   │   └── layout.tsx
│   │   ├── api/
│   │   │   ├── auth/
│   │   │   │   └── [...nextauth]/
│   │   │   └── stores/
│   │   ├── globals.css
│   │   ├── layout.tsx       # Root layout
│   │   ├── page.tsx         # Root page
│   │   └── providers.tsx    # Context providers
│   ├── components/          # Reusable components
│   │   ├── forms/
│   │   │   ├── LoginForm.tsx
│   │   │   ├── RegisterForm.tsx
│   │   │   ├── ForgotPasswordForm.tsx
│   │   │   └── TwoFactorForm.tsx
│   │   ├── modals/
│   │   ├── Providers.tsx
│   │   └── index.ts
│   ├── hooks/               # Custom React hooks
│   │   └── useOrders.ts
│   ├── lib/                 # Utility libraries
│   │   ├── auth.ts          # Authentication utilities
│   │   ├── firebase.ts      # Firebase configuration
│   │   ├── pricing.ts       # Pricing calculations
│   │   └── react-query.ts   # React Query setup
│   ├── server/              # Server-side API calls
│   │   ├── auth.api.ts
│   │   ├── order.api.ts
│   │   └── user.api.ts
│   ├── services/            # Business logic services
│   │   ├── orderService.ts
│   │   └── storeService.ts
│   ├── types/               # TypeScript type definitions
│   │   ├── User.ts
│   │   ├── orders.ts
│   │   ├── Store.ts
│   │   └── next-auth.d.ts
│   ├── utils/               # Utility functions
│   │   └── auth.ts
│   └── middleware.ts        # Next.js middleware
├── __tests__/               # Test files
├── .gitignore
├── eslint.config.mjs
├── next.config.ts
├── package.json
├── postcss.config.mjs
└── tsconfig.json
```

## 🔐 Authentication & Authorization

### Phương thức đăng nhập
1. **Email/Password**: Truyền thống với JWT token
2. **Google OAuth**: Sử dụng Firebase Authentication
3. **Two-Factor Authentication**: TOTP support

### Token Management
```typescript
// Cookie-based token storage
setTokenCookie(token: string)
getTokenCookie(): string | undefined
removeTokenCookie()

// JWT utilities
decodeJWT(token: string): any
isTokenExpired(token: string): boolean
requireAuth(redirectUrl?: string): boolean
```

### Protected Routes
```typescript
// middleware.ts
const protectedRoutes = [
  '/account',
  '/(dashboard)',
];
```

## 📱 Tính năng chính

### 1. Authentication System
- ✅ Email/Password login
- ✅ Google OAuth integration
- ✅ Two-factor authentication (TOTP)
- ✅ Registration with email verification
- ✅ Forgot password functionality
- ✅ Automatic token refresh
- ✅ Route protection middleware

### 2. Customer Dashboard
- ✅ Welcome overview with statistics
- ✅ Quick access to main functions
- ✅ Recent orders display
- ✅ Account summary

### 3. Order Management
- ✅ Create new orders
- ✅ View order list with filters
- ✅ Order detail view
- ✅ Real-time order tracking
- ✅ Order status updates
- ✅ Order history

### 4. Profile Management
- ✅ View personal information
- ✅ Update profile details
- ✅ Change password
- ✅ Account settings

### 5. Pricing & Estimation
- ✅ Shipping cost calculator
- ✅ Distance-based pricing
- ✅ Service type options
- ✅ Real-time estimates

## 🚀 Getting Started

### Yêu cầu hệ thống
- Node.js 18.0+
- npm, yarn, pnpm hoặc bun

### Cài đặt

1. **Clone repository**
```bash
git clone <repository-url>
cd nextjs-project
```

2. **Cài đặt dependencies**
```bash
npm install
# or
yarn install
# or
pnpm install
```

3. **Cấu hình environment variables**
```bash
# .env.local
NEXTAUTH_URL=http://localhost:3000
NEXTAUTH_SECRET=your-secret-key

# Firebase Configuration
NEXT_PUBLIC_FIREBASE_API_KEY=your-api-key
NEXT_PUBLIC_FIREBASE_AUTH_DOMAIN=your-auth-domain
NEXT_PUBLIC_FIREBASE_PROJECT_ID=your-project-id

# Backend API
NEXT_PUBLIC_API_BASE_URL=http://localhost:8080
```

4. **Chạy development server**
```bash
npm run dev
# or
yarn dev
# or
pnpm dev
```

5. **Mở trình duyệt**
Truy cập [http://localhost:3000](http://localhost:3000)

### Build Production

```bash
# Build project
npm run build

# Start production server
npm run start

# Lint code
npm run lint
```

## 🔄 Luồng dữ liệu (Data Flow)

### 1. Authentication Flow
```
User Login → Firebase/Custom Auth → JWT Token → Local Storage + Cookies → Protected Routes
```

### 2. API Communication Flow
```
React Component → Custom Hook (useOrders) → TanStack Query → Service Layer → Axios → Spring Boot API
```

### 3. State Management Flow
```
Server State: TanStack React Query
UI State: React useState/useReducer
Global State: React Context (minimal usage)
```

## 🧪 Testing

### Cấu trúc test
```
__tests__/
├── components/
├── pages/
├── hooks/
└── utils/
```

### Chạy tests
```bash
npm run test
npm run test:watch
npm run test:coverage
```

## 🛡️ Security Features

### 1. Route Protection
- Middleware-based authentication
- Token expiration validation
- Automatic redirect to login

### 2. Data Security
- JWT token with expiration
- Secure cookie storage
- HTTPS enforcement in production
- XSS protection

### 3. Input Validation
- Form validation with Ant Design
- Type safety with TypeScript
- API response validation

## 🔧 Configuration Files

### next.config.ts
```typescript
const nextConfig = {
  typescript: {
    ignoreBuildErrors: false,
  },
  eslint: {
    ignoreDuringBuilds: false,
  },
};
```

### tailwind.config.js
```javascript
module.exports = {
  content: ['./src/**/*.{js,ts,jsx,tsx}'],
  theme: {
    extend: {},
  },
  plugins: [],
};
```

## 📊 Performance Optimization

### 1. Code Splitting
- Automatic route-based splitting
- Dynamic imports for heavy components
- Lazy loading for non-critical features

### 2. Caching Strategy
- TanStack Query for API caching
- Browser caching for static assets
- CDN integration ready

### 3. Bundle Optimization
- Tree shaking enabled
- Dead code elimination
- Optimized dependencies

## 🚀 Deployment

### Vercel (Recommended)
```bash
# Deploy to Vercel
vercel --prod
```

### Docker
```dockerfile
FROM node:18-alpine
WORKDIR /app
COPY package*.json ./
RUN npm ci --only=production
COPY . .
RUN npm run build
EXPOSE 3000
CMD ["npm", "start"]
```

### Environment Variables for Production
```bash
NEXTAUTH_URL=https://your-domain.com
NEXTAUTH_SECRET=production-secret
NEXT_PUBLIC_API_BASE_URL=https://api.your-domain.com
```

## 🐛 Troubleshooting

### Common Issues

1. **Token expiration**
```typescript
// Check token validity
const isValid = !isTokenExpired(getToken()!);
```

2. **API connection issues**
```typescript
// Check backend connectivity
const response = await fetch(`${API_BASE_URL}/health`);
```

3. **Build errors**
```bash
# Clear Next.js cache
rm -rf .next
npm run build
```

## 📚 API Documentation

### Endpoints được sử dụng

#### Authentication
```typescript
// Login
POST /api/auth/login
Body: { email: string, password: string }
Response: { user: User, token: string }

// Google Login  
POST /api/auth/google-login
Body: { accessToken: string }
Response: { user: User, token: string }

// Register
POST /api/auth/register
Body: { email: string, password: string, fullName: string }
Response: { user: User, token: string }
```

#### Orders
```typescript
// Get orders
GET /api/orders
Headers: { Authorization: "Bearer <token>" }
Response: Order[]

// Create order
POST /api/orders
Body: { address: string, items: OrderItem[] }
Response: Order

// Get order by ID
GET /api/orders/{id}
Response: Order

// Update order
PATCH /api/orders/{id}
Body: Partial<Order>
Response: Order

// Order tracking
GET /api/orders/{id}/tracking
Response: TrackingInfo[]
```

## 🤝 Contributing

### Quy tắc phát triển
1. Sử dụng TypeScript cho tất cả code mới
2. Follow ESLint rules
3. Viết tests cho components mới
4. Update documentation khi có thay đổi

### Git Workflow
```bash
# Create feature branch
git checkout -b feature/new-feature

# Commit changes
git commit -m "feat: add new feature"

# Push and create PR
git push origin feature/new-feature
```

## 📝 Changelog

### Version 1.0.0
- ✅ Initial release
- ✅ Authentication system
- ✅ Order management
- ✅ Customer dashboard
- ✅ Integration with Spring Boot backend

## 📞 Support

Để được hỗ trợ, vui lòng:
1. Kiểm tra documentation này
2. Xem issues trên GitHub
3. Liên hệ team phát triển

---

© 2025 Fast Route Logistics. All rights reserved.
