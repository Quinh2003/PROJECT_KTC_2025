# 🌐 KTC Logistics Management Platform - B2B Customer Portal

A B2B customer portal for logistics built with Next.js 15, TypeScript, TailwindCSS, and Ant Design. This web application is tailored specifically for business clients to place shipping orders, track deliveries in real-time, estimate shipping costs, manage invoices, and oversee their entire logistics operations from a centralized dashboard. The platform streamlines order management workflows for corporate customers with integration to the KTC Logistics 2025 ecosystem.

![KTC Logistics Portal Screenshot](public/screenshot.png)

## 📋 Table of Contents

1. [Getting Started](#-getting-started)
2. [Main Features](#-main-features)
3. [Project Structure](#-project-structure)
4. [Tech Stack](#-tech-stack)
5. [License & Contact](#-license--contact)

## 🚀 Getting Started

### Prerequisites

- **Node.js**: 18.17.0 or later
- **pnpm**: 8.0.0 or later (recommended package manager)
- **Git**: Latest version
- **Spring Boot Backend**: Running on port 8080 (or configured API URL)

### Installation

1. **Clone the repository**

   ```bash
   git clone https://github.com/Quinh2003/PROJECT_KTC_2025.git
   cd PROJECT_KTC_2025/nextjs-project
   ```

2. **Install dependencies**

   ```bash
   pnpm install
   ```

3. **Environment setup**

   ```bash
   cp .env.example .env.local
   ```

   Configure your environment variables:

   ```env
   NEXT_PUBLIC_API_URL=http://localhost:8080/api
   NEXTAUTH_SECRET=your-nextauth-secret
   NEXTAUTH_URL=http://localhost:3000
   FIREBASE_API_KEY=your-firebase-api-key
   MAPBOX_ACCESS_TOKEN=your-mapbox-token
   ```

4. **Run development server**
   ```bash
   pnpm dev
   ```
   Open [http://localhost:3000](http://localhost:3000) in your browser.

### Available Scripts

| Command           | Description                             |
| ----------------- | --------------------------------------- |
| `pnpm dev`        | Start development server with Turbopack |
| `pnpm build`      | Build production version                |
| `pnpm start`      | Start production server                 |
| `pnpm lint`       | Run ESLint for code quality             |
| `pnpm test`       | Run Jest tests                          |
| `pnpm type-check` | Run TypeScript type checking            |

### Test Accounts

#### 🔒 **Test Corporate Client Account**
- **Email**: `corporate@ktclogistics.com`
- **Password**: `Test123456`

#### 🔒 **Test Enterprise Partner Account**
- **Email**: `partner@ktclogistics.com`
- **Password**: `Test123456`

## 🚀 Main Features

### 📦 Order Placement & Management
- Create single or bulk shipping orders with multiple destinations
- Set priority levels and service tiers for business shipments
- Import order data via Excel/CSV or integrate with ERP systems
- Track order status throughout the entire fulfillment cycle

### 💰 Shipping Cost Estimation
- Real-time price calculation based on distance, weight, and service level
- Volume-based pricing for corporate clients with automated discounts
- Custom rate cards for contracted business partners
- Comparative pricing for different service options

### 🗺️ Real-time Shipment Tracking
- GPS-based tracking of business deliveries with detailed status updates
- SLA compliance monitoring with automated alerts for exceptions
- Proof of delivery verification with digital signatures
- Geofence notifications for warehouse arrivals and departures

### 📊 Business Analytics Dashboard
- Comprehensive logistics KPIs and performance metrics
- Delivery success rates and on-time delivery reporting
- Cost analysis and optimization recommendations
- Customizable views for different business departments

### 💵 Corporate Billing & Invoicing
- Automated monthly invoicing with detailed delivery breakdowns
- Departmental cost allocation for enterprise clients
- Payment integration with corporate accounting systems
- Credit management and payment history tracking

### 👥 Multi-level Access Control
- Department and branch management for enterprise customers
- Role-based permissions for ordering, tracking, and financial operations
- User activity logs and security audit trails
- Integration with corporate SSO (Single Sign-On) systems

## 🏗️ Project Structure

```
src/
├── app/                    # App Router (Next.js 13+)
│   ├── (auth)/            # Auth route group
│   │   ├── layout.tsx     # Auth layout
│   │   ├── login/         # Login page
│   │   └── register/      # Registration page
│   ├── (public)/          # Public pages group
│   ├── (dashboard)/       # Dashboard route group
│   ├── account/           # Protected account pages
│   │   ├── layout.tsx     # Account layout
│   │   ├── orders/        # Order management
│   │   │   ├── [id]/      # Order details page
│   │   │   │   └── tracking/ # Order tracking page
│   │   │   ├── new/       # Create new order
│   │   │   └── components/ # Order-specific components
│   │   ├── profile/       # User profile
│   │   └── estimate/      # Price estimation
│   ├── api/               # API routes
│   │   ├── auth/          # NextAuth configuration
│   │   ├── orders/        # Order-related APIs
│   │   ├── deliveries/    # Delivery tracking APIs
│   │   └── stores/        # Store-related APIs
│   ├── globals.css        # Global styles
│   ├── layout.tsx         # Root layout
│   ├── page.tsx           # Home page
│   └── providers.tsx      # App providers
│
├── components/            # Reusable UI components
│   ├── forms/            # Form components
│   │   ├── LoginForm.tsx
│   │   ├── RegisterForm.tsx
│   │   └── TwoFactorForm.tsx
│   ├── modals/           # Modal components
│   └── index.ts          # Component exports
│
├── hooks/                # Custom React hooks
│   └── useOrders.ts      # Order management hooks
│
├── lib/                  # Utility libraries
│   ├── auth.ts           # Authentication utilities
│   ├── firebase.ts       # Firebase configuration
│   ├── pricing.ts        # Pricing calculations
│   └── react-query.ts    # React Query setup
│
├── server/               # Server-side utilities
│   ├── auth.api.ts       # Authentication APIs
│   ├── order.api.ts      # Order APIs
│   └── user.api.ts       # User APIs
│
├── services/             # Business logic services
│   ├── orderService.ts   # Order service
│   ├── orderFlowService.ts # Order flow logic
│   └── storeService.ts   # Store service
│
├── types/                # TypeScript type definitions
│   ├── User.ts
│   ├── orders.ts
│   ├── Store.ts
│   └── next-auth.d.ts
│
├── utils/                # General utilities
│   ├── auth.ts           # Auth helpers
│   ├── distance.ts       # Distance calculations
│   ├── shipping.ts       # Shipping utilities
│   └── mapbox.ts         # Map integration utilities
│
└── middleware.ts         # Next.js middleware
```

## 🛠️ Tech Stack

### Core Technologies

- **Framework**: Next.js 15.4.2
- **Language**: TypeScript 5.x
- **Styling**: TailwindCSS 4.x
- **UI Library**: Ant Design 5.27.1

### State Management & Data Fetching

- **Server State**: TanStack Query (React Query) 5.85.9
- **Authentication**: NextAuth.js 4.24.11
- **HTTP Client**: Axios 1.11.0

### Mapping & Visualization

- **Maps**: Mapbox GL JS 3.4.x
- **Charts**: Chart.js 4.2.x with React wrapper
- **Visualization**: D3.js for custom data visualization

### Additional Libraries

- **Icons**: React Icons 5.5.0, Ant Design Icons 6.0.0
- **Date Handling**: Day.js 1.11.15
- **QR Codes**: qrcode.react 4.2.0
- **Cookies**: js-cookie 3.0.5
- **Firebase**: Firebase 12.1.0 (Notifications & Analytics)
- **Form Handling**: React Hook Form 7.x with Zod validation

### Development Tools

- **Build Tool**: Turbopack (Next.js)
- **Testing**: Jest 29.x, React Testing Library
- **Linting**: ESLint 9.x
- **Package Manager**: pnpm


## � License & Contact

Copyright © 2025 KTC Logistics. All rights reserved.

For technical issues or support:
- **Development Team**: dev@ktclogistics.com
- **Project Lead**: project@ktclogistics.com
- Open an issue in the repository for quick assistance

---

© 2025 KTC Logistics. All rights reserved.