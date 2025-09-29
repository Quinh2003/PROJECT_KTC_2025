# 🖥️ KTC Logistics Management Platform - Admin Dashboard

An administrative dashboard for KTC Logistics built with React 19, TypeScript, and Vite. This web application provides a comprehensive suite of tools for logistics management, integrating dispatcher order management, fleet operations, and performance analytics. The system enables dispatchers, fleet managers, operations managers, and administrators to effectively manage the entire logistics workflow through a modern, responsive interface featuring interactive visualizations and real-time updates.

![KTC Logistics Operations Dashboard](public/dashboard-screenshot.png)

## 📋 Table of Contents

1. [Getting Started](#-getting-started)
2. [Main Features](#-main-features)
3. [Project Structure](#-project-structure)
4. [Tech Stack](#-tech-stack)
5. [Internationalization](#-internationalization)
6. [License & Contact](#-license--contact)

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
   cd PROJECT_KTC_2025/reactjs-project
   ```

2. **Install dependencies**

   ```bash
   pnpm install
   ```

3. **Environment setup**

   ```bash
   cp .env.example .env
   ```

   Configure your environment variables:

   ```env
   VITE_API_URL=http://localhost:8080/api
   VITE_MAPBOX_ACCESS_TOKEN=your-mapbox-token
   VITE_GOOGLE_MAPS_API_KEY=your-google-maps-api-key
   ```

4. **Run development server**
   ```bash
   pnpm dev
   ```
   Open [http://localhost:5173](http://localhost:5173) in your browser.

### Available Scripts

| Command        | Description                      |
| -------------- | -------------------------------- |
| `pnpm dev`     | Start development server         |
| `pnpm build`   | Build production version         |
| `pnpm preview` | Preview production build locally |
| `pnpm lint`    | Run ESLint for code quality      |

### Test Accounts

#### 🔒 **Admin Account**
- **Email**: `admin@ktclogistics.com`
- **Password**: `Admin123456`

#### 🔒 **Operations Manager Account**
- **Email**: `operations@ktclogistics.com`
- **Password**: `Ops123456`

#### 🔒 **Fleet Manager Account**
- **Email**: `fleet@ktclogistics.com`
- **Password**: `Fleet123456`

#### 🔒 **Dispatcher Account**
- **Email**: `dispatcher@ktclogistics.com`
- **Password**: `Disp123456`

## 🚀 Main Features

### 📦 Dispatcher Order Management
- Real-time order allocation and monitoring
- Exception handling and issue resolution
- Order tracking with status updates
- Delivery performance reporting

### 🚚 Fleet Management
- Vehicle assignment and telemetry monitoring
- Driver scheduling and resource allocation
- Vehicle maintenance tracking and alerts
- Fleet performance analytics and reporting

### 📊 Operations Dashboard
- Process oversight and performance monitoring
- Resource allocation and optimization
- Interactive KPIs and business intelligence
- Customizable reporting with export capabilities

### 👥 User & System Administration
- Role-based access control (Admin, Operations Manager, Fleet Manager, Dispatcher)
- User activity monitoring and audit logs
- System configuration and maintenance
- Security management and compliance monitoring

### 🗺️ AI-Assisted Route Planning
- Intelligent route optimization with machine learning
- Heat maps of delivery density and performance
- Geographic data analysis and visualization
- Service area coverage mapping

### 🔔 Real-time Notification System
- Instant alerts for delivery exceptions
- Scheduled reports distribution
- Critical event monitoring
- Custom alert rules configuration

### 🌐 Internationalization (i18n)
- **Dual Language Support**: English and Vietnamese
- **Real-time Language Switching**: No page reload required
- **Comprehensive Coverage**: All dashboards, forms, and components translated
- **Professional UI**: Language switcher with flags and smooth transitions
- **Persistent Selection**: Language preference saved across sessions
- **Role-based Translations**: Specialized terms for each dashboard type

## 🏗️ Project Structure

```
src/
├── assets/              # Static assets like images and icons
├── components/          # Reusable UI components
│   ├── common/          # Shared components (buttons, modals, etc.)
│   ├── charts/          # Chart and data visualization components
│   ├── forms/           # Form components and validation
│   ├── layout/          # Layout components (header, sidebar, etc.)
│   ├── maps/            # Map-related components
│   └── tables/          # Table components for data display
│
├── constants/           # Application constants and configuration
│
├── contexts/            # React context providers
│   ├── AuthContext.tsx  # Authentication context
│   └── ThemeContext.tsx # Theme management context
│
├── data/                # Mock data and data utilities
│
├── hooks/               # Custom React hooks
│   ├── useAuth.ts       # Authentication hook
│   ├── useFetch.ts      # Data fetching hook
│   └── useMap.ts        # Map functionality hook
│
├── pages/               # Application pages
│   ├── dashboard/       # Dashboard page and components
│   ├── fleet/           # Fleet management pages
│   ├── users/           # User management pages
│   ├── analytics/       # Analytics and reporting pages
│   ├── settings/        # System settings pages
│   └── auth/            # Authentication pages
│
├── services/            # API services and data fetching
│   ├── api.ts           # API client setup
│   ├── auth.service.ts  # Authentication service
│   ├── fleet.service.ts # Fleet management service
│   └── user.service.ts  # User management service
│
├── types/               # TypeScript type definitions
│
├── utils/               # Utility functions
│   ├── auth.ts          # Authentication utilities
│   ├── formatting.ts    # Data formatting utilities
│   ├── mapping.ts       # Map-related utilities
│   └── validation.ts    # Form validation utilities
│
├── App.tsx              # Main application component
├── main.tsx             # Application entry point
└── index.css            # Global styles
```

## 🛠️ Tech Stack

### Core Technologies

- **Framework**: React 19.1.0
- **Build Tool**: Vite 7.0.4
- **Language**: TypeScript 5.8.3
- **Styling**: TailwindCSS 3.x (via CDN)

### State Management & Data Fetching

- **Server State**: TanStack Query (React Query) 5.85.3
- **HTTP Client**: Axios 1.11.0

### Mapping & Visualization

- **Maps**: Mapbox GL 2.15.0, React MapGL 7.1.7, Google Maps API
- **Charts**: Chart.js 4.5.0 with react-chartjs-2 5.3.0

### Additional Libraries

- **Icons**: React Icons 5.5.0, Lucide React 0.541.0
- **Routing**: React Router 7.7.1
- **Internationalization**: react-i18next 13.5.0, i18next 23.7.6, i18next-browser-languagedetector 7.2.0

### Development Tools

- **Build Tool**: Vite 7.0.4
- **Linting**: ESLint 9.30.1
- **Package Manager**: pnpm

## 🌐 Internationalization

The application features comprehensive dual-language support (English/Vietnamese) with:

- **🎯 Complete Coverage**: All 5 dashboards fully translated
- **🔄 Real-time Switching**: Instant language changes without page reload
- **💾 Persistent Settings**: Language preference saved across sessions
- **🎨 Professional UI**: Language switcher with country flags
- **📱 Responsive**: Optimized for both desktop and mobile devices

### Quick Usage

```typescript
import { useTranslation } from 'react-i18next';

function MyComponent() {
  const { t } = useTranslation();
  return <h1>{t('dashboard.title')}</h1>;
}
```

### Documentation

- 📚 **[Complete i18n Documentation](./INTERNATIONALIZATION.md)**: Detailed implementation guide
- 🚀 **[Quick Start Guide](./I18N_QUICK_GUIDE.md)**: Fast setup and common patterns

### Supported Languages

| Language | Code | Status | Coverage |
|----------|------|--------|----------|
| English  | `en` | ✅ Complete | 100% |
| Vietnamese | `vi` | ✅ Complete | 100% |

## 📄 License & Contact

Copyright © 2025 KTC Logistics. All rights reserved.

For technical issues or support:
- **Development Team**: admin-team@ktclogistics.com
- **Project Lead**: admin-lead@ktclogistics.com
- Open an issue in the repository for quick assistance

---

© 2025 KTC Logistics. All rights reserved.
