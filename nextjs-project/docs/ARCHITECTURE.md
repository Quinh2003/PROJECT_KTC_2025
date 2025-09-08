# 🏗️ Kiến trúc Next.js Project - Fast Route Logistics

## 📋 Tổng quan Kiến trúc

Dự án được thiết kế theo kiến trúc modular với Next.js App Router, tách biệt rõ ràng giữa presentation layer, business logic, và data layer.

## 🎯 Design Patterns

### 1. Component-Based Architecture
```
├── Components (UI Layer)
├── Pages (Routing Layer)  
├── Hooks (State Management)
├── Services (Business Logic)
├── API Layer (Data Layer)
└── Types (Type Safety)
```

### 2. Separation of Concerns

#### Presentation Layer
- **Components**: Reusable UI components
- **Pages**: Route-specific components
- **Layouts**: Shared layout components

#### Business Logic Layer
- **Hooks**: Custom hooks for state management
- **Services**: API communication logic
- **Utils**: Helper functions

#### Data Layer
- **API Routes**: Next.js API endpoints
- **Server Actions**: Server-side logic
- **Types**: TypeScript definitions

## 🔄 Data Flow Architecture

### 1. Client-Side Data Flow
```
User Interaction → Component → Custom Hook → Service → API Call → Backend
                                    ↓
                           TanStack Query Cache
                                    ↓
                              UI Update
```

### 2. Authentication Flow
```
Login Form → Firebase/JWT Auth → Token Storage → Middleware → Protected Route
```

### 3. State Management Flow
```
Server State: TanStack React Query
├── Cache Management
├── Background Updates
├── Optimistic Updates
└── Error Handling

Client State: React State
├── Form State
├── UI State
└── Component State
```

## 📁 Detailed File Structure

### Core Architecture
```
src/
├── app/                     # Next.js App Router
│   ├── (auth)/             # Auth route group
│   ├── (public)/           # Public route group
│   ├── account/            # Protected customer area
│   ├── api/                # API routes
│   ├── globals.css         # Global styles
│   ├── layout.tsx          # Root layout
│   ├── page.tsx            # Home page
│   └── providers.tsx       # Context providers
├── components/             # Reusable UI components
├── hooks/                  # Custom React hooks
├── lib/                    # Core libraries & configurations
├── server/                 # Server-side API calls
├── services/               # Business logic services
├── types/                  # TypeScript type definitions
├── utils/                  # Utility functions
└── middleware.ts           # Route protection
```

### Component Architecture
```
components/
├── forms/                  # Form components
│   ├── LoginForm.tsx      # Authentication forms
│   ├── RegisterForm.tsx
│   ├── ForgotPasswordForm.tsx
│   └── TwoFactorForm.tsx
├── modals/                # Modal components
├── ui/                    # Basic UI components
└── layout/                # Layout components
```

### Service Layer Architecture
```
services/
├── orderService.ts        # Order-related API calls
├── storeService.ts        # Store-related API calls
├── userService.ts         # User-related API calls
└── authService.ts         # Authentication API calls
```

## 🔐 Security Architecture

### 1. Authentication Layer
```
Frontend Auth → JWT Token → Backend Validation → Database Query
     ↓
Firebase OAuth → Google Token → Backend Exchange → JWT Token
```

### 2. Route Protection
```
Middleware → Token Validation → Route Access Control → Component Render
```

### 3. Data Security
```
Input Validation → Type Safety → API Validation → Database Security
```

## 🌐 API Integration Architecture

### 1. Service Layer Pattern
```typescript
// Service Interface
interface OrderService {
  getOrders(): Promise<Order[]>
  getOrderById(id: string): Promise<Order>
  createOrder(data: CreateOrderData): Promise<Order>
  updateOrder(id: string, data: UpdateOrderData): Promise<Order>
  deleteOrder(id: string): Promise<void>
}

// Implementation
export const orderService: OrderService = {
  // Implementation details
}
```

### 2. Hook Layer Pattern
```typescript
// Custom Hook
export const useOrders = () => {
  return useQuery({
    queryKey: ['orders'],
    queryFn: orderService.getOrders,
    staleTime: 5 * 60 * 1000, // 5 minutes
  })
}
```

### 3. Component Integration
```typescript
// Component Usage
export default function OrderList() {
  const { data: orders, isLoading, error } = useOrders()
  
  if (isLoading) return <LoadingSpinner />
  if (error) return <ErrorMessage error={error} />
  
  return <OrderTable orders={orders} />
}
```

## 🏗️ Backend Integration

### 1. API Gateway Pattern
```
Next.js Frontend → API Proxy → Spring Boot Backend → Database
```

### 2. Authentication Integration
```
Frontend JWT → Backend Validation → Spring Security → Database Auth
```

### 3. Real-time Features
```
Frontend Polling → REST API → Database Changes → Updated UI
```

## 📊 Performance Architecture

### 1. Caching Strategy
```
Browser Cache → CDN Cache → API Cache → Database Cache
```

### 2. Code Splitting
```
Route-based Splitting → Component-based Splitting → Library Splitting
```

### 3. Optimization Techniques
- **Tree Shaking**: Remove unused code
- **Bundle Splitting**: Separate vendor and app code
- **Image Optimization**: Next.js Image component
- **Font Optimization**: Next.js Font optimization

## 🧪 Testing Architecture

### 1. Testing Pyramid
```
E2E Tests (Cypress/Playwright)
    ↓
Integration Tests (React Testing Library)
    ↓
Unit Tests (Jest)
```

### 2. Test Structure
```
__tests__/
├── components/            # Component tests
├── hooks/                 # Hook tests
├── pages/                 # Page tests
├── services/              # Service tests
└── utils/                 # Utility tests
```

## 🚀 Deployment Architecture

### 1. Development Environment
```
Local Development → Git Push → GitHub Actions → Preview Deploy
```

### 2. Production Environment
```
Git Tag → Build Process → Container Image → Production Deploy
```

### 3. Environment Configuration
```
Development: .env.local
Staging: .env.staging  
Production: .env.production
```

## 🔄 State Management Architecture

### 1. Server State (TanStack Query)
```typescript
// Query Configuration
const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: 5 * 60 * 1000,
      retry: 1,
      refetchOnWindowFocus: false,
    },
  },
})
```

### 2. Client State (React Context)
```typescript
// Auth Context
interface AuthContextType {
  user: User | null
  login: (credentials: LoginCredentials) => Promise<void>
  logout: () => void
  isAuthenticated: boolean
}
```

### 3. Form State (React Hook Form)
```typescript
// Form Management
const {
  register,
  handleSubmit,
  formState: { errors },
} = useForm<FormData>()
```

## 🛡️ Error Handling Architecture

### 1. Error Boundaries
```typescript
// React Error Boundary
class ErrorBoundary extends Component {
  componentDidCatch(error: Error, errorInfo: ErrorInfo) {
    // Log error to monitoring service
  }
}
```

### 2. API Error Handling
```typescript
// Service Error Handling
try {
  const response = await apiCall()
  return response.data
} catch (error) {
  if (error.response?.status === 401) {
    // Handle authentication error
  }
  throw new ServiceError(error.message)
}
```

### 3. User-Friendly Errors
```typescript
// Error Display Component
interface ErrorDisplayProps {
  error: Error
  fallback?: ReactNode
  retry?: () => void
}
```

## 📈 Monitoring & Analytics

### 1. Performance Monitoring
- **Web Vitals**: Core performance metrics
- **Bundle Analysis**: Code splitting effectiveness
- **API Response Times**: Service performance

### 2. Error Tracking
- **Runtime Errors**: Client-side error tracking
- **API Errors**: Service failure monitoring
- **User Journey**: Error impact analysis

### 3. User Analytics
- **Feature Usage**: Component interaction tracking
- **Conversion Funnel**: User flow analysis
- **Performance Impact**: User experience metrics

## 🔮 Scalability Considerations

### 1. Horizontal Scaling
- **Component Reusability**: Modular design
- **Service Separation**: Independent scaling
- **Cache Distribution**: Multi-layer caching

### 2. Vertical Scaling
- **Performance Optimization**: Code efficiency
- **Resource Management**: Memory and CPU usage
- **Bundle Size**: Loading performance

### 3. Future Architecture
- **Micro-frontends**: Independent deployment
- **GraphQL**: Flexible data fetching
- **Real-time**: WebSocket integration

---

Kiến trúc này đảm bảo tính maintainability, scalability và performance cho ứng dụng Fast Route Logistics.