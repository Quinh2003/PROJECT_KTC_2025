# 🌐 API Integration - Fast Route Logistics

## 📋 Tổng quan API Integration

Fast Route Logistics Next.js frontend tích hợp với Spring Boot backend thông qua RESTful APIs, sử dụng các design patterns hiện đại để đảm bảo hiệu suất và maintainability.

## 🔗 Backend Connection

### Base Configuration
```typescript
// API Base URL
const API_BASE_URL = process.env.NEXT_PUBLIC_API_BASE_URL || 'http://localhost:8080'

// Axios instance
const api = axios.create({
  baseURL: `${API_BASE_URL}/api`,
  timeout: 10000,
  headers: {
    'Content-Type': 'application/json'
  }
})
```

### Request Interceptors
```typescript
// Add auth token to requests
api.interceptors.request.use(
  (config) => {
    const token = getToken()
    if (token) {
      config.headers.Authorization = `Bearer ${token}`
    }
    return config
  },
  (error) => Promise.reject(error)
)
```

### Response Interceptors
```typescript
// Handle token refresh and errors
api.interceptors.response.use(
  (response) => response,
  async (error) => {
    if (error.response?.status === 401) {
      const newToken = await autoRefreshToken('/api/auth/refresh')
      if (newToken) {
        return api.request({
          ...error.config,
          headers: {
            ...error.config.headers,
            Authorization: `Bearer ${newToken}`
          }
        })
      }
      // Redirect to login if refresh fails
      window.location.href = '/login'
    }
    return Promise.reject(error)
  }
)
```

## 🔐 Authentication APIs

### 1. Login API
```typescript
// POST /api/auth/login
export async function loginApi(email: string, password: string) {
  const response = await fetch(`${API_BASE_URL}/api/auth/login`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ email, password })
  })
  return response
}

// Usage in component
const handleLogin = async (credentials: LoginCredentials) => {
  try {
    const response = await loginApi(credentials.email, credentials.password)
    const data = await response.json()
    
    if (response.ok) {
      setTokenCookie(data.token)
      setRefreshTokenCookie(data.refreshToken)
      onLogin(data)
    } else {
      setError(data.message || 'Đăng nhập thất bại')
    }
  } catch (error) {
    setError('Lỗi kết nối server')
  }
}
```

### 2. Google OAuth API
```typescript
// POST /api/auth/google-login
export async function googleLoginApi(accessToken: string) {
  const response = await fetch(`${API_BASE_URL}/api/auth/google-login`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ accessToken })
  })
  return response
}

// Integration with Firebase
const handleGoogleLogin = async () => {
  const auth = getAuth(app)
  const provider = new GoogleAuthProvider()
  
  try {
    const result = await signInWithPopup(auth, provider)
    const credential = GoogleAuthProvider.credentialFromResult(result)
    const accessToken = credential?.accessToken
    
    if (accessToken) {
      const response = await googleLoginApi(accessToken)
      const data = await response.json()
      
      if (data.user?.role?.toLowerCase() === 'customer') {
        setTokenCookie(data.token)
        onLogin(data)
      }
    }
  } catch (error) {
    console.error('Google login failed:', error)
  }
}
```

### 3. Registration API
```typescript
// POST /api/auth/register
interface RegisterData {
  email: string
  password: string
  fullName: string
}

export async function registerApi(data: RegisterData) {
  const response = await api.post('/auth/register', data)
  return response.data
}
```

## 📦 Order Management APIs

### 1. Order Service Layer
```typescript
// services/orderService.ts
export interface OrderSummary {
  orderId: number
  storeId: number
  createdAt: string
  deliveryAddress: string
  totalItems: number
  deliveryFee: number
  orderStatus: string
}

export const orderApi = {
  // Get all orders
  getOrders: async (): Promise<Order[]> => {
    const { data } = await api.get<Order[]>('/orders')
    return data
  },

  // Get order by ID
  getOrderById: async (id: string): Promise<Order> => {
    const { data } = await api.get<Order>(`/orders/${id}`)
    return data
  },

  // Create new order
  createOrder: async (orderData: Partial<Order>): Promise<Order> => {
    const { data } = await api.post<Order>('/orders', orderData)
    return data
  },

  // Update order
  updateOrder: async (id: string, orderData: Partial<Order>): Promise<Order> => {
    const { data } = await api.patch<Order>(`/orders/${id}`, orderData)
    return data
  },

  // Delete order
  deleteOrder: async (id: string): Promise<void> => {
    await api.delete(`/orders/${id}`)
  },

  // Get order tracking
  getOrderTracking: async (id: string) => {
    const { data } = await api.get(`/orders/${id}/tracking`)
    return data
  },

  // Get orders by user
  getOrdersByUser: async (userId: number): Promise<OrderSummary[]> => {
    const { data } = await axios.get<OrderSummary[]>(
      `${API_BASE_URL}/api/orders/user/${userId}/summary`
    )
    return data
  }
}
```

### 2. Custom Hooks Integration
```typescript
// hooks/useOrders.ts
export const useOrders = () => {
  return useQuery<Order[]>({
    queryKey: ['orders'],
    queryFn: orderApi.getOrders,
    staleTime: 5 * 60 * 1000, // 5 minutes
    retry: 1,
    onError: (error) => {
      console.error('Failed to fetch orders:', error)
    }
  })
}

export const useCreateOrder = () => {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: orderApi.createOrder,
    onSuccess: (newOrder) => {
      // Update orders cache
      queryClient.invalidateQueries({ queryKey: ['orders'] })
      
      // Add to cache optimistically
      queryClient.setQueryData<Order[]>(['orders'], (old) => 
        old ? [...old, newOrder] : [newOrder]
      )
    },
    onError: (error) => {
      console.error('Failed to create order:', error)
    }
  })
}
```

## 🏪 Store Management APIs

### 1. Store Service
```typescript
// services/storeService.ts
export const storeApi = {
  // Get all stores
  getStores: async (): Promise<Store[]> => {
    const { data } = await api.get<Store[]>('/stores')
    return data
  },

  // Get store by ID
  getStoreById: async (id: string): Promise<Store> => {
    const { data } = await api.get<Store>(`/stores/${id}`)
    return data
  },

  // Create store
  createStore: async (storeData: Partial<Store>): Promise<Store> => {
    const { data } = await api.post<Store>('/stores', storeData)
    return data
  },

  // Update store
  updateStore: async (id: string, storeData: Partial<Store>): Promise<Store> => {
    const { data } = await api.put<Store>(`/stores/${id}`, storeData)
    return data
  },

  // Delete store
  deleteStore: async (id: string): Promise<void> => {
    await api.delete(`/stores/${id}`)
  }
}
```

### 2. Store Hooks
```typescript
// hooks/useStores.ts
export const useStores = () => {
  return useQuery<Store[]>({
    queryKey: ['stores'],
    queryFn: storeApi.getStores,
    staleTime: 10 * 60 * 1000, // 10 minutes (stores change less frequently)
  })
}

export const useStore = (id: string) => {
  return useQuery<Store>({
    queryKey: ['stores', id],
    queryFn: () => storeApi.getStoreById(id),
    enabled: !!id,
  })
}
```

## 👤 User Management APIs

### 1. User Service
```typescript
// server/user.api.ts
export const userApi = {
  // Get current user profile
  getProfile: async (): Promise<User> => {
    const { data } = await api.get<User>('/users/profile')
    return data
  },

  // Update user profile
  updateProfile: async (profileData: Partial<User>): Promise<User> => {
    const { data } = await api.put<User>('/users/profile', profileData)
    return data
  },

  // Change password
  changePassword: async (passwordData: ChangePasswordData): Promise<void> => {
    await api.post('/users/change-password', passwordData)
  }
}

interface ChangePasswordData {
  currentPassword: string
  newPassword: string
  confirmPassword: string
}
```

## 🔄 Data Synchronization

### 1. Real-time Updates
```typescript
// Polling for order status updates
export const useOrderTracking = (orderId: string) => {
  return useQuery({
    queryKey: ['orders', orderId, 'tracking'],
    queryFn: () => orderApi.getOrderTracking(orderId),
    enabled: !!orderId,
    refetchInterval: 30000, // Poll every 30 seconds
    refetchOnWindowFocus: true,
  })
}
```

### 2. Optimistic Updates
```typescript
// Optimistic order status update
export const useUpdateOrderStatus = () => {
  const queryClient = useQueryClient()

  return useMutation({
    mutationFn: async ({ orderId, status }: { orderId: string; status: string }) => {
      return await orderApi.updateOrder(orderId, { status })
    },
    onMutate: async ({ orderId, status }) => {
      // Cancel outgoing refetches
      await queryClient.cancelQueries({ queryKey: ['orders', orderId] })

      // Snapshot the previous value
      const previousOrder = queryClient.getQueryData(['orders', orderId])

      // Optimistically update to the new value
      queryClient.setQueryData(['orders', orderId], (old: any) => ({
        ...old,
        status
      }))

      return { previousOrder }
    },
    onError: (err, variables, context) => {
      // Rollback on error
      queryClient.setQueryData(['orders', variables.orderId], context?.previousOrder)
    },
    onSettled: (data, error, variables) => {
      // Always refetch after error or success
      queryClient.invalidateQueries({ queryKey: ['orders', variables.orderId] })
    }
  })
}
```

## 🔧 Error Handling

### 1. API Error Types
```typescript
interface ApiError {
  code: string
  message: string
  details?: any
}

enum ApiErrorCode {
  VALIDATION_ERROR = 'VALIDATION_ERROR',
  AUTHENTICATION_ERROR = 'AUTHENTICATION_ERROR',
  AUTHORIZATION_ERROR = 'AUTHORIZATION_ERROR',
  NOT_FOUND = 'NOT_FOUND',
  SERVER_ERROR = 'SERVER_ERROR',
  NETWORK_ERROR = 'NETWORK_ERROR'
}
```

### 2. Error Handling Service
```typescript
// utils/errorHandler.ts
export const handleApiError = (error: any): ApiError => {
  if (error.response) {
    // Server responded with error status
    return {
      code: error.response.data?.code || 'SERVER_ERROR',
      message: error.response.data?.message || 'Lỗi server',
      details: error.response.data?.details
    }
  } else if (error.request) {
    // Network error
    return {
      code: 'NETWORK_ERROR',
      message: 'Lỗi kết nối mạng'
    }
  } else {
    // Other error
    return {
      code: 'UNKNOWN_ERROR',
      message: error.message || 'Lỗi không xác định'
    }
  }
}
```

### 3. Error Boundary Integration
```typescript
// components/ApiErrorBoundary.tsx
interface ApiErrorBoundaryProps {
  children: React.ReactNode
  fallback?: (error: ApiError) => React.ReactNode
}

export function ApiErrorBoundary({ children, fallback }: ApiErrorBoundaryProps) {
  return (
    <ErrorBoundary
      FallbackComponent={({ error }) => {
        const apiError = handleApiError(error)
        return fallback ? fallback(apiError) : <DefaultErrorFallback error={apiError} />
      }}
    >
      {children}
    </ErrorBoundary>
  )
}
```

## 📊 API Performance Monitoring

### 1. Request Timing
```typescript
// Add request timing interceptor
api.interceptors.request.use((config) => {
  config.metadata = { startTime: new Date() }
  return config
})

api.interceptors.response.use(
  (response) => {
    const duration = new Date().getTime() - response.config.metadata.startTime.getTime()
    console.log(`API ${response.config.method?.toUpperCase()} ${response.config.url}: ${duration}ms`)
    return response
  },
  (error) => {
    if (error.config?.metadata) {
      const duration = new Date().getTime() - error.config.metadata.startTime.getTime()
      console.error(`API ${error.config.method?.toUpperCase()} ${error.config.url}: ${duration}ms (ERROR)`)
    }
    return Promise.reject(error)
  }
)
```

### 2. Cache Metrics
```typescript
// TanStack Query DevTools for cache inspection
import { ReactQueryDevtools } from '@tanstack/react-query-devtools'

export function QueryProvider({ children }: { children: React.ReactNode }) {
  return (
    <QueryClientProvider client={queryClient}>
      {children}
      <ReactQueryDevtools initialIsOpen={false} />
    </QueryClientProvider>
  )
}
```

## 🧪 API Testing

### 1. Mock Service Worker
```typescript
// __tests__/mocks/handlers.ts
import { rest } from 'msw'

export const handlers = [
  rest.get('/api/orders', (req, res, ctx) => {
    return res(
      ctx.json([
        { id: '1', status: 'pending', total: 100 },
        { id: '2', status: 'delivered', total: 200 }
      ])
    )
  }),

  rest.post('/api/orders', (req, res, ctx) => {
    return res(
      ctx.json({ id: '3', status: 'pending', total: 150 })
    )
  })
]
```

### 2. Integration Tests
```typescript
// __tests__/api/orders.test.ts
describe('Order API Integration', () => {
  test('should fetch orders successfully', async () => {
    const orders = await orderApi.getOrders()
    expect(orders).toHaveLength(2)
    expect(orders[0]).toHaveProperty('id')
    expect(orders[0]).toHaveProperty('status')
  })

  test('should create order successfully', async () => {
    const newOrder = await orderApi.createOrder({
      items: [{ name: 'Test Item', quantity: 1, price: 50 }],
      address: 'Test Address'
    })
    
    expect(newOrder).toHaveProperty('id')
    expect(newOrder.status).toBe('pending')
  })
})
```

## 📈 API Documentation Reference

### Endpoints Mapping

| Frontend Function | HTTP Method | Backend Endpoint | Description |
|------------------|-------------|------------------|-------------|
| `loginApi()` | POST | `/api/auth/login` | User authentication |
| `googleLoginApi()` | POST | `/api/auth/google-login` | Google OAuth login |
| `orderApi.getOrders()` | GET | `/api/orders` | Get all orders |
| `orderApi.createOrder()` | POST | `/api/orders` | Create new order |
| `orderApi.getOrderById()` | GET | `/api/orders/{id}` | Get order details |
| `orderApi.updateOrder()` | PATCH | `/api/orders/{id}` | Update order |
| `orderApi.deleteOrder()` | DELETE | `/api/orders/{id}` | Delete order |
| `orderApi.getOrderTracking()` | GET | `/api/orders/{id}/tracking` | Get tracking info |
| `storeApi.getStores()` | GET | `/api/stores` | Get all stores |
| `userApi.getProfile()` | GET | `/api/users/profile` | Get user profile |

---

Hệ thống API integration này đảm bảo communication hiệu quả và đáng tin cậy giữa frontend và backend.