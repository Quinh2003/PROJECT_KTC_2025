# 🔐 Authentication & Authorization - Fast Route Logistics

## 📋 Tổng quan Authentication

Hệ thống authentication của Fast Route Logistics sử dụng kết hợp nhiều phương thức để đảm bảo tính bảo mật và trải nghiệm người dùng tốt nhất.

## 🎯 Phương thức Authentication

### 1. Email/Password Authentication
```typescript
// Login Flow
interface LoginCredentials {
  email: string
  password: string
}

// API Call
POST /api/auth/login
Body: LoginCredentials
Response: {
  user: User
  token: string
  refreshToken: string
}
```

### 2. Google OAuth Integration
```typescript
// Firebase Google Auth
import { getAuth, signInWithPopup, GoogleAuthProvider } from 'firebase/auth'

const handleGoogleLogin = async () => {
  const auth = getAuth(app)
  const provider = new GoogleAuthProvider()
  
  try {
    const result = await signInWithPopup(auth, provider)
    const credential = GoogleAuthProvider.credentialFromResult(result)
    const accessToken = credential?.accessToken
    
    // Send to backend for verification
    const response = await googleLoginApi(accessToken)
    // Handle response
  } catch (error) {
    // Handle error
  }
}
```

### 3. Two-Factor Authentication (2FA)
```typescript
// TOTP Implementation
interface TwoFactorData {
  userId: number
  totpCode: string
}

// Verification Flow
POST /api/auth/verify-2fa
Body: TwoFactorData
Response: {
  success: boolean
  token?: string
}
```

## 🔑 Token Management

### 1. JWT Token Structure
```typescript
// Token Payload
interface JWTPayload {
  sub: string        // User ID
  email: string      // User email
  role: string       // User role
  iat: number        // Issued at
  exp: number        // Expires at
}
```

### 2. Token Storage Strategy
```typescript
// Cookie Storage (Secure)
export function setTokenCookie(token: string) {
  document.cookie = `access_token=${token}; path=/; max-age=86400; SameSite=Lax; Secure`
}

// LocalStorage (Backup)
localStorage.setItem('token', token)
localStorage.setItem('user', JSON.stringify(user))
```

### 3. Token Refresh Mechanism
```typescript
// Auto Refresh Implementation
export async function autoRefreshToken(apiRefreshUrl: string): Promise<string | null> {
  const refreshToken = getRefreshTokenCookie()
  if (!refreshToken) return null
  
  try {
    const response = await fetch(apiRefreshUrl, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ refreshToken })
    })
    
    if (!response.ok) return null
    
    const data = await response.json()
    if (data.token) {
      setTokenCookie(data.token)
      return data.token
    }
    return null
  } catch {
    return null
  }
}
```

## 🛡️ Route Protection

### 1. Middleware Implementation
```typescript
// middleware.ts
import { NextResponse } from 'next/server'
import type { NextRequest } from 'next/server'
import { decodeJWT } from './lib/auth'

const protectedRoutes = [
  '/account',
  '/(dashboard)',
]

export function middleware(request: NextRequest) {
  const { pathname } = request.nextUrl
  
  // Check if route needs protection
  if (protectedRoutes.some(route => pathname.startsWith(route))) {
    const token = request.cookies.get('access_token')?.value
    
    if (!token) {
      return NextResponse.redirect(new URL('/login', request.url))
    }
    
    const payload = decodeJWT(token)
    if (!payload || !payload.exp || payload.exp < Math.floor(Date.now() / 1000)) {
      return NextResponse.redirect(new URL('/login', request.url))
    }
  }
  
  return NextResponse.next()
}

export const config = {
  matcher: ['/account/:path*', '/(dashboard)/:path*'],
}
```

### 2. Component-Level Protection
```typescript
// HOC for Route Protection
export function withAuth<P extends object>(Component: React.ComponentType<P>) {
  return function ProtectedComponent(props: P) {
    const router = useRouter()
    const [isAuthorized, setIsAuthorized] = useState(false)
    
    useEffect(() => {
      const token = getToken()
      if (!token || isTokenExpired(token)) {
        router.push('/login')
        return
      }
      setIsAuthorized(true)
    }, [router])
    
    if (!isAuthorized) {
      return <LoadingSpinner />
    }
    
    return <Component {...props} />
  }
}
```

### 3. Hook-Based Protection
```typescript
// useAuth Hook
export function useAuth() {
  const router = useRouter()
  const [user, setUser] = useState<User | null>(null)
  const [loading, setLoading] = useState(true)
  
  useEffect(() => {
    const token = getToken()
    if (!token || isTokenExpired(token)) {
      router.push('/login')
      return
    }
    
    const userData = JSON.parse(localStorage.getItem('user') || 'null')
    setUser(userData)
    setLoading(false)
  }, [router])
  
  const logout = useCallback(() => {
    removeTokenCookie()
    removeRefreshTokenCookie()
    localStorage.clear()
    router.push('/login')
  }, [router])
  
  return { user, loading, logout, isAuthenticated: !!user }
}
```

## 🔒 Authorization & Role-Based Access

### 1. User Roles
```typescript
enum UserRole {
  CUSTOMER = 'CUSTOMER',
  ADMIN = 'ADMIN',
  DRIVER = 'DRIVER',
  DISPATCHER = 'DISPATCHER',
  FLEET_MANAGER = 'FLEET_MANAGER',
  OPERATIONS = 'OPERATIONS'
}

interface User {
  id: number
  email: string
  fullName: string
  role: UserRole
}
```

### 2. Role-Based Routing
```typescript
// Role-specific redirects
const handleLogin = (response: AuthResponse) => {
  const userRole = response.user.role?.toLowerCase()
  
  switch (userRole) {
    case 'customer':
      router.push('/account')
      break
    case 'admin':
      router.push('/admin/dashboard')
      break
    case 'driver':
      router.push('/driver/dashboard')
      break
    default:
      alert('Unknown role. Please contact support.')
      localStorage.clear()
  }
}
```

### 3. Permission Checking
```typescript
// Permission utilities
export function hasPermission(user: User, permission: string): boolean {
  const rolePermissions = {
    CUSTOMER: ['view_orders', 'create_orders', 'view_profile'],
    ADMIN: ['*'], // All permissions
    DRIVER: ['view_deliveries', 'update_delivery_status'],
    DISPATCHER: ['view_orders', 'assign_drivers', 'view_routes'],
  }
  
  const userPermissions = rolePermissions[user.role] || []
  return userPermissions.includes('*') || userPermissions.includes(permission)
}

// Usage in components
export function AdminOnlyButton({ children }: { children: React.ReactNode }) {
  const { user } = useAuth()
  
  if (!user || !hasPermission(user, 'admin_access')) {
    return null
  }
  
  return <button>{children}</button>
}
```

## 🔄 Authentication Flow Diagrams

### 1. Email/Password Login Flow
```
User Input (Email/Password)
          ↓
Frontend Validation
          ↓
API Call to /api/auth/login
          ↓
Backend Validation
          ↓
JWT Token Generation
          ↓
Token Storage (Cookie + LocalStorage)
          ↓
Redirect to Dashboard
```

### 2. Google OAuth Flow
```
User Clicks Google Login
          ↓
Firebase Auth Popup
          ↓
Google OAuth Consent
          ↓
Access Token Retrieved
          ↓
Send Token to Backend
          ↓
Backend Validates with Google
          ↓
User Created/Updated in Database
          ↓
JWT Token Generated
          ↓
Frontend Receives Token
          ↓
Redirect to Dashboard
```

### 3. 2FA Verification Flow
```
Initial Login Success
          ↓
Check if 2FA Enabled
          ↓
Show 2FA Input Form
          ↓
User Enters TOTP Code
          ↓
Backend Validates TOTP
          ↓
Final Token Generation
          ↓
Complete Authentication
```

## 🧪 Testing Authentication

### 1. Unit Tests
```typescript
// auth.test.ts
describe('Authentication utilities', () => {
  test('should decode JWT token correctly', () => {
    const token = 'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9...'
    const payload = decodeJWT(token)
    
    expect(payload).toHaveProperty('sub')
    expect(payload).toHaveProperty('exp')
  })
  
  test('should detect expired tokens', () => {
    const expiredToken = createExpiredToken()
    expect(isTokenExpired(expiredToken)).toBe(true)
  })
})
```

### 2. Integration Tests
```typescript
// login.test.tsx
describe('Login Form', () => {
  test('should redirect after successful login', async () => {
    render(<LoginForm onLogin={mockOnLogin} />)
    
    fireEvent.change(screen.getByLabelText(/email/i), {
      target: { value: 'test@example.com' }
    })
    fireEvent.change(screen.getByLabelText(/password/i), {
      target: { value: 'password123' }
    })
    
    fireEvent.click(screen.getByRole('button', { name: /login/i }))
    
    await waitFor(() => {
      expect(mockOnLogin).toHaveBeenCalled()
    })
  })
})
```

## 🔧 Security Best Practices

### 1. Token Security
- **Short-lived access tokens** (24 hours)
- **Long-lived refresh tokens** (7 days)
- **Secure cookie storage** with HttpOnly flag
- **CSRF protection** with SameSite cookies

### 2. Input Validation
```typescript
// Email validation
const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/

// Password strength validation
const passwordValidation = {
  minLength: 8,
  requireUppercase: true,
  requireLowercase: true,
  requireNumbers: true,
  requireSpecialChars: true
}
```

### 3. Rate Limiting
```typescript
// Login attempt limiting
const MAX_LOGIN_ATTEMPTS = 5
const LOCKOUT_DURATION = 15 * 60 * 1000 // 15 minutes

interface LoginAttempt {
  email: string
  attempts: number
  lastAttempt: Date
  lockedUntil?: Date
}
```

## 🚨 Error Handling

### 1. Authentication Errors
```typescript
enum AuthError {
  INVALID_CREDENTIALS = 'INVALID_CREDENTIALS',
  ACCOUNT_LOCKED = 'ACCOUNT_LOCKED',
  TOKEN_EXPIRED = 'TOKEN_EXPIRED',
  INSUFFICIENT_PERMISSIONS = 'INSUFFICIENT_PERMISSIONS',
  TWO_FACTOR_REQUIRED = 'TWO_FACTOR_REQUIRED'
}

const authErrorMessages = {
  [AuthError.INVALID_CREDENTIALS]: 'Email hoặc mật khẩu không đúng',
  [AuthError.ACCOUNT_LOCKED]: 'Tài khoản đã bị khóa do quá nhiều lần đăng nhập sai',
  [AuthError.TOKEN_EXPIRED]: 'Phiên đăng nhập đã hết hạn',
  [AuthError.INSUFFICIENT_PERMISSIONS]: 'Bạn không có quyền truy cập tính năng này',
  [AuthError.TWO_FACTOR_REQUIRED]: 'Vui lòng nhập mã xác thực 2FA'
}
```

### 2. Error Recovery
```typescript
// Automatic token refresh on 401 errors
axios.interceptors.response.use(
  (response) => response,
  async (error) => {
    if (error.response?.status === 401) {
      const newToken = await autoRefreshToken('/api/auth/refresh')
      if (newToken) {
        // Retry original request with new token
        return axios.request({
          ...error.config,
          headers: {
            ...error.config.headers,
            Authorization: `Bearer ${newToken}`
          }
        })
      } else {
        // Redirect to login
        window.location.href = '/login'
      }
    }
    return Promise.reject(error)
  }
)
```

## 📊 Authentication Monitoring

### 1. Login Analytics
- **Success/failure rates**
- **Login methods usage** (Email vs Google)
- **Geographic login patterns**
- **Device and browser statistics**

### 2. Security Monitoring
- **Failed login attempts**
- **Suspicious activity detection**
- **Token usage patterns**
- **Session duration analytics**

---

Hệ thống authentication này đảm bảo tính bảo mật cao trong khi vẫn mang lại trải nghiệm người dùng mượt mà.