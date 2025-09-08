# 🧩 Components & Pages - Fast Route Logistics

## 📋 Tổng quan Components

Ứng dụng được xây dựng theo component-based architecture với sự phân chia rõ ràng giữa UI components, form components, và page components.

## 🏗️ Component Architecture

### 1. Component Hierarchy
```
App (Root Layout)
├── Providers (Context Providers)
├── AuthLayout (Authentication Pages)
│   ├── LoginForm
│   ├── RegisterForm
│   ├── ForgotPasswordForm
│   └── TwoFactorForm
├── AccountLayout (Customer Area)
│   ├── Navigation
│   ├── OrderPages
│   ├── ProfilePages
│   └── Dashboard
└── PublicLayout (Landing Pages)
    └── HomePage
```

## 📝 Form Components

### 1. LoginForm Component
```typescript
// components/forms/LoginForm.tsx
interface LoginFormProps {
  onLogin: (response: AuthResponse) => void
}

export default function LoginForm({ onLogin }: LoginFormProps) {
  const [email, setEmail] = useState('')
  const [password, setPassword] = useState('')
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState('')
  const [showPassword, setShowPassword] = useState(false)

  // Email/Password login handler
  const handleEmailLogin = async (e: React.FormEvent) => {
    e.preventDefault()
    setLoading(true)
    setError('')

    try {
      const res = await loginApi(email, password)
      const data = await res.json()

      if (res.ok) {
        if (data.user?.role?.toLowerCase() === 'customer') {
          setTokenCookie(data.token)
          setRefreshTokenCookie(data.refreshToken)
          onLogin(data)
        } else {
          setError('Ứng dụng này chỉ dành cho khách hàng')
        }
      } else {
        setError(data.message || 'Đăng nhập thất bại')
      }
    } catch (error) {
      setError('Lỗi kết nối server')
    } finally {
      setLoading(false)
    }
  }

  // Google login handler
  const handleGoogleLogin = async () => {
    const auth = getAuth(app)
    const provider = new GoogleAuthProvider()
    
    try {
      const result = await signInWithPopup(auth, provider)
      const credential = GoogleAuthProvider.credentialFromResult(result)
      const accessToken = credential?.accessToken

      if (accessToken) {
        const res = await googleLoginApi(accessToken)
        const data = await res.json()

        if (data.user?.role?.toLowerCase() === 'customer') {
          if (!data.user?.totpEnabled) {
            setPending2FA(true)
            setPendingUser(data.user)
            return
          }
          setTokenCookie(data.token)
          setRefreshTokenCookie(data.refreshToken)
          onLogin(data)
        }
      }
    } catch (error) {
      setError('Đăng nhập Google thất bại')
    }
  }

  return (
    <div className="bg-white/5 backdrop-blur-lg rounded-3xl p-8 border border-white/10 shadow-2xl">
      <div className="text-center mb-8">
        <h1 className="text-3xl font-bold text-white mb-2">Chào mừng trở lại! 👋</h1>
        <p className="text-white/60">Đăng nhập để tiếp tục sử dụng dịch vụ</p>
      </div>

      <form onSubmit={handleEmailLogin} className="space-y-6">
        {/* Email Input */}
        <div>
          <label className="block text-white/80 text-sm font-medium mb-2">
            Email
          </label>
          <input
            type="email"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            className="w-full px-4 py-3 bg-white/10 border border-white/20 rounded-xl text-white placeholder-white/40 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent transition-all"
            placeholder="your@email.com"
            required
          />
        </div>

        {/* Password Input */}
        <div>
          <label className="block text-white/80 text-sm font-medium mb-2">
            Mật khẩu
          </label>
          <div className="relative">
            <input
              type={showPassword ? "text" : "password"}
              value={password}
              onChange={(e) => setPassword(e.target.value)}
              className="w-full px-4 py-3 pr-12 bg-white/10 border border-white/20 rounded-xl text-white placeholder-white/40 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-transparent transition-all"
              placeholder="••••••••"
              required
            />
            <button
              type="button"
              onClick={() => setShowPassword(!showPassword)}
              className="absolute right-3 top-1/2 transform -translate-y-1/2 text-white/60 hover:text-white transition-colors"
            >
              {showPassword ? <FaLockOpen size={20} /> : <FaLock size={20} />}
            </button>
          </div>
        </div>

        {/* Error Message */}
        {error && (
          <div className="bg-red-500/20 border border-red-500/30 rounded-xl p-3">
            <p className="text-red-200 text-sm text-center">{error}</p>
          </div>
        )}

        {/* Submit Button */}
        <button
          type="submit"
          disabled={loading}
          className="w-full bg-gradient-to-r from-blue-600 to-purple-600 hover:from-blue-700 hover:to-purple-700 text-white font-semibold py-3 rounded-xl transition-all transform hover:scale-[1.02] disabled:opacity-50 disabled:transform-none"
        >
          {loading ? (
            <div className="flex items-center justify-center gap-2">
              <div className="w-5 h-5 border-2 border-white border-t-transparent rounded-full animate-spin"></div>
              Đang đăng nhập...
            </div>
          ) : (
            'Đăng nhập'
          )}
        </button>

        {/* Google Login */}
        <button
          type="button"
          onClick={handleGoogleLogin}
          className="w-full bg-white/10 hover:bg-white/20 text-white font-semibold py-3 rounded-xl transition-all border border-white/20 flex items-center justify-center gap-3"
        >
          <FcGoogle size={24} />
          Đăng nhập với Google
        </button>
      </form>

      {/* Footer Links */}
      <div className="mt-6 text-center">
        <button
          onClick={() => setShowForgotPassword(true)}
          className="text-blue-300 hover:text-blue-200 text-sm transition-colors"
        >
          Quên mật khẩu?
        </button>
      </div>
    </div>
  )
}
```

### 2. RegisterForm Component
```typescript
// components/forms/RegisterForm.tsx
interface RegisterFormProps {
  onRegister: (response: AuthResponse) => void
}

export default function RegisterForm({ onRegister }: RegisterFormProps) {
  const [formData, setFormData] = useState({
    fullName: '',
    email: '',
    password: '',
    confirmPassword: ''
  })
  const [loading, setLoading] = useState(false)
  const [errors, setErrors] = useState<Record<string, string>>({})

  const validateForm = (): boolean => {
    const newErrors: Record<string, string> = {}

    // Full name validation
    if (!formData.fullName.trim()) {
      newErrors.fullName = 'Họ tên không được để trống'
    }

    // Email validation
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/
    if (!emailRegex.test(formData.email)) {
      newErrors.email = 'Email không hợp lệ'
    }

    // Password validation
    if (formData.password.length < 8) {
      newErrors.password = 'Mật khẩu phải có ít nhất 8 ký tự'
    }

    // Confirm password validation
    if (formData.password !== formData.confirmPassword) {
      newErrors.confirmPassword = 'Mật khẩu xác nhận không khớp'
    }

    setErrors(newErrors)
    return Object.keys(newErrors).length === 0
  }

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    
    if (!validateForm()) return

    setLoading(true)
    try {
      const response = await registerApi({
        fullName: formData.fullName,
        email: formData.email,
        password: formData.password
      })

      onRegister(response)
    } catch (error) {
      setErrors({ submit: 'Đăng ký thất bại. Vui lòng thử lại.' })
    } finally {
      setLoading(false)
    }
  }

  return (
    <div className="bg-white/5 backdrop-blur-lg rounded-3xl p-8 border border-white/10 shadow-2xl">
      <div className="text-center mb-8">
        <h1 className="text-3xl font-bold text-white mb-2">Tạo tài khoản mới 🚀</h1>
        <p className="text-white/60">Tham gia Fast Route để trải nghiệm dịch vụ tốt nhất</p>
      </div>

      <form onSubmit={handleSubmit} className="space-y-6">
        {/* Form fields with validation */}
        {/* ... form implementation */}
      </form>
    </div>
  )
}
```

### 3. TwoFactorForm Component
```typescript
// components/forms/TwoFactorForm.tsx
interface TwoFactorFormProps {
  user: User
  onSuccess: (token: string) => void
  onCancel: () => void
}

export default function TwoFactorForm({ user, onSuccess, onCancel }: TwoFactorFormProps) {
  const [totpCode, setTotpCode] = useState('')
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState('')

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault()
    setLoading(true)
    setError('')

    try {
      const response = await fetch('/api/auth/verify-2fa', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          userId: user.id,
          totpCode
        })
      })

      const data = await response.json()

      if (response.ok && data.success) {
        onSuccess(data.token)
      } else {
        setError(data.message || 'Mã xác thực không đúng')
      }
    } catch (error) {
      setError('Lỗi kết nối server')
    } finally {
      setLoading(false)
    }
  }

  return (
    <div className="bg-white/5 backdrop-blur-lg rounded-3xl p-8 border border-white/10 shadow-2xl">
      <div className="text-center mb-8">
        <div className="w-16 h-16 bg-blue-500/20 rounded-full flex items-center justify-center mx-auto mb-4">
          <FaShieldAlt className="text-blue-400 text-2xl" />
        </div>
        <h1 className="text-2xl font-bold text-white mb-2">Xác thực 2FA</h1>
        <p className="text-white/60">Nhập mã 6 số từ ứng dụng authenticator</p>
      </div>

      <form onSubmit={handleSubmit} className="space-y-6">
        <div>
          <input
            type="text"
            value={totpCode}
            onChange={(e) => setTotpCode(e.target.value.replace(/\D/g, '').slice(0, 6))}
            className="w-full px-4 py-3 bg-white/10 border border-white/20 rounded-xl text-white text-center text-2xl tracking-widest font-mono placeholder-white/40 focus:outline-none focus:ring-2 focus:ring-blue-500"
            placeholder="000000"
            maxLength={6}
            required
          />
        </div>

        {error && (
          <div className="bg-red-500/20 border border-red-500/30 rounded-xl p-3">
            <p className="text-red-200 text-sm text-center">{error}</p>
          </div>
        )}

        <div className="space-y-3">
          <button
            type="submit"
            disabled={loading || totpCode.length !== 6}
            className="w-full bg-gradient-to-r from-blue-600 to-purple-600 hover:from-blue-700 hover:to-purple-700 text-white font-semibold py-3 rounded-xl transition-all disabled:opacity-50"
          >
            {loading ? 'Đang xác thực...' : 'Xác thực'}
          </button>

          <button
            type="button"
            onClick={onCancel}
            className="w-full bg-white/10 hover:bg-white/20 text-white font-semibold py-3 rounded-xl transition-all"
          >
            Hủy
          </button>
        </div>
      </form>
    </div>
  )
}
```

## 📄 Page Components

### 1. Customer Dashboard
```typescript
// app/account/page.tsx
export default function CustomerAccount() {
  const { user } = useAuth()
  const { data: orders } = useOrders()
  
  return (
    <div style={{ display: "flex", flexDirection: "column", gap: "24px" }}>
      {/* Welcome Section */}
      <Card>
        <Title level={2} style={{ marginBottom: 16 }}>
          Chào mừng đến với Fast Route! <CarOutlined />
        </Title>
        <Text style={{ fontSize: 16 }}>
          Dịch vụ giao hàng thông minh với công nghệ tối ưu hóa tuyến đường.
        </Text>
      </Card>

      {/* Quick Actions */}
      <Row gutter={[24, 24]}>
        <Col xs={24} md={8}>
          <Link href="/account/orders/new">
            <Card hoverable>
              <div style={{ textAlign: "center", marginBottom: 16 }}>
                <BoxPlotOutlined style={{ fontSize: 32, color: "#1890ff" }} />
              </div>
              <Title level={4} style={{ textAlign: "center" }}>
                Tạo đơn hàng
              </Title>
              <Text type="secondary" style={{ textAlign: "center", display: "block" }}>
                Tạo đơn hàng giao hàng mới
              </Text>
            </Card>
          </Link>
        </Col>

        <Col xs={24} md={8}>
          <Link href="/account/orders">
            <Card hoverable>
              <div style={{ textAlign: "center", marginBottom: 16 }}>
                <EnvironmentOutlined style={{ fontSize: 32, color: "#52c41a" }} />
              </div>
              <Title level={4} style={{ textAlign: "center" }}>
                Theo dõi đơn hàng
              </Title>
              <Text type="secondary" style={{ textAlign: "center", display: "block" }}>
                Xem trạng thái và vị trí đơn hàng
              </Text>
            </Card>
          </Link>
        </Col>

        <Col xs={24} md={8}>
          <Link href="/account/estimate">
            <Card hoverable>
              <div style={{ textAlign: "center", marginBottom: 16 }}>
                <DollarOutlined style={{ fontSize: 32, color: "#faad14" }} />
              </div>
              <Title level={4} style={{ textAlign: "center" }}>
                Tính phí vận chuyển
              </Title>
              <Text type="secondary" style={{ textAlign: "center", display: "block" }}>
                Ước tính chi phí giao hàng
              </Text>
            </Card>
          </Link>
        </Col>
      </Row>

      {/* Statistics */}
      <Card>
        <Title level={4} style={{ marginBottom: 16 }}>
          Thống kê <BarChartOutlined />
        </Title>
        <Row gutter={16}>
          <Col span={8}>
            <Statistic
              title="Tổng đơn hàng"
              value={orders?.length || 0}
              prefix={<BoxPlotOutlined />}
            />
          </Col>
          <Col span={8}>
            <Statistic
              title="Đơn thành công"
              value={orders?.filter(order => order.status === 'delivered').length || 0}
              prefix={<CheckCircleOutlined />}
            />
          </Col>
          <Col span={8}>
            <Statistic
              title="Tiết kiệm"
              value={1128}
              prefix="₫"
              suffix="k"
            />
          </Col>
        </Row>
      </Card>
    </div>
  )
}
```

### 2. Order List Page
```typescript
// app/account/orders/page.tsx
export default function OrdersPage() {
  const { data: orders, isLoading, error } = useOrders()
  const [filteredOrders, setFilteredOrders] = useState<Order[]>([])
  const [searchTerm, setSearchTerm] = useState('')
  const [statusFilter, setStatusFilter] = useState('all')

  useEffect(() => {
    if (!orders) return

    let filtered = orders
    
    // Search filter
    if (searchTerm) {
      filtered = filtered.filter(order =>
        order.id.toLowerCase().includes(searchTerm.toLowerCase()) ||
        order.address.toLowerCase().includes(searchTerm.toLowerCase())
      )
    }

    // Status filter
    if (statusFilter !== 'all') {
      filtered = filtered.filter(order => order.status === statusFilter)
    }

    setFilteredOrders(filtered)
  }, [orders, searchTerm, statusFilter])

  if (isLoading) return <Spin size="large" />
  if (error) return <Alert message="Lỗi tải dữ liệu" type="error" />

  return (
    <div>
      <div style={{ marginBottom: 24 }}>
        <Title level={2}>Danh sách đơn hàng</Title>
        
        {/* Search and Filter */}
        <Row gutter={16} style={{ marginTop: 16 }}>
          <Col span={12}>
            <Input
              placeholder="Tìm kiếm theo mã đơn hoặc địa chỉ..."
              prefix={<SearchOutlined />}
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
            />
          </Col>
          <Col span={6}>
            <Select
              style={{ width: '100%' }}
              value={statusFilter}
              onChange={setStatusFilter}
            >
              <Option value="all">Tất cả trạng thái</Option>
              <Option value="pending">Chờ xử lý</Option>
              <Option value="confirmed">Đã xác nhận</Option>
              <Option value="shipping">Đang giao</Option>
              <Option value="delivered">Đã giao</Option>
              <Option value="cancelled">Đã hủy</Option>
            </Select>
          </Col>
          <Col span={6}>
            <Link href="/account/orders/new">
              <Button type="primary" icon={<PlusOutlined />} block>
                Tạo đơn mới
              </Button>
            </Link>
          </Col>
        </Row>
      </div>

      {/* Orders Table */}
      <Card>
        <Table
          dataSource={filteredOrders}
          rowKey="id"
          pagination={{
            pageSize: 10,
            showSizeChanger: true,
            showQuickJumper: true,
          }}
          columns={[
            {
              title: 'Mã đơn',
              dataIndex: 'id',
              key: 'id',
              render: (id) => (
                <Link href={`/account/orders/${id}`}>
                  <Button type="link" style={{ padding: 0 }}>
                    #{id}
                  </Button>
                </Link>
              )
            },
            {
              title: 'Địa chỉ giao',
              dataIndex: 'address',
              key: 'address',
              ellipsis: true,
            },
            {
              title: 'Trạng thái',
              dataIndex: 'status',
              key: 'status',
              render: (status) => (
                <Tag color={getStatusColor(status)}>
                  {getStatusText(status)}
                </Tag>
              )
            },
            {
              title: 'Tổng tiền',
              dataIndex: 'total',
              key: 'total',
              render: (total) => `${total.toLocaleString()}đ`
            },
            {
              title: 'Ngày tạo',
              dataIndex: 'createdAt',
              key: 'createdAt',
              render: (date) => dayjs(date).format('DD/MM/YYYY HH:mm')
            },
            {
              title: 'Thao tác',
              key: 'actions',
              render: (_, record) => (
                <Space>
                  <Link href={`/account/orders/${record.id}`}>
                    <Button size="small" icon={<EyeOutlined />}>
                      Xem
                    </Button>
                  </Link>
                  <Link href={`/account/orders/${record.id}/tracking`}>
                    <Button size="small" icon={<EnvironmentOutlined />}>
                      Theo dõi
                    </Button>
                  </Link>
                </Space>
              )
            }
          ]}
        />
      </Card>
    </div>
  )
}
```

### 3. Order Detail Page
```typescript
// app/account/orders/[id]/page.tsx
export default function OrderDetailPage({ params }: { params: { id: string } }) {
  const { data: order, isLoading, error } = useOrder(params.id)
  const { data: tracking } = useOrderTracking(params.id)

  if (isLoading) return <Spin size="large" />
  if (error) return <Alert message="Đơn hàng không tồn tại" type="error" />
  if (!order) return <Alert message="Không tìm thấy đơn hàng" type="warning" />

  return (
    <div>
      <div style={{ marginBottom: 24 }}>
        <Button
          icon={<ArrowLeftOutlined />}
          onClick={() => window.history.back()}
          style={{ marginBottom: 16 }}
        >
          Quay lại
        </Button>
        
        <Title level={2}>Chi tiết đơn hàng #{order.id}</Title>
      </div>

      <Row gutter={24}>
        {/* Order Information */}
        <Col span={16}>
          <Card title="Thông tin đơn hàng" style={{ marginBottom: 24 }}>
            <Descriptions column={2}>
              <Descriptions.Item label="Mã đơn hàng">#{order.id}</Descriptions.Item>
              <Descriptions.Item label="Trạng thái">
                <Tag color={getStatusColor(order.status)}>
                  {getStatusText(order.status)}
                </Tag>
              </Descriptions.Item>
              <Descriptions.Item label="Ngày tạo">
                {dayjs(order.createdAt).format('DD/MM/YYYY HH:mm')}
              </Descriptions.Item>
              <Descriptions.Item label="Địa chỉ giao">
                {order.address}
              </Descriptions.Item>
              <Descriptions.Item label="Số lượng sản phẩm">
                {order.itemCount} sản phẩm
              </Descriptions.Item>
              <Descriptions.Item label="Phí vận chuyển">
                {order.shippingFee.toLocaleString()}đ
              </Descriptions.Item>
              <Descriptions.Item label="Tổng tiền">
                <Text strong style={{ fontSize: 16, color: '#1890ff' }}>
                  {order.total.toLocaleString()}đ
                </Text>
              </Descriptions.Item>
            </Descriptions>
          </Card>

          {/* Order Items */}
          <Card title="Danh sách sản phẩm">
            <Table
              dataSource={order.items}
              rowKey="id"
              pagination={false}
              columns={[
                {
                  title: 'Tên sản phẩm',
                  dataIndex: 'name',
                  key: 'name',
                },
                {
                  title: 'Số lượng',
                  dataIndex: 'quantity',
                  key: 'quantity',
                  align: 'center',
                },
                {
                  title: 'Đơn giá',
                  dataIndex: 'price',
                  key: 'price',
                  render: (price) => `${price.toLocaleString()}đ`
                },
                {
                  title: 'Thành tiền',
                  key: 'total',
                  render: (_, record) => 
                    `${(record.quantity * record.price).toLocaleString()}đ`
                }
              ]}
            />
          </Card>
        </Col>

        {/* Tracking Information */}
        <Col span={8}>
          <Card title="Theo dõi đơn hàng">
            {tracking && tracking.length > 0 ? (
              <Timeline>
                {tracking.map((item, index) => (
                  <Timeline.Item
                    key={index}
                    color={index === 0 ? 'green' : 'blue'}
                    dot={index === 0 ? <CheckCircleOutlined /> : undefined}
                  >
                    <div>
                      <Text strong>{item.status}</Text>
                      <br />
                      <Text type="secondary">{item.location}</Text>
                      <br />
                      <Text type="secondary" style={{ fontSize: 12 }}>
                        {dayjs(item.timestamp).format('DD/MM/YYYY HH:mm')}
                      </Text>
                      <br />
                      <Text>{item.description}</Text>
                    </div>
                  </Timeline.Item>
                ))}
              </Timeline>
            ) : (
              <Empty description="Chưa có thông tin theo dõi" />
            )}
          </Card>
        </Col>
      </Row>
    </div>
  )
}
```

## 🎨 UI Utility Components

### 1. Loading Spinner
```typescript
// components/ui/LoadingSpinner.tsx
interface LoadingSpinnerProps {
  size?: 'small' | 'medium' | 'large'
  text?: string
}

export function LoadingSpinner({ size = 'medium', text }: LoadingSpinnerProps) {
  const sizeClasses = {
    small: 'w-4 h-4',
    medium: 'w-8 h-8',
    large: 'w-12 h-12'
  }

  return (
    <div className="flex items-center justify-center gap-3">
      <div className={`${sizeClasses[size]} border-2 border-blue-600 border-t-transparent rounded-full animate-spin`}></div>
      {text && <span className="text-gray-600">{text}</span>}
    </div>
  )
}
```

### 2. Error Display
```typescript
// components/ui/ErrorDisplay.tsx
interface ErrorDisplayProps {
  error: Error | string
  retry?: () => void
  className?: string
}

export function ErrorDisplay({ error, retry, className }: ErrorDisplayProps) {
  const message = typeof error === 'string' ? error : error.message

  return (
    <div className={`bg-red-50 border border-red-200 rounded-lg p-4 ${className}`}>
      <div className="flex items-center gap-3">
        <ExclamationTriangleIcon className="w-5 h-5 text-red-500" />
        <div className="flex-1">
          <h3 className="text-red-800 font-medium">Có lỗi xảy ra</h3>
          <p className="text-red-600 text-sm mt-1">{message}</p>
        </div>
        {retry && (
          <button
            onClick={retry}
            className="text-red-600 hover:text-red-800 font-medium text-sm"
          >
            Thử lại
          </button>
        )}
      </div>
    </div>
  )
}
```

## 🔄 Component Patterns

### 1. Higher-Order Components (HOCs)
```typescript
// hocs/withAuth.tsx
export function withAuth<P extends object>(Component: React.ComponentType<P>) {
  return function AuthenticatedComponent(props: P) {
    const { user, loading } = useAuth()
    
    if (loading) return <LoadingSpinner />
    if (!user) return <Navigate to="/login" replace />
    
    return <Component {...props} />
  }
}
```

### 2. Render Props Pattern
```typescript
// components/DataFetcher.tsx
interface DataFetcherProps<T> {
  queryKey: string[]
  queryFn: () => Promise<T>
  children: (data: T | undefined, loading: boolean, error: Error | null) => React.ReactNode
}

export function DataFetcher<T>({ queryKey, queryFn, children }: DataFetcherProps<T>) {
  const { data, isLoading, error } = useQuery({
    queryKey,
    queryFn
  })

  return <>{children(data, isLoading, error)}</>
}

// Usage
<DataFetcher queryKey={['orders']} queryFn={orderApi.getOrders}>
  {(orders, loading, error) => {
    if (loading) return <LoadingSpinner />
    if (error) return <ErrorDisplay error={error} />
    return <OrderList orders={orders} />
  }}
</DataFetcher>
```

### 3. Compound Components
```typescript
// components/Modal/Modal.tsx
interface ModalContextType {
  isOpen: boolean
  onClose: () => void
}

const ModalContext = createContext<ModalContextType | null>(null)

export function Modal({ children, isOpen, onClose }: ModalProps) {
  return (
    <ModalContext.Provider value={{ isOpen, onClose }}>
      {isOpen && (
        <div className="fixed inset-0 bg-black/50 flex items-center justify-center">
          <div className="bg-white rounded-lg max-w-md w-full mx-4">
            {children}
          </div>
        </div>
      )}
    </ModalContext.Provider>
  )
}

Modal.Header = function ModalHeader({ children }: { children: React.ReactNode }) {
  const { onClose } = useContext(ModalContext)!
  return (
    <div className="flex items-center justify-between p-4 border-b">
      {children}
      <button onClick={onClose} className="text-gray-400 hover:text-gray-600">
        <XMarkIcon className="w-5 h-5" />
      </button>
    </div>
  )
}

Modal.Body = function ModalBody({ children }: { children: React.ReactNode }) {
  return <div className="p-4">{children}</div>
}

Modal.Footer = function ModalFooter({ children }: { children: React.ReactNode }) {
  return <div className="flex justify-end gap-2 p-4 border-t">{children}</div>
}
```

---

Hệ thống components này đảm bảo tính tái sử dụng, maintainability và consistency trong toàn bộ ứng dụng.