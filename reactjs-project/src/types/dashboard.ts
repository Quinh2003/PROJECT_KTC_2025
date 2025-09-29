// Vehicle cho domain fleet (phục vụ UI FleetDashboard)
export interface FleetVehicle {
  id: number;
  licensePlate: string;
  type: string;
  brand: string;
  model: string;
  year: number;
  status: "Hoạt động" | "Bảo trì" | "Cần bảo trì";
  lastMaintenance: string;
  nextMaintenance: string;
  driver: string;
  mileage: number;
  capacityWeightKg?: number;
  capacityVolumeM3?: number;
}
// Vehicle interface từ operationsAPI.ts
export interface Vehicle {
  id: string;
  name: string;
  type: 'TRUCK' | 'VAN' | 'MOTORCYCLE';
  status: 'AVAILABLE' | 'IN_USE' | 'MAINTENANCE' | 'MAINTENANCE_PENDING';
  statusDisplay?: string; // Vietnamese display name for status
  statusCode?: string; // status.name
  statusDescription?: string; // status.description
  created_at?: string;
  driver?: {
    id: string;
    name: string;
    phone: string;
  };
  mileage: number;
  lastMaintenance: string;
  nextMaintenance: string;
}

// Order interface từ operationsAPI.ts (unified version)
export interface Order {
  id: string;
  orderCode?: string; // New field from API
  description?: string; // New field from API
  totalAmount?: string; // New field from API
  customerName: string;
  customerPhone: string;
  pickupAddress?: string; // Optional for backward compatibility
  deliveryAddress: string;
  status: string; // Can be Vietnamese status or English enum
  priority?: 'LOW' | 'MEDIUM' | 'HIGH' | 'URGENT'; // Optional for backward compatibility
  assignedVehicle?: string;
  assignedDriver?: string;
  estimatedDeliveryTime?: string; // Optional for backward compatibility
  actualDeliveryTime?: string;
  weight?: number; // Optional for backward compatibility
  value?: number; // Optional for backward compatibility
  createdAt: string;
  updatedAt: string;
}

// Staff interface từ operationsAPI.ts
export interface Staff {
  id: string;
  name: string;
  email: string;
  phone: string;
  role: 'DRIVER' | 'DISPATCHER' | 'FLEET';
  status: 'ACTIVE' | 'ON_LEAVE' | 'SICK_LEAVE' | 'TERMINATED';
  department: string;
  shiftStart: string;
  shiftEnd: string;
  performanceScore: number;
  totalDeliveries: number;
  onTimeDeliveries: number;
}

// SystemMetrics interface từ operationsAPI.ts
export interface SystemMetrics {
  uptime: number;
  cpuUsage: number;
  memoryUsage: number;
  diskUsage: number;
  activeConnections: number;
  requestsPerMinute: number;
  responseTime: number;
  errorRate: number;
}

// Alert interface từ operationsAPI.ts
export interface Alert {
  id: string;
  level: 'INFO' | 'WARNING' | 'ERROR' | 'CRITICAL';
  message: string;
  source: string;
  timestamp: string;
  acknowledged: boolean;
  resolvedAt?: string;
}

// User
export interface User {
  id: string;
  name: string;
  email: string;
  role: string;
  roleIcon?: React.ReactNode;
  status: string;
  lastLogin: string;
}

// Role
export interface Role {
  key: string;
  name: string;
  icon?: string; // Tên icon, render ở component
  permissions: string[];
}

// Vehicle (chuẩn hóa theo backend và code thực tế)
export interface Vehicle {
  id: string;
  name: string;
  type: 'TRUCK' | 'VAN' | 'MOTORCYCLE';
  status: 'AVAILABLE' | 'IN_USE' | 'MAINTENANCE' | 'MAINTENANCE_PENDING';
  driver?: {
    id: string;
    name: string;
    phone: string;
  };
  location: {
    lat: number;
    lng: number;
    address: string;
  };
  fuel: number;
  mileage: number;
  lastMaintenance: string;
  nextMaintenance: string;
}

// SystemMetrics
export interface SystemMetrics {
  uptime: number;
  cpuUsage: number;
  memoryUsage: number;
  diskUsage: number;
  activeConnections: number;
  requestsPerMinute: number;
  responseTime: number;
  errorRate: number;
}

// Alert
export interface Alert {
  id: string;
  level: 'INFO' | 'WARNING' | 'ERROR' | 'CRITICAL';
  message: string;
  source: string;
  timestamp: string;
  acknowledged: boolean;
  resolvedAt?: string;
}

// Maintenance giữ lại nếu thực tế có dùng riêng
export interface Maintenance {
  id: number;
  vehicleId: number;
  date: string;
  description: string;
  status?: 'scheduled' | 'completed';
}

// Log giữ lại nếu thực tế có dùng riêng
export interface Log {
  time: string;
  user: string;
  action: string;
  detail: string;
  ip: string;
  status: "success" | "error";
}

