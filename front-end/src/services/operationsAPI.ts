// API endpoints và các interface để tương tác với backend
export const API_BASE_URL = 'http://localhost:8080/api';

// Auth headers helper
export const getAuthHeaders = () => {
  const token = localStorage.getItem('token');
  return {
    'Authorization': `Bearer ${token}`,
    'Content-Type': 'application/json',
  };
};

// Interfaces cho Operations Dashboard
export interface Vehicle {
  id: string;
  name: string;
  type: 'TRUCK' | 'VAN' | 'MOTORCYCLE';
  status: 'ACTIVE' | 'MAINTENANCE' | 'IDLE' | 'OUT_OF_SERVICE';
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
  fuel: number; // percentage
  mileage: number;
  lastMaintenance: string;
  nextMaintenance: string;
}

export interface Order {
  id: string;
  customerName: string;
  customerPhone: string;
  pickupAddress: string;
  deliveryAddress: string;
  status: 'PENDING' | 'ASSIGNED' | 'PICKED_UP' | 'IN_TRANSIT' | 'DELIVERED' | 'CANCELLED';
  priority: 'LOW' | 'MEDIUM' | 'HIGH' | 'URGENT';
  assignedVehicle?: string;
  assignedDriver?: string;
  estimatedDeliveryTime: string;
  actualDeliveryTime?: string;
  weight: number;
  value: number;
  createdAt: string;
  updatedAt: string;
}

export interface Staff {
  id: string;
  name: string;
  email: string;
  phone: string;
  role: 'DRIVER' | 'DISPATCHER' | 'WAREHOUSE_STAFF' | 'MAINTENANCE';
  status: 'ACTIVE' | 'ON_LEAVE' | 'SICK_LEAVE' | 'TERMINATED';
  department: string;
  shiftStart: string;
  shiftEnd: string;
  performanceScore: number;
  totalDeliveries: number;
  onTimeDeliveries: number;
}

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

export interface Alert {
  id: string;
  level: 'INFO' | 'WARNING' | 'ERROR' | 'CRITICAL';
  message: string;
  source: string;
  timestamp: string;
  acknowledged: boolean;
  resolvedAt?: string;
}

// API functions
export const operationsAPI = {
  // Dashboard overview
  getOverviewStats: async () => {
    const response = await fetch(`${API_BASE_URL}/operations/overview`, {
      headers: getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to fetch overview stats');
    return response.json();
  },

  // Vehicles
  getVehicles: async () => {
    const response = await fetch(`${API_BASE_URL}/operations/vehicles`, {
      headers: getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to fetch vehicles');
    return response.json();
  },

  updateVehicleStatus: async (vehicleId: string, status: Vehicle['status']) => {
    const response = await fetch(`${API_BASE_URL}/operations/vehicles/${vehicleId}/status`, {
      method: 'PATCH',
      headers: getAuthHeaders(),
      body: JSON.stringify({ status }),
    });
    if (!response.ok) throw new Error('Failed to update vehicle status');
    return response.json();
  },

  // Orders
  getOrders: async (params?: { status?: string; priority?: string; limit?: number }) => {
    const queryParams = new URLSearchParams(params as Record<string, string>);
    const response = await fetch(`${API_BASE_URL}/operations/orders?${queryParams}`, {
      headers: getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to fetch orders');
    return response.json();
  },

  assignOrder: async (orderId: string, vehicleId: string, driverId: string) => {
    const response = await fetch(`${API_BASE_URL}/operations/orders/${orderId}/assign`, {
      method: 'POST',
      headers: getAuthHeaders(),
      body: JSON.stringify({ vehicleId, driverId }),
    });
    if (!response.ok) throw new Error('Failed to assign order');
    return response.json();
  },

  // Staff
  getStaff: async (department?: string) => {
    const queryParams = department ? `?department=${department}` : '';
    const response = await fetch(`${API_BASE_URL}/operations/staff${queryParams}`, {
      headers: getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to fetch staff');
    return response.json();
  },

  updateStaffStatus: async (staffId: string, status: Staff['status']) => {
    const response = await fetch(`${API_BASE_URL}/operations/staff/${staffId}/status`, {
      method: 'PATCH',
      headers: getAuthHeaders(),
      body: JSON.stringify({ status }),
    });
    if (!response.ok) throw new Error('Failed to update staff status');
    return response.json();
  },

  // Performance Analytics
  getPerformanceMetrics: async (timeRange: string) => {
    const response = await fetch(`${API_BASE_URL}/operations/performance?range=${timeRange}`, {
      headers: getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to fetch performance metrics');
    return response.json();
  },

  // System Monitoring
  getSystemMetrics: async () => {
    const response = await fetch(`${API_BASE_URL}/operations/system/metrics`, {
      headers: getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to fetch system metrics');
    return response.json();
  },

  getAlerts: async (acknowledged?: boolean) => {
    const queryParams = acknowledged !== undefined ? `?acknowledged=${acknowledged}` : '';
    const response = await fetch(`${API_BASE_URL}/operations/alerts${queryParams}`, {
      headers: getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to fetch alerts');
    return response.json();
  },

  acknowledgeAlert: async (alertId: string) => {
    const response = await fetch(`${API_BASE_URL}/operations/alerts/${alertId}/acknowledge`, {
      method: 'POST',
      headers: getAuthHeaders(),
    });
    if (!response.ok) throw new Error('Failed to acknowledge alert');
    return response.json();
  },

  // Reports
  generateReport: async (type: string, params: Record<string, unknown>) => {
    const response = await fetch(`${API_BASE_URL}/operations/reports/${type}`, {
      method: 'POST',
      headers: getAuthHeaders(),
      body: JSON.stringify(params),
    });
    if (!response.ok) throw new Error('Failed to generate report');
    return response.json();
  },
};
