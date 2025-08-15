import type { Vehicle, Staff } from "../types/dashboard";
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
