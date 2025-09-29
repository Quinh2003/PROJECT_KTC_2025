// Lấy danh sách yêu cầu bảo trì khẩn cấp theo vehicleId
export async function fetchEmergencyRequestsByVehicleId(vehicleId: number | string): Promise<any[]> {
  try {
    // Sử dụng endpoint đúng từ backend
    const response = await axios.get(`/api/fleet/maintenance-requests?vehicleId=${vehicleId}`, {
      headers: getAuthHeaders(),
    });
    
    // Backend trả về ApiResponse với data là Page object
    if (response.data && response.data.data && response.data.data.content) {
      return response.data.data.content;
    }
    return response.data?.data || [];
  } catch (error) {
    console.error('Error fetching emergency requests:', error);
    return [];
  }
}

// Lấy maintenance request theo ID cụ thể
export async function fetchMaintenanceRequestById(maintenanceId: number | string): Promise<any> {
  try {
    const response = await axios.get(`/api/fleet/maintenance-requests/${maintenanceId}`, {
      headers: getAuthHeaders(),
    });
    return response.data?.data || null;
  } catch (error) {
    console.error('Error fetching maintenance request by ID:', error);
    return null;
  }
}
export interface CreateVehicleMaintenanceRequest {
  vehicle: { id: number };
  maintenanceDate: string;
  nextDueDate?: string;
  maintenanceType?: string;
  description: string;
  cost?: number;
  status?: { id: number };
  notes?: string;
}

export async function createVehicleMaintenance(data: CreateVehicleMaintenanceRequest) {
  const response = await axios.post<VehicleMaintenance>(
    '/api/vehicle-maintenance',
    data,
    { headers: getAuthHeaders() }
  );
  return response.data;
}
import axios from 'axios';

function getAuthHeaders() {
  const token = localStorage.getItem('token');
  return token ? { Authorization: `Bearer ${token}` } : {};
}

export interface VehicleMaintenance {
  id: number;
  vehicle: {
    id: number;
    licensePlate: string;
    [key: string]: any;
  };
  maintenanceDate: string;
  nextDueDate: string;
  maintenanceType: string;
  description: string;
  cost: number;
  status: {
    id: number;
    name: string;
    [key: string]: any;
  };
  createdAt: string;
  createdBy: any;
  updatedAt: string;
  notes: string;
}

export async function fetchVehicleMaintenanceHistory(): Promise<VehicleMaintenance[]> {
  const response = await axios.get<VehicleMaintenance[]>('/api/vehicle-maintenance', {
    headers: getAuthHeaders(),
  });
  return response.data;
}

// Lấy lịch sử bảo trì theo vehicleId
export async function fetchVehicleMaintenanceByVehicleId(vehicleId: number | string): Promise<VehicleMaintenance[]> {
  const response = await axios.get<VehicleMaintenance[]>(`/api/vehicle-maintenance?vehicleId=${vehicleId}`, {
    headers: getAuthHeaders(),
  });
  return response.data;
}