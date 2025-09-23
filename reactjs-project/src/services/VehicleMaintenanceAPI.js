// Lấy danh sách yêu cầu bảo trì khẩn cấp theo vehicleId
export async function fetchEmergencyRequestsByVehicleId(vehicleId) {
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
    }
    catch (error) {
        console.error('Error fetching emergency requests:', error);
        return [];
    }
}
// Lấy maintenance request theo ID cụ thể
export async function fetchMaintenanceRequestById(maintenanceId) {
    try {
        const response = await axios.get(`/api/fleet/maintenance-requests/${maintenanceId}`, {
            headers: getAuthHeaders(),
        });
        return response.data?.data || null;
    }
    catch (error) {
        console.error('Error fetching maintenance request by ID:', error);
        return null;
    }
}
export async function createVehicleMaintenance(data) {
    const response = await axios.post('/api/vehicle-maintenance', data, { headers: getAuthHeaders() });
    return response.data;
}
import axios from 'axios';
function getAuthHeaders() {
    const token = localStorage.getItem('token');
    return token ? { Authorization: `Bearer ${token}` } : {};
}
export async function fetchVehicleMaintenanceHistory() {
    const response = await axios.get('/api/vehicle-maintenance', {
        headers: getAuthHeaders(),
    });
    return response.data;
}
// Lấy lịch sử bảo trì theo vehicleId
export async function fetchVehicleMaintenanceByVehicleId(vehicleId) {
    const response = await axios.get(`/api/vehicle-maintenance?vehicleId=${vehicleId}`, {
        headers: getAuthHeaders(),
    });
    return response.data;
}
