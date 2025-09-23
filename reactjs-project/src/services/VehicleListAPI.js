const API_URL = "http://localhost:8080/api/vehicles";
function getAuthHeaders() {
    const token = localStorage.getItem("token");
    return token ? { Authorization: `Bearer ${token}` } : undefined;
}
export async function fetchVehicles() {
    const headers = getAuthHeaders();
    const res = await fetch(API_URL, headers ? { headers } : undefined);
    if (!res.ok)
        throw new Error("Failed to fetch vehicles");
    const json = await res.json();
    // API trả về { data: [...] }
    return Array.isArray(json.data) ? json.data : [];
}
// Server-side pagination: trả về { data: Vehicle[], total: number }
export async function fetchVehiclesRaw(page = 1, size = 5) {
    const token = localStorage.getItem("token");
    const res = await fetch(`${API_URL}?page=${page}&size=${size}`, {
        headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
    });
    if (!res.ok)
        throw new Error("Failed to fetch vehicles");
    const data = await res.json();
    // data.data: array, data.totalRecords: number
    return {
        data: Array.isArray(data.data) ? data.data : [],
        total: data.totalRecords || 0
    };
}
// Thêm function để lấy tổng số phương tiện và stats
export async function fetchVehicleStats() {
    const token = localStorage.getItem("token");
    const res = await fetch(`${API_URL}?page=1&size=200`, {
        headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
    });
    if (!res.ok)
        throw new Error("Failed to fetch vehicle stats");
    const data = await res.json();
    // Đảm bảo sắp xếp theo thời gian cập nhật mới nhất lên đầu
    const sampleVehicles = (data.data || []).sort((a, b) => {
        const dateA = new Date(a.updatedAt || 0).getTime();
        const dateB = new Date(b.updatedAt || 0).getTime();
        return dateB - dateA; // Mới nhất lên đầu
    });
    return {
        totalRecords: data.totalRecords || 0,
        sampleVehicles
    };
}
export async function addVehicle(vehicle) {
    const token = localStorage.getItem("token");
    const res = await fetch(API_URL, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${token}`,
        },
        body: JSON.stringify(vehicle),
    });
    if (!res.ok)
        throw new Error("Failed to add vehicle");
    return res.json();
}
export async function editVehicle(id, vehicle) {
    const token = localStorage.getItem("token");
    const res = await fetch(`${API_URL}/${id}`, {
        method: "PATCH",
        headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${token}`,
        },
        body: JSON.stringify(vehicle),
    });
    if (!res.ok)
        throw new Error("Failed to update vehicle");
    return res.json();
}
export async function deleteVehicle(id) {
    const token = localStorage.getItem("token");
    const res = await fetch(`${API_URL}/${id}`, {
        method: "DELETE",
        headers: {
            "Authorization": `Bearer ${token}`,
        },
    });
    if (!res.ok)
        throw new Error("Failed to delete vehicle");
    return true;
}
export async function fetchVehicleById(id) {
    const headers = getAuthHeaders();
    const res = await fetch(`${API_URL}/${id}`, headers ? { headers } : undefined);
    if (!res.ok)
        throw new Error("Failed to fetch vehicle");
    return res.json();
}
export async function updateVehicleStatus(vehicleId, status) {
    const token = localStorage.getItem("token");
    const res = await fetch(`${API_URL}/${vehicleId}/status`, {
        method: "PATCH",
        headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${token}`,
        },
        body: JSON.stringify({ status }),
    });
    if (!res.ok)
        throw new Error("Failed to update vehicle status");
    return res.json();
}
// Gán tài xế cho xe (dùng endpoint riêng)
export async function assignDriverToVehicle(vehicleId, driverId) {
    const token = localStorage.getItem("token");
    const res = await fetch(`${API_URL}/${vehicleId}/assign-driver`, {
        method: "PATCH",
        headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${token}`,
        },
        body: JSON.stringify({ driverId }),
    });
    if (!res.ok)
        throw new Error("Failed to assign driver to vehicle");
    return res.json();
}
