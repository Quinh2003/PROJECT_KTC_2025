const API_URL = "http://localhost:8080/api/auth/users";
function getAuthHeaders() {
    const token = localStorage.getItem("token");
    return token ? { Authorization: `Bearer ${token}` } : undefined;
}
// Lấy tất cả user
export async function fetchUsers() {
    const headers = getAuthHeaders();
    const res = await fetch(API_URL, headers ? { headers } : undefined);
    if (!res.ok)
        throw new Error("Failed to fetch users");
    return res.json();
}
// Lấy tất cả driver
export async function fetchDrivers() {
    const headers = getAuthHeaders();
    const res = await fetch(`${API_URL}?role=driver`, headers ? { headers } : undefined);
    if (!res.ok)
        throw new Error("Failed to fetch drivers");
    return res.json();
}
// Thêm user mới
export async function addUser(user) {
    const token = localStorage.getItem("token");
    const res = await fetch(API_URL, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${token}`,
        },
        body: JSON.stringify(user),
    });
    if (!res.ok)
        throw new Error("Failed to add user");
    return res.json();
}
// Sửa user (PUT)
export async function editUser(id, user) {
    const token = localStorage.getItem("token");
    // Chuẩn hóa payload nếu backend yêu cầu
    const payload = {
        ...user,
        password: user.password || "1234",
        role: user.role,
        status: user.isActive ? { id: 7 } : { id: 8 },
        googleId: null,
        notes: null,
    };
    const res = await fetch(`${API_URL}/${id}`, {
        method: "PUT",
        headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${token}`,
        },
        body: JSON.stringify(payload),
    });
    if (!res.ok)
        throw new Error("Failed to update user");
    return res.json();
}
// Xóa user
export async function deleteUser(id) {
    const token = localStorage.getItem("token");
    const res = await fetch(`${API_URL}/${id}`, {
        method: "DELETE",
        headers: {
            "Authorization": `Bearer ${token}`,
        },
    });
    if (!res.ok)
        throw new Error("Failed to delete user");
    return true;
}
// Cập nhật trạng thái user
export async function updateUserStatus(userId, status) {
    const token = localStorage.getItem("token");
    const statusMap = {
        "Active": 7,
        "Inactive": 8,
        "Pending": 1,
    };
    const statusId = statusMap[status];
    if (!statusId)
        throw new Error(`Invalid status: ${status}`);
    const payload = { statusId };
    const res = await fetch(`${API_URL}/${userId}/status`, {
        method: "PATCH",
        headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${token}`,
        },
        body: JSON.stringify(payload),
    });
    if (!res.ok)
        throw new Error("Failed to update user status");
    return res.json();
}
export async function fetchActivityLogs(params, retries = 3) {
    const makeRequest = async () => {
        const token = localStorage.getItem("token");
        // Build query parameters
        const queryParams = new URLSearchParams();
        if (params?.dateFrom)
            queryParams.append('dateFrom', params.dateFrom);
        if (params?.dateTo)
            queryParams.append('dateTo', params.dateTo);
        if (params?.actionType)
            queryParams.append('actionType', params.actionType);
        if (params?.userId)
            queryParams.append('userId', params.userId.toString());
        if (params?.page !== undefined)
            queryParams.append('page', params.page.toString());
        if (params?.size !== undefined)
            queryParams.append('size', params.size.toString());
        const url = `http://localhost:8080/api/admin/activity-logs${queryParams.toString() ? '?' + queryParams.toString() : ''}`;
        const res = await fetch(url, {
            headers: {
                "Authorization": `Bearer ${token}`,
                "Content-Type": "application/json",
            },
            credentials: 'include', // Include cookies for session management
        });
        if (!res.ok) {
            if (res.status === 403) {
                throw new Error(`Authentication failed (403) - Session may have expired`);
            }
            else if (res.status === 401) {
                throw new Error(`Authentication failed (401) - Please login again`);
            }
            throw new Error(`HTTP error! status: ${res.status}`);
        }
        return res.json();
    };
    // Retry logic for handling temporary network/auth issues
    for (let attempt = 1; attempt <= retries; attempt++) {
        try {
            const result = await makeRequest();
            console.log(`fetchActivityLogs: Success on attempt ${attempt}`, result.length, 'logs');
            return result;
        }
        catch (error) {
            console.error(`fetchActivityLogs: Attempt ${attempt} failed:`, error);
            // If it's an auth error (401/403), don't retry
            if (error instanceof Error && (error.message.includes('401') || error.message.includes('403'))) {
                throw error;
            }
            // If last attempt, return mock data
            if (attempt === retries) {
                console.warn('fetchActivityLogs: All attempts failed, returning mock data');
                // Mock data fallback
                const now = new Date();
                const mockLogs = [
                    {
                        id: 1,
                        time: new Date(now.getTime() - 1000 * 60 * 5).toISOString(),
                        user: 'admin',
                        action: 'LOGIN',
                        detail: 'Successful login',
                        status: 'success',
                        role: 'Administrator',
                    },
                    {
                        id: 2,
                        time: new Date(now.getTime() - 1000 * 60 * 60).toISOString(),
                        user: 'john.doe',
                        action: 'UPDATE',
                        detail: 'Updated user profile',
                        status: 'success',
                        role: 'Manager',
                    },
                    {
                        id: 3,
                        time: new Date(now.getTime() - 1000 * 60 * 60 * 2).toISOString(),
                        user: 'jane.smith',
                        action: 'DELETE',
                        detail: 'Deleted vehicle record',
                        status: 'error',
                        role: 'Operator',
                    },
                    {
                        id: 4,
                        time: new Date(now.getTime() - 1000 * 60 * 60 * 3).toISOString(),
                        user: 'admin',
                        action: 'LOGOUT',
                        detail: 'User logged out',
                        status: 'success',
                        role: 'Administrator',
                    },
                ];
                return mockLogs;
            }
            // Wait before retry (exponential backoff)
            await new Promise(resolve => setTimeout(resolve, 1000 * attempt));
        }
    }
    // Should never reach here
    throw new Error('All retry attempts failed');
}
