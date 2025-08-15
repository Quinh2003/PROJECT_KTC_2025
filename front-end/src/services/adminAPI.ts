// Cập nhật user (PUT - update toàn bộ)
export async function updateUser(id: string | number, user: User): Promise<User> {
  const token = localStorage.getItem("token");
  const res = await fetch(`${API_URL}/${id}`, {
    method: "PUT",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${token}`,
    },
    body: JSON.stringify(user),
  });
  if (!res.ok) throw new Error("Failed to update user (PUT)");
  return res.json();
}
// Admin API service: fetch all users, fetch all drivers, add/edit/delete user

export type User = {
  id: string | number;
  username: string;
  fullName: string;
  email: string;
  phone?: string;
  password?: string;
  role?: {
    id: string | number;
    roleName: string;
    description?: string;
  };
  status?: {
    id: string | number;
    statusType: string;
    name: string;
    description?: string;
  };
};

const API_URL = "http://localhost:8080/api/auth/users";

function getAuthHeaders(): HeadersInit | undefined {
  const token = localStorage.getItem("token");
  return token ? { Authorization: `Bearer ${token}` } : undefined;
}

export async function fetchUsers(): Promise<User[]> {
  const headers = getAuthHeaders();
  const res = await fetch(API_URL, headers ? { headers } : undefined);
  if (!res.ok) throw new Error("Failed to fetch users");
  return res.json();
}

export async function fetchDrivers(): Promise<User[]> {
  // Giả sử role=driver, backend cần hỗ trợ filter này
  const headers = getAuthHeaders();
  const res = await fetch(`${API_URL}?role=driver`, headers ? { headers } : undefined);
  if (!res.ok) throw new Error("Failed to fetch drivers");
  return res.json();
}

export async function addUser(user: Partial<User>): Promise<User> {
  const token = localStorage.getItem("token");
  const res = await fetch(API_URL, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${token}`,
    },
    body: JSON.stringify(user),
  });
  if (!res.ok) throw new Error("Failed to add user");
  return res.json();
}

// Sử dụng PUT để cập nhật toàn bộ user
export async function editUser(id: string | number, user: User): Promise<User> {
  console.log("[adminAPI] editUser called with id:", id, "user:", user);
  const token = localStorage.getItem("token");
  
  try {
    const res = await fetch(`${API_URL}/${id}`, {
      method: "PUT",
      headers: {
        "Content-Type": "application/json",
        "Authorization": `Bearer ${token}`,
      },
      body: JSON.stringify(user),
    });
    
    console.log("[adminAPI] Response status:", res.status, res.statusText);
    
    if (!res.ok) {
      const errorText = await res.text();
      console.error("[adminAPI] Error response:", errorText);
      throw new Error(`Failed to update user: ${res.status} ${res.statusText} - ${errorText}`);
    }
    
    const result = await res.json();
    console.log("[adminAPI] Update successful:", result);
    return result;
  } catch (error) {
    console.error("[adminAPI] editUser error:", error);
    throw error;
  }
}

export async function deleteUser(id: string | number): Promise<boolean> {
  const token = localStorage.getItem("token");
  const res = await fetch(`${API_URL}/${id}`, {
    method: "DELETE",
    headers: {
      "Authorization": `Bearer ${token}`,
    },
  });
  if (!res.ok) throw new Error("Failed to delete user");
  return true;
}

// Activity Logs API
export type ActivityLog = {
  id: number;
  time: string;
  user: string;
  action: string;
  detail: string;
  status: string;
  role?: string;
};

export async function fetchActivityLogs(params?: {
  dateFrom?: string;
  dateTo?: string;
  actionType?: string;
  userId?: number;
  page?: number;
  size?: number;
}, retries = 3): Promise<ActivityLog[]> {
  const makeRequest = async (): Promise<ActivityLog[]> => {
    const token = localStorage.getItem("token");
    
    // Build query parameters
    const queryParams = new URLSearchParams();
    if (params?.dateFrom) queryParams.append('dateFrom', params.dateFrom);
    if (params?.dateTo) queryParams.append('dateTo', params.dateTo);
    if (params?.actionType) queryParams.append('actionType', params.actionType);
    if (params?.userId) queryParams.append('userId', params.userId.toString());
    if (params?.page !== undefined) queryParams.append('page', params.page.toString());
    if (params?.size !== undefined) queryParams.append('size', params.size.toString());
    
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
      } else if (res.status === 401) {
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
    } catch (error) {
      console.error(`fetchActivityLogs: Attempt ${attempt} failed:`, error);
      
      // If it's an auth error (401/403), don't retry
      if (error instanceof Error && (error.message.includes('401') || error.message.includes('403'))) {
        throw error;
      }
      
      // If last attempt, throw error
      if (attempt === retries) {
        throw error;
      }
      
      // Wait before retry (exponential backoff)
      await new Promise(resolve => setTimeout(resolve, 1000 * attempt));
    }
  }

  throw new Error('All retry attempts failed');
}
