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
