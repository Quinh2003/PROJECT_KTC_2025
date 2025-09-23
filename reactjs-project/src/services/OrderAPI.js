// Tìm đơn hàng theo ID
export async function fetchOrderById(orderId, token) {
    const authToken = token || localStorage.getItem("token") || "";
    const res = await fetch(`http://localhost:8080/api/orders/${orderId}`, {
        headers: authToken ? { "Authorization": `Bearer ${authToken}` } : undefined,
    });
    if (!res.ok)
        return null;
    const data = await res.json();
    // Đảm bảo trả về đúng kiểu Order
    return {
        ...data,
        createdAt: data.createdAt || data.created_at || ""
    };
}
export async function fetchOrders(page, size, token) {
    const res = await fetch(`http://localhost:8080/api/orders?page=${page}&size=${size}`, {
        headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
    });
    if (!res.ok)
        throw new Error("Failed to fetch orders");
    const data = await res.json();
    // Map lại trường created_at -> createdAt nếu cần, rồi sort
    const orders = (data.data || []).map((order) => ({
        ...order,
        createdAt: order.createdAt || order.created_at || ""
    })).sort((a, b) => {
        const dateA = new Date(a.createdAt).getTime();
        const dateB = new Date(b.createdAt).getTime();
        return dateB - dateA;
    });
    return {
        data: orders,
        totalPages: data.totalPages || 1,
        totalRecords: data.totalRecords || 0,
    };
}
export async function addOrder(order) {
    const token = localStorage.getItem("token");
    const res = await fetch("http://localhost:8080/api/orders", {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${token}`,
        },
        body: JSON.stringify(order),
    });
    if (!res.ok)
        throw new Error("Failed to add order");
    return res.json();
}
export async function editOrder(id, order) {
    const token = localStorage.getItem("token");
    const res = await fetch(`http://localhost:8080/api/orders/${id}`, {
        method: "PATCH",
        headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${token}`,
        },
        body: JSON.stringify(order),
    });
    if (!res.ok)
        throw new Error("Failed to update order");
    return res.json();
}
export async function updateOrderVehicle(orderId, vehicleId) {
    const token = localStorage.getItem("token");
    const res = await fetch(`http://localhost:8080/api/orders/${orderId}/vehicle`, {
        method: "PATCH",
        headers: {
            "Content-Type": "application/json",
            "Authorization": `Bearer ${token}`,
        },
        body: JSON.stringify({
            vehicleId: vehicleId === 0 ? null : vehicleId
        }),
    });
    if (!res.ok)
        throw new Error("Failed to update order vehicle");
    return res.json();
}
export async function deleteOrder(id) {
    const token = localStorage.getItem("token");
    const res = await fetch(`http://localhost:8080/api/orders/${id}`, {
        method: "DELETE",
        headers: {
            "Authorization": `Bearer ${token}`,
        },
    });
    if (!res.ok)
        throw new Error("Failed to delete order");
    return true;
}
// Server-side pagination: trả về { data: Order[], total: number }
export async function fetchOrdersRaw(page = 1, size = 5) {
    const token = localStorage.getItem("token");
    const res = await fetch(`http://localhost:8080/api/orders?page=${page}&size=${size}`, {
        headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
    });
    if (!res.ok)
        throw new Error("Failed to fetch orders");
    const data = await res.json();
    // data.data: array, data.totalRecords: number
    return {
        data: Array.isArray(data.data) ? data.data : [],
        total: data.totalRecords || 0
    };
}
// Thêm function để lấy tổng số đơn hàng và stats
export async function fetchOrderStats() {
    const token = localStorage.getItem("token");
    const res = await fetch("http://localhost:8080/api/orders?page=1&size=200", {
        headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
    });
    if (!res.ok)
        throw new Error("Failed to fetch order stats");
    const data = await res.json();
    // Đảm bảo sắp xếp theo thời gian tạo mới nhất lên đầu
    const sampleOrders = (data.data || []).sort((a, b) => {
        const dateA = new Date(a.createdAt || 0).getTime();
        const dateB = new Date(b.createdAt || 0).getTime();
        return dateB - dateA; // Mới nhất lên đầu
    });
    return {
        totalRecords: data.totalRecords || 0,
        sampleOrders
    };
}
