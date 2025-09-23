// Batch: lấy tổng số lượng sản phẩm cho nhiều đơn hàng
export async function fetchOrdersTotalQuantityBatch(orderIds) {
    const token = localStorage.getItem("token");
    const res = await fetch(`http://localhost:8080/api/order-items/orders/total-quantity`, {
        method: "POST",
        headers: {
            "Content-Type": "application/json",
            ...(token ? { "Authorization": `Bearer ${token}` } : {})
        },
        body: JSON.stringify(orderIds),
    });
    if (!res.ok)
        throw new Error("Failed to fetch batch total quantity");
    return res.json();
}
// Lấy danh sách sản phẩm của 1 đơn hàng (có phân trang)
export async function fetchOrderItemsByOrderIdPaged(orderId, page = 0, size = 10) {
    const token = localStorage.getItem("token");
    const res = await fetch(`http://localhost:8080/api/order-items/order/${orderId}/paged?page=${page}&size=${size}`, {
        headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
    });
    if (!res.ok)
        throw new Error("Failed to fetch paged order items");
    const data = await res.json();
    return {
        content: Array.isArray(data.content)
            ? data.content.map((item) => ({
                id: item.id,
                product: {
                    name: item.product?.name || item.productName || "",
                    weight: item.product?.weight,
                    volume: item.product?.volume,
                    fragile: item.product?.isFragile === true,
                },
                quantity: item.quantity,
                shippingFee: item.shippingFee,
                notes: item.notes,
            }))
            : [],
        totalElements: data.totalElements ?? 0,
        totalPages: data.totalPages ?? 1,
    };
}
// Lấy tất cả order items (có phân trang)
export async function fetchAllOrderItemsPaged(page = 0, size = 10) {
    const token = localStorage.getItem("token");
    const res = await fetch(`http://localhost:8080/api/order-items?page=${page}&size=${size}`, {
        headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
    });
    if (!res.ok)
        throw new Error("Failed to fetch all order items paged");
    const data = await res.json();
    return {
        content: Array.isArray(data.content)
            ? data.content.map((item) => ({
                id: item.id,
                product: {
                    name: item.product?.name || item.productName || "",
                    weight: item.product?.weight,
                    volume: item.product?.volume,
                    fragile: item.product?.isFragile === true,
                },
                quantity: item.quantity,
                shippingFee: item.shippingFee,
                notes: item.notes,
            }))
            : [],
        totalElements: data.totalElements ?? 0,
        totalPages: data.totalPages ?? 1,
    };
}
// Lấy danh sách sản phẩm của 1 đơn hàng (trả về đúng cấu trúc ProductItem cho OrderDetailModal)
export async function fetchOrderItemsByOrderId(orderId) {
    const token = localStorage.getItem("token");
    const res = await fetch(`http://localhost:8080/api/order-items/order/${orderId}`, {
        headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
    });
    if (!res.ok)
        throw new Error("Failed to fetch order items");
    const data = await res.json();
    // Map về đúng cấu trúc ProductItem
    return Array.isArray(data)
        ? data.map((item) => {
            console.log('Raw item data:', item);
            console.log('Product isFragile:', item.product?.isFragile);
            console.log('Product fragile:', item.product?.fragile);
            return {
                id: item.id,
                product: {
                    name: item.product?.name || item.productName || "",
                    weight: item.product?.weight,
                    volume: item.product?.volume,
                    fragile: item.product?.isFragile === true,
                },
                quantity: item.quantity,
                shippingFee: item.shippingFee,
                notes: item.notes,
            };
        })
        : [];
}
// Lấy tổng số lượng sản phẩm của 1 đơn hàng
export async function fetchOrderTotalQuantity(orderId) {
    const token = localStorage.getItem("token");
    const res = await fetch(`http://localhost:8080/api/order-items/order/${orderId}/total-quantity`, {
        headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
    });
    if (!res.ok)
        throw new Error("Failed to fetch total quantity");
    return res.json();
}
