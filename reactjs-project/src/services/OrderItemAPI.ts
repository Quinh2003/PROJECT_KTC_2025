// API for OrderItem
export interface OrderItem {
  id: number;
  orderId: number;
  productName: string;
  quantity: number;
}

// Lấy danh sách sản phẩm của 1 đơn hàng
export async function fetchOrderItemsByOrderId(orderId: number | string): Promise<OrderItem[]> {
  const token = localStorage.getItem("token");
  const res = await fetch(`http://localhost:8080/api/order-items/order/${orderId}`, {
    headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
  });
  if (!res.ok) throw new Error("Failed to fetch order items");
  return res.json();
}

// Lấy tổng số lượng sản phẩm của 1 đơn hàng
export async function fetchOrderTotalQuantity(orderId: number | string): Promise<number> {
  const token = localStorage.getItem("token");
  const res = await fetch(`http://localhost:8080/api/order-items/order/${orderId}/total-quantity`, {
    headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
  });
  if (!res.ok) throw new Error("Failed to fetch total quantity");
  return res.json();
}
