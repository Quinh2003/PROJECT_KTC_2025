// src/services/orderAPI.ts
// Hàm fetch danh sách đơn hàng từ API

export async function fetchOrders() {
  const response = await fetch("http://localhost:8080/api/orders");
  if (!response.ok) throw new Error("Không thể lấy dữ liệu đơn hàng");
  return response.json();
}
