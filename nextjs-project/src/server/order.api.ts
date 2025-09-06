// src/server/order.api.ts
// Các hàm gọi API liên quan đến đơn hàng

export async function getOrderByTrackingCodeApi(trackingCode: string) {
  const res = await fetch(`http://localhost:8080/api/orders/${trackingCode}`);
  return res;
}
