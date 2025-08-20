
import type { Order, Vehicle } from '../types/Operations';


export async function addOrder(order: Partial<Order>): Promise<Order> {
  const token = localStorage.getItem("token");
  const res = await fetch("http://localhost:8080/api/orders", {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${token}`,
    },
    body: JSON.stringify(order),
  });
  if (!res.ok) throw new Error("Failed to add order");
  return res.json();
}


export async function editOrder(id: string | number, order: Partial<Order>): Promise<Order> {
  const token = localStorage.getItem("token");
  const res = await fetch(`http://localhost:8080/api/orders/${id}`, {
    method: "PATCH",
    headers: {
      "Content-Type": "application/json",
      "Authorization": `Bearer ${token}`,
    },
    body: JSON.stringify(order),
  });
  if (!res.ok) throw new Error("Failed to update order");
  return res.json();
}


export async function updateOrderVehicle(orderId: string | number, vehicleId: number): Promise<Order> {
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
  if (!res.ok) throw new Error("Failed to update order vehicle");
  return res.json();
}


export async function deleteOrder(id: string | number): Promise<boolean> {
  const token = localStorage.getItem("token");
  const res = await fetch(`http://localhost:8080/api/orders/${id}`, {
    method: "DELETE",
    headers: {
      "Authorization": `Bearer ${token}`,
    },
  });
  if (!res.ok) throw new Error("Failed to delete order");
  return true;
}


export async function fetchOrdersRaw(): Promise<Order[]> {
  const token = localStorage.getItem("token");
  const res = await fetch("http://localhost:8080/api/orders", {
    headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
  });
  if (!res.ok) throw new Error("Failed to fetch orders");
  return res.json();
}