import axios from "axios";
import type { Order } from "@/types/orders";

export interface OrderSummary {
  orderId: number;
  storeId: number;
  createdAt: string;
  deliveryAddress: string;
  totalItems: number;
  deliveryFee: number;
  orderStatus: string;
}

const api = axios.create({
  baseURL: "/api",
});

export const orderApi = {
  getOrders: async () => {
    const { data } = await api.get<Order[]>("/orders");
    return data;
  },

  getOrderById: async (id: string) => {
    const { data } = await api.get<Order>(`/orders/${id}`);
    return data;
  },

  getOrderTracking: async (id: string) => {
    const { data } = await api.get<any>(`/orders/${id}/tracking`);
    return data;
  },

  createOrder: async (orderData: Partial<Order>) => {
    const { data } = await api.post<Order>("/orders", orderData);
    return data;
  },

  updateOrder: async (id: string, orderData: Partial<Order>) => {
    const { data } = await api.patch<Order>(`/orders/${id}`, orderData);
    return data;
  },

  deleteOrder: async (id: string) => {
    await api.delete(`/orders/${id}`);
  },

  getOrdersByUser: async (userId: number): Promise<OrderSummary[]> => {
    const { data } = await axios.get<OrderSummary[]>(
      `http://localhost:8080/api/orders/user/${userId}/summary`
    );
    return data;
  },

  downloadInvoice: async (id: string) => {
    const response = await api.get(`/orders/${id}/invoice`, {
      responseType: "blob",
    });
    const url = window.URL.createObjectURL(new Blob([response.data]));
    const link = document.createElement("a");
    link.href = url;
    link.setAttribute("download", `invoice-${id}.pdf`);
    document.body.appendChild(link);
    link.click();
    link.parentNode?.removeChild(link);
  },
};
