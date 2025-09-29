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

export interface OrderStatsDto {
  totalOrders: number;
  processingOrders: number;
  completedOrders: number;
}

export interface PaginatedOrderSummaryResponse {
  data: OrderSummary[];
  pageNumber: number;
  pageSize: number;
  totalRecords: number;
  totalPages: number;
  hasNext: boolean;
  hasPrevious: boolean;
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

  getOrdersByUserPaginated: async (
    userId: number,
    page: number = 1,
    size: number = 10
  ): Promise<PaginatedOrderSummaryResponse> => {
    const { data } = await axios.get<PaginatedOrderSummaryResponse>(
      `http://localhost:8080/api/orders/user/${userId}/summary/paginated?page=${page}&size=${size}`
    );
    return data;
  },

  searchOrdersByStoreAndOrderId: async (
    storeId: number,
    orderId: number,
    page: number = 1,
    size: number = 10
  ): Promise<PaginatedOrderSummaryResponse> => {
    const { data } = await axios.get<PaginatedOrderSummaryResponse>(
      `http://localhost:8080/api/orders/search-by-order-id?storeId=${storeId}&orderId=${orderId}&page=${page}&size=${size}`
    );
    return data;
  },

  searchOrdersByStoreAndDateRange: async (
    storeId: number,
    page: number = 1,
    size: number = 10,
    fromDate?: string,
    toDate?: string
  ): Promise<PaginatedOrderSummaryResponse> => {
    let url = `http://localhost:8080/api/orders/search-by-date?storeId=${storeId}&page=${page}&size=${size}`;

    if (fromDate) {
      url += `&fromDate=${fromDate}`;
    }

    if (toDate) {
      url += `&toDate=${toDate}`;
    }

    const { data } = await axios.get<PaginatedOrderSummaryResponse>(url);
    return data;
  },

  /**
   * Unified search API that supports multiple search criteria
   * @param storeId - required, orders must belong to this store
   * @param orderId - optional, exact match if provided
   * @param fromDate - optional, start date (YYYY-MM-DD format)
   * @param toDate - optional, end date (YYYY-MM-DD format)
   * @param statusList - optional, array of status names for multiple selection
   * @param page - page number (1-based)
   * @param size - page size
   */
  searchOrdersUnified: async (
    storeId: number,
    page: number = 1,
    size: number = 10,
    orderId?: number,
    fromDate?: string,
    toDate?: string,
    statusList?: string[]
  ): Promise<PaginatedOrderSummaryResponse> => {
    let url = `http://localhost:8080/api/orders/search?storeId=${storeId}&page=${page}&size=${size}`;

    if (orderId) {
      url += `&orderId=${orderId}`;
    }

    if (fromDate) {
      url += `&fromDate=${fromDate}`;
    }

    if (toDate) {
      url += `&toDate=${toDate}`;
    }

    if (statusList && statusList.length > 0) {
      // Add multiple status parameters
      statusList.forEach((status) => {
        if (status && status.trim()) {
          url += `&status=${encodeURIComponent(status)}`;
        }
      });
    }

    const { data } = await axios.get<PaginatedOrderSummaryResponse>(url);
    return data;
  },

  /**
   * Get order statistics for a user
   * @param userId - the user ID to get statistics for
   * @returns OrderStatsDto with total, processing, and completed order counts
   */
  getUserOrderStats: async (userId: number): Promise<OrderStatsDto> => {
    const { data } = await axios.get<OrderStatsDto>(
      `http://localhost:8080/api/orders/user/${userId}/stats`
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
