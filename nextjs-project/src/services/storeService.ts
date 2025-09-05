import axios from "axios";
import type { Store } from "@/types/Store";

/**
 * Interfaces
 */
export interface StoreSummary {
  storeId: number;
  name: string;
  address: string;
  phone: string;
  email: string;
  status: string;
  rating: number;
  totalOrders: number;
}

export interface StoreAnalytics {
  totalRevenue: number;
  totalOrders: number;
  topProducts: { name: string; sales: number }[];
}

/**
 * DTO types
 */
export type CreateStoreDto = Omit<Store, "id" | "createdAt" | "updatedAt">;
export type UpdateStoreDto = Partial<CreateStoreDto>;

/**
 * Axios instance
 */
const api = axios.create({
  baseURL: "/api", // Sử dụng Next.js API routes
});

/**
 * Service methods
 */
export const storeService = {
  getStores: async (): Promise<Store[]> => {
    const { data } = await api.get<Store[]>("/stores");
    return data;
  },

  getStoreById: async (id: string): Promise<Store> => {
    const { data } = await api.get<Store>(`/stores/${id}`);
    return data;
  },

  getStoreAnalytics: async (id: string): Promise<StoreAnalytics> => {
    const { data } = await api.get<StoreAnalytics>(`/stores/${id}/analytics`);
    return data;
  },

  createStore: async (storeData: CreateStoreDto): Promise<Store> => {
    const { data } = await api.post<Store>("/stores", storeData);
    return data;
  },

  updateStore: async (
    id: string,
    storeData: UpdateStoreDto
  ): Promise<Store> => {
    const { data } = await api.patch<Store>(`/stores/${id}`, storeData);
    return data;
  },

  deleteStore: async (id: string): Promise<void> => {
    await api.delete(`/stores/${id}`);
  },

  getStoresByUserId: async (userId: string): Promise<Store[]> => {
    const { data } = await api.get<Store[]>(`/stores/user/${userId}`);
    return data;
  },

  getStoreReport: async (id: string): Promise<void> => {
    const response = await api.get(`/stores/${id}/report`, {
      responseType: "blob",
    });

    const url = window.URL.createObjectURL(new Blob([response.data]));
    const link = document.createElement("a");
    link.href = url;
    link.setAttribute("download", `store-report-${id}.pdf`);
    document.body.appendChild(link);
    link.click();
    link.remove();
    window.URL.revokeObjectURL(url);
  },
};
