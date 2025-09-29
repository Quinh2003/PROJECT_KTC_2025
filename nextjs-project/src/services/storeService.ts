import axios from "axios";
import type {
  Store,
  UpdateStoreInfoDto,
  StoreInfoResponseDto,
} from "@/types/Store";

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
  baseURL: "http://localhost:8080/api", // Trỏ trực tiếp đến Spring Boot backend
  headers: {
    "Content-Type": "application/json",
  },
});

// Add request interceptor to include auth token
api.interceptors.request.use(
  (config) => {
    // Get token from localStorage
    const token =
      localStorage.getItem("token") || localStorage.getItem("accessToken");
    if (token) {
      config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
  },
  (error) => {
    return Promise.reject(error);
  }
);

// Add response interceptor to handle errors
api.interceptors.response.use(
  (response) => response,
  (error) => {
    if (error.response?.status === 401) {
      // Token expired or invalid, redirect to login
      localStorage.removeItem("token");
      localStorage.removeItem("accessToken");
      localStorage.removeItem("user");
      window.location.href = "/login";
    }
    return Promise.reject(error);
  }
);

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

  /**
   * Get store information for editing (includes read-only address)
   */
  getStoreInfoForEdit: async (id: string): Promise<UpdateStoreInfoDto> => {
    const { data } = await api.get<UpdateStoreInfoDto>(`/stores/${id}/info`);
    return data;
  },

  /**
   * Update store information with restricted fields
   * Only updates: storeName, email, phone, isActive, notes
   * Address is displayed but cannot be updated
   */
  updateStoreInfo: async (
    id: string,
    storeData: UpdateStoreInfoDto
  ): Promise<StoreInfoResponseDto> => {
    const { data } = await api.put<StoreInfoResponseDto>(
      `/stores/${id}/info`,
      storeData
    );
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
