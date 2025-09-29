export interface Store {
  id: number;
  storeName: string;
  email: string;
  phone: string;
  address: string;
  latitude?: number;
  longitude?: number;
  isActive: boolean;
  notes?: string;
  createdAt?: Date;
  updatedAt?: Date;
  createdBy?: string;
  statistics?: StoreStatistics;
}

export interface StoreStatistics {
  totalOrders: number;
  completedOrders: number;
  pendingOrders: number;
  averageDeliveryTime: number;
  totalRevenue: number;
}

/**
 * DTO for updating store information with restricted fields
 * Fields that can be updated: storeName, email, phone, isActive, notes
 * Address is displayed but cannot be updated
 */
export interface UpdateStoreInfoDto {
  storeName: string;
  email: string;
  phone: string;
  isActive: boolean;
  notes?: string;
  address?: string; // Read-only field for display purposes
}

/**
 * Response DTO for store information
 * Shows all store information including address (read-only)
 */
export interface StoreInfoResponseDto {
  id: number;
  storeName: string;
  email: string;
  phone: string;
  address: string; // Read-only, cannot be updated via update endpoint
  isActive: boolean;
  notes?: string;
  createdAt?: Date;
  updatedAt?: Date;
  createdByUserName?: string;
}
