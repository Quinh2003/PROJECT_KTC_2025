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
