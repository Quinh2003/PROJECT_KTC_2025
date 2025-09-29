export interface OrderStatus {
  name: string;
  statusType: string;
}

export interface Store {
  storeName: string;
  address: string;
  latitude?: number;
  longitude?: number;
}

export interface Address {
  address: string;
  city?: string;
  latitude?: number;
  longitude?: number;
  contactName?: string;
  contactPhone?: string;
}

export interface Driver {
  id?: number;
  fullName: string;
}

export interface Vehicle {
  id: number;
  licensePlate?: string;
  currentDriver?: Driver;
}

export interface Delivery {
  id: number;
  orderId?: number;
  vehicleId?: number;
  driverId?: number;
  transportMode?: string;
  serviceType?: string;
  status?: string;
  createdAt?: string;
  updatedAt?: string;
}

export interface Order {
  id: number;
  createdAt: string;
  status?: OrderStatus;
  store?: Store;
  address?: Address;
  vehicle?: Vehicle;
  delivery?: Delivery;
  totalAmount?: number | string;
  // Thêm các trường khác nếu cần
  status_id?: number;
}


// Checklist/timeline cho đơn hàng
export interface TimelineStepDto {
  stepOrder: number;
  stepCode: string;
  stepName: string;
  description?: string;
  completed?: boolean;
  completedAt?: string | Date;
  actor?: {
    userId?: number;
    fullName?: string;
    role?: string;
    phone?: string;
  };
  details?: string;
  status?: string;
}
