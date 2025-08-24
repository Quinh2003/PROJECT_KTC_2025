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
  latitude?: number;
  longitude?: number;
}

export interface Driver {
  fullName: string;
}

export interface Vehicle {
  id: number;
  licensePlate?: string;
  currentDriver?: Driver;
}

export interface Order {
  id: number;
  createdAt: string;
  status?: OrderStatus;
  store?: Store;
  address?: Address;
  vehicle?: Vehicle;
  // Thêm các trường khác nếu cần
}
