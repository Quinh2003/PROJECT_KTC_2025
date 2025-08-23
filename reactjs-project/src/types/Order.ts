export interface OrderStatus {
  name: string;
  statusType: string;
}

export interface Store {
  storeName: string;
  address: string;
}

export interface Address {
  address: string;
}

export interface Driver {
  fullName: string;
}

export interface Vehicle {
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
