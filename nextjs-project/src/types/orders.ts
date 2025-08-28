export interface OrderItem {
  id: string;
  name: string;
  quantity: number;
  price: number;
}

export interface TrackingInfo {
  timestamp: string;
  status: string;
  location: string;
  description: string;
}

export interface Order {
  id: string;
  createdAt: string;
  updatedAt: string;
  customerId: string;
  address: string;
  items: OrderItem[];
  itemCount: number;
  shippingFee: number;
  subtotal: number;
  total: number;
  status: OrderStatus;
  trackingHistory: TrackingInfo[];
}

export type OrderStatus =
  | "pending"
  | "confirmed"
  | "processing"
  | "shipping"
  | "delivered"
  | "cancelled";
