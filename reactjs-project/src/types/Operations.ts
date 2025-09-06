export interface Vehicle {
  id: string;
  name: string;
  type: 'TRUCK' | 'VAN' | 'MOTORCYCLE';
  status: 'ACTIVE' | 'MAINTENANCE' | 'IDLE' | 'OUT_OF_SERVICE';
  driver?: {
    id: string;
    name: string;
    phone: string;
  };
  location: {
    lat: number;
    lng: number;
    address: string;
  };
  fuel: number; // percentage
  mileage: number;
  lastUpdated: string;
}

export interface Route {
  id: string;
  name: string;
  origin: {
    lat: number;
    lng: number;
    address: string;
  };
  destination: {
    lat: number;
    lng: number;
    address: string;
  };
  distance: number; // in km
  estimatedTime: number; // in minutes
  status: 'PLANNED' | 'IN_PROGRESS' | 'COMPLETED' | 'CANCELLED';
  assignedVehicle?: string;
  waypoints?: Array<{
    lat: number;
    lng: number;
    address: string;
    order: number;
  }>;
  createdAt: string;
  updatedAt: string;
}

export interface Order {
  id: string;
  customerName: string;
  customerPhone: string;
  pickupAddress: string;
  deliveryAddress: string;
  status: 'PENDING' | 'ASSIGNED' | 'PICKED_UP' | 'IN_TRANSIT' | 'DELIVERED' | 'CANCELLED';
  priority: 'LOW' | 'MEDIUM' | 'HIGH' | 'URGENT';
  assignedVehicle?: string;
  assignedDriver?: string;
  scheduledPickup: string;
  scheduledDelivery: string;
  actualPickup?: string;
  actualDelivery?: string;
  packageInfo: {
    weight: number; // in kg
    dimensions: {
      length: number;
      width: number;
      height: number;
    };
    fragile: boolean;
    value: number;
  };
  createdAt: string;
  updatedAt: string;
}

export interface PerformanceMetrics {
  totalDeliveries: number;
  completedDeliveries: number;
  onTimeDeliveries: number;
  avgDeliveryTime: number; // in minutes
  customerSatisfaction: number; // percentage
  fuelEfficiency: number; // km per liter
  revenue: number;
  costs: number;
  profit: number;
  period: {
    start: string;
    end: string;
  };
}

export interface OperationsSummary {
  activeVehicles: number;
  totalOrders: number;
  pendingOrders: number;
  completedToday: number;
  revenue: number;
  alerts: Array<{
    id: string;
    type: 'WARNING' | 'ERROR' | 'INFO';
    message: string;
    timestamp: string;
    resolved: boolean;
  }>;
}

export interface OperationsApiResponse<T> {
  success: boolean;
  data: T;
  message?: string;
  pagination?: {
    page: number;
    limit: number;
    total: number;
    totalPages: number;
  };
}
