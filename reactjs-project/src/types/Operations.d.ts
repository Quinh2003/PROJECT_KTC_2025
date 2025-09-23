export interface Vehicle {
    id: string | number;
    name?: string;
    licensePlate: string;
    vehicleType: string;
    type?: 'TRUCK' | 'VAN' | 'MOTORCYCLE';
    brand?: string;
    model?: string;
    capacityWeightKg?: number;
    capacityVolumeM3?: number;
    year?: number;
    status: 'AVAILABLE' | 'IN_USE' | 'MAINTENANCE' | 'MAINTENANCE_PENDING';
    lastMaintenance?: string;
    nextMaintenance?: string;
    currentDriver?: {
        id?: string | number;
        fullName?: string;
        email?: string;
        phone?: string;
    };
    driver?: {
        id: string;
        name: string;
        phone: string;
    };
    notes?: string;
    location?: {
        lat: number;
        lng: number;
        address: string;
    };
    fuel?: number;
    mileage?: number;
    updatedAt: string;
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
    distance: number;
    estimatedTime: number;
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
        weight: number;
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
    avgDeliveryTime: number;
    customerSatisfaction: number;
    fuelEfficiency: number;
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
