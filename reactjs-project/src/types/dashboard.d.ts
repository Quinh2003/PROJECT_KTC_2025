export interface FleetVehicle {
    id: number;
    licensePlate: string;
    type: string;
    brand: string;
    model: string;
    year: number;
    status: "Hoạt động" | "Bảo trì" | "Cần bảo trì";
    lastMaintenance: string;
    nextMaintenance: string;
    driver: string;
    mileage: number;
    capacityWeightKg?: number;
    capacityVolumeM3?: number;
}
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
    fuel: number;
    mileage: number;
    lastMaintenance: string;
    nextMaintenance: string;
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
    estimatedDeliveryTime: string;
    actualDeliveryTime?: string;
    weight: number;
    value: number;
    createdAt: string;
    updatedAt: string;
}
export interface Staff {
    id: string;
    name: string;
    email: string;
    phone: string;
    role: 'DRIVER' | 'DISPATCHER' | 'WAREHOUSE_STAFF' | 'MAINTENANCE';
    status: 'ACTIVE' | 'ON_LEAVE' | 'SICK_LEAVE' | 'TERMINATED';
    department: string;
    shiftStart: string;
    shiftEnd: string;
    performanceScore: number;
    totalDeliveries: number;
    onTimeDeliveries: number;
}
export interface SystemMetrics {
    uptime: number;
    cpuUsage: number;
    memoryUsage: number;
    diskUsage: number;
    activeConnections: number;
    requestsPerMinute: number;
    responseTime: number;
    errorRate: number;
}
export interface Alert {
    id: string;
    level: 'INFO' | 'WARNING' | 'ERROR' | 'CRITICAL';
    message: string;
    source: string;
    timestamp: string;
    acknowledged: boolean;
    resolvedAt?: string;
}
export interface User {
    id: string;
    name: string;
    email: string;
    role: string;
    roleIcon?: React.ReactNode;
    status: string;
    lastLogin: string;
}
export interface Role {
    key: string;
    name: string;
    icon?: string;
    permissions: string[];
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
    estimatedDeliveryTime: string;
    actualDeliveryTime?: string;
    weight: number;
    value: number;
    createdAt: string;
    updatedAt: string;
}
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
    fuel: number;
    mileage: number;
    lastMaintenance: string;
    nextMaintenance: string;
}
export interface Staff {
    id: string;
    name: string;
    email: string;
    phone: string;
    role: 'DRIVER' | 'DISPATCHER' | 'WAREHOUSE_STAFF' | 'MAINTENANCE';
    status: 'ACTIVE' | 'ON_LEAVE' | 'SICK_LEAVE' | 'TERMINATED';
    department: string;
    shiftStart: string;
    shiftEnd: string;
    performanceScore: number;
    totalDeliveries: number;
    onTimeDeliveries: number;
}
export interface SystemMetrics {
    uptime: number;
    cpuUsage: number;
    memoryUsage: number;
    diskUsage: number;
    activeConnections: number;
    requestsPerMinute: number;
    responseTime: number;
    errorRate: number;
}
export interface Alert {
    id: string;
    level: 'INFO' | 'WARNING' | 'ERROR' | 'CRITICAL';
    message: string;
    source: string;
    timestamp: string;
    acknowledged: boolean;
    resolvedAt?: string;
}
export interface Maintenance {
    id: number;
    vehicleId: number;
    date: string;
    description: string;
    status?: 'scheduled' | 'completed';
}
export interface Log {
    time: string;
    user: string;
    action: string;
    detail: string;
    ip: string;
    status: "success" | "error";
}
