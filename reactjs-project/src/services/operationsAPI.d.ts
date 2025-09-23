import type { Vehicle } from "../types/dashboard";
export declare const API_BASE_URL = "http://localhost:8080/api";
export declare const getAuthHeaders: () => {
    Authorization: string;
    'Content-Type': string;
};
export declare const operationsAPI: {
    getOverviewStats: () => Promise<OperationsSummary>;
    getVehicles: () => Promise<Vehicle[]>;
    updateVehicleStatus: (vehicleId: string, status: Vehicle["status"]) => Promise<Vehicle>;
    getOrders: (params?: {
        status?: string;
        priority?: string;
        limit?: number;
    }) => Promise<Order[]>;
    assignOrder: (orderId: string, vehicleId: string, driverId: string) => Promise<Order>;
    getStaff: (department?: string) => Promise<any[]>;
    updateStaffStatus: (staffId: string, status: string) => Promise<any>;
    getPerformanceMetrics: (timeRange: string) => Promise<PerformanceMetrics>;
    getSystemMetrics: () => Promise<any>;
    getAlerts: (acknowledged?: boolean) => Promise<any[]>;
    acknowledgeAlert: (alertId: string) => Promise<any>;
    generateReport: (type: string, params: Record<string, unknown>) => Promise<any>;
};
