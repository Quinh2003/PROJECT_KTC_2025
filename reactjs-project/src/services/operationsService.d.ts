import type { Vehicle, Order, Route, PerformanceMetrics, OperationsSummary } from '../types/Operations';
export declare class OperationsService {
    private static instance;
    static getInstance(): OperationsService;
    getVehicles(): Promise<Vehicle[]>;
    getVehicleById(id: string): Promise<Vehicle>;
    updateVehicleStatus(id: string, status: Vehicle['status']): Promise<Vehicle>;
    getOrders(): Promise<Order[]>;
    getOrderById(id: string): Promise<Order>;
    updateOrderStatus(id: string, status: Order['status']): Promise<Order>;
    assignOrderToVehicle(orderId: string, vehicleId: string): Promise<Order>;
    getRoutes(): Promise<Route[]>;
    optimizeRoute(vehicleId: string, orderIds: string[]): Promise<Route>;
    getOperationsSummary(): Promise<OperationsSummary>;
    getPerformanceMetrics(startDate: string, endDate: string): Promise<PerformanceMetrics>;
}
export declare const operationsService: OperationsService;
