export declare function fetchOrderById(orderId: string | number, token?: string): Promise<Order | null>;
import type { Order } from '../types/Order';
export interface FetchOrdersResponse {
    data: Order[];
    totalPages: number;
    totalRecords: number;
}
export declare function fetchOrders(page: number, size: number, token: string): Promise<FetchOrdersResponse>;
export declare function addOrder(order: Partial<Order>): Promise<Order>;
export declare function editOrder(id: string | number, order: Partial<Order>): Promise<Order>;
export declare function updateOrderVehicle(orderId: string | number, vehicleId: number): Promise<Order>;
export declare function deleteOrder(id: string | number): Promise<boolean>;
export declare function fetchOrdersRaw(page?: number, size?: number): Promise<{
    data: Order[];
    total: number;
}>;
export declare function fetchOrderStats(): Promise<{
    totalRecords: number;
    sampleOrders: Order[];
}>;
