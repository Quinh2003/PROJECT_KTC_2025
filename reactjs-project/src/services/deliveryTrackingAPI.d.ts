export interface CreateDeliveryRequest {
    orderId: number;
    vehicleId: number;
    driverId?: number;
    transportMode?: string;
    serviceType?: string;
    scheduleDeliveryTime?: string;
    deliveryNotes?: string;
}
export interface CreateDeliveryTrackingRequest {
    deliveryId: number;
    vehicleId: number;
    statusId: number;
    latitude: number;
    longitude: number;
    location?: string;
    notes?: string;
}
export interface DeliveryResponse {
    id: number;
    orderId?: number;
    deliveryFee?: number;
    transportMode: string;
    serviceType: string;
    orderDate: string;
    pickupDate?: string;
    scheduleDeliveryTime?: string;
    actualDeliveryTime?: string;
    lateDeliveryRisk?: number;
    trackingId?: number;
    routeId?: number;
    deliveryAttempts?: number;
    deliveryNotes?: string;
    driverId?: number;
    vehicleId?: number;
    createdAt?: string;
    updatedAt?: string;
}
export interface DeliveryTrackingResponse {
    id: number;
    vehicleId: number;
    statusId: number;
    latitude?: number;
    longitude?: number;
    timestamp: string;
    location?: string;
    notes?: string;
    deliveryId: number;
    createdAt: string;
    updatedAt: string;
}
/**
 * Create a delivery record when vehicle is assigned to order
 */
export declare function createDelivery(data: CreateDeliveryRequest): Promise<DeliveryResponse>;
/**
 * Create delivery tracking point
 */
export declare function createDeliveryTrackingPoint(data: CreateDeliveryTrackingRequest): Promise<DeliveryTrackingResponse>;
/**
 * Get delivery by order ID
 * Uses query parameter to filter by orderId instead of fetching all deliveries
 */
export declare function getDeliveryByOrderId(orderId: number): Promise<DeliveryResponse | null>;
/**
 * Get delivery tracking points for a delivery
 */
export declare function getDeliveryTrackingPoints(deliveryId: number): Promise<DeliveryTrackingResponse[]>;
/**
 * Get deliveries with pagination support
 */
export declare function getDeliveriesWithPagination(page?: number, size?: number, orderId?: number): Promise<{
    content: DeliveryResponse[];
    totalElements: number;
    totalPages: number;
    size: number;
    number: number;
}>;
/**
 * Helper function to get start coordinates from store
 * Returns actual coordinates from store table only - no fallback coordinates
 */
export declare function getStartCoordinatesFromStore(storeId: number): Promise<{
    latitude: number;
    longitude: number;
} | null>;
/**
 * Helper function to get coordinates from address using geocoding service
 * This is a placeholder - you might want to integrate with a real geocoding service
 */
export declare function getCoordinatesFromAddress(address: string): Promise<{
    latitude: number;
    longitude: number;
} | null>;
/**
 * Helper function to get status ID for delivery tracking
 * These should match the status table in your database
 */
export declare function getDeliveryStatusId(statusName: string): number;
export declare function updateDelivery(deliveryId: number, data: Partial<CreateDeliveryRequest>): Promise<DeliveryResponse>;
