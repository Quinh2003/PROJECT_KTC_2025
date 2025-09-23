import type { VehicleResponse } from '../types/Tracking';
export declare class TrackingError extends Error {
    code?: string;
    status?: number;
    constructor(message: string, code?: string, status?: number);
}
export declare class TrackingService {
    private static instance;
    static getInstance(): TrackingService;
    /**
     * Fetch active vehicles tracking data
     * @returns Promise<VehicleResponse[]>
     */
    getActiveVehicles(): Promise<VehicleResponse[]>;
    /**
     * Get tracking data for a specific vehicle
     * @param vehicleId - The ID of the vehicle
     * @returns Promise<VehicleResponse>
     */
    getVehicleTracking(vehicleId: number): Promise<VehicleResponse>;
    /**
     * Update vehicle location
     * @param vehicleId - The ID of the vehicle
     * @param latitude - Vehicle latitude
     * @param longitude - Vehicle longitude
     * @param status - Vehicle status
     * @returns Promise<VehicleResponse>
     */
    updateVehicleLocation(vehicleId: number, latitude: number, longitude: number, status?: string): Promise<VehicleResponse>;
}
export declare const trackingService: TrackingService;
