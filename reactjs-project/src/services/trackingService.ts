import { getAuthHeaders } from './operationsAPI';
import type { VehicleResponse } from '../types/Tracking';

const API_BASE_URL = 'http://localhost:8080';

// Custom error class for tracking operations
export class TrackingError extends Error {
  public code?: string;
  public status?: number;

  constructor(message: string, code?: string, status?: number) {
    super(message);
    this.name = 'TrackingError';
    this.code = code;
    this.status = status;
  }
}

export class TrackingService {
  private static instance: TrackingService;

  public static getInstance(): TrackingService {
    if (!TrackingService.instance) {
      TrackingService.instance = new TrackingService();
    }
    return TrackingService.instance;
  }

  /**
   * Fetch active vehicles tracking data
   * @returns Promise<VehicleResponse[]>
   */
  async getActiveVehicles(): Promise<VehicleResponse[]> {
    try {
      const response = await fetch(`${API_BASE_URL}/api/tracking/active-vehicles`, {
        method: 'GET',
        headers: {
          ...getAuthHeaders(),
        },
      });

      if (!response.ok) {
        throw new TrackingError(`Failed to fetch tracking data: ${response.status} ${response.statusText}`);
      }

      const data: VehicleResponse[] = await response.json();
      return data;
    } catch (error) {
      console.error('Error fetching active vehicles:', error);
      if (error instanceof TrackingError) {
        throw error;
      }
      throw new TrackingError('Failed to fetch tracking data');
    }
  }

  /**
   * Get tracking data for a specific vehicle
   * @param vehicleId - The ID of the vehicle
   * @returns Promise<VehicleResponse>
   */
  async getVehicleTracking(vehicleId: number): Promise<VehicleResponse> {
    try {
      const response = await fetch(`${API_BASE_URL}/api/tracking/vehicle/${vehicleId}`, {
        method: 'GET',
        headers: {
          ...getAuthHeaders(),
        },
      });

      if (!response.ok) {
        throw new TrackingError(`Failed to fetch vehicle tracking: ${response.status} ${response.statusText}`);
      }

      const data: VehicleResponse = await response.json();
      return data;
    } catch (error) {
      console.error(`Error fetching vehicle ${vehicleId} tracking:`, error);
      if (error instanceof TrackingError) {
        throw error;
      }
      throw new TrackingError(`Failed to fetch vehicle ${vehicleId} tracking`);
    }
  }

  /**
   * Update vehicle location
   * @param vehicleId - The ID of the vehicle
   * @param latitude - Vehicle latitude
   * @param longitude - Vehicle longitude
   * @param status - Vehicle status
   * @returns Promise<VehicleResponse>
   */
  async updateVehicleLocation(
    vehicleId: number, 
    latitude: number, 
    longitude: number, 
    status?: string
  ): Promise<VehicleResponse> {
    try {
      const response = await fetch(`${API_BASE_URL}/api/tracking/vehicle/${vehicleId}/location`, {
        method: 'PUT',
        headers: {
          ...getAuthHeaders(),
        },
        body: JSON.stringify({
          latitude,
          longitude,
          status,
          timestamp: new Date().toISOString(),
        }),
      });

      if (!response.ok) {
        throw new TrackingError(`Failed to update vehicle location: ${response.status} ${response.statusText}`);
      }

      const data: VehicleResponse = await response.json();
      return data;
    } catch (error) {
      console.error(`Error updating vehicle ${vehicleId} location:`, error);
      if (error instanceof TrackingError) {
        throw error;
      }
      throw new TrackingError(`Failed to update vehicle ${vehicleId} location`);
    }
  }
}

// Export singleton instance
export const trackingService = TrackingService.getInstance();
