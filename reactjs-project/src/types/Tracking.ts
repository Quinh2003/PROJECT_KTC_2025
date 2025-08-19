export interface TrackingPoint {
  id: number;
  latitude: number;
  longitude: number;
  vehicleId: number;
  status: string;
  timestamp: string;
}

export interface VehicleResponse {
  vehicleId: number;
  latitude: number;
  longitude: number;
  status?: string;
  updatedAt?: string;
}

export interface TrackingApiResponse {
  success: boolean;
  data: VehicleResponse[];
  message?: string;
}

export interface TrackingError {
  message: string;
  code?: string;
  status?: number;
<<<<<<< HEAD
}
=======
}
>>>>>>> 042a7c16d89d185c6e74a32de79f098e8a6971b5
