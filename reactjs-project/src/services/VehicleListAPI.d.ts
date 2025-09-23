import type { Vehicle } from "../types";
export declare function fetchVehicles(): Promise<Vehicle[]>;
export declare function fetchVehiclesRaw(page?: number, size?: number): Promise<{
    data: Vehicle[];
    total: number;
}>;
export declare function fetchVehicleStats(): Promise<{
    totalRecords: number;
    sampleVehicles: Vehicle[];
}>;
export declare function addVehicle(vehicle: Partial<Vehicle>): Promise<Vehicle>;
export declare function editVehicle(id: string | number, vehicle: Partial<Vehicle>): Promise<Vehicle>;
export declare function deleteVehicle(id: string | number): Promise<boolean>;
export declare function fetchVehicleById(id: string | number): Promise<Vehicle>;
export declare function updateVehicleStatus(vehicleId: string | number, status: string): Promise<Vehicle>;
export declare function assignDriverToVehicle(vehicleId: string | number, driverId: string | number | null): Promise<Vehicle>;
