export declare function fetchEmergencyRequestsByVehicleId(vehicleId: number | string): Promise<any[]>;
export declare function fetchMaintenanceRequestById(maintenanceId: number | string): Promise<any>;
export interface CreateVehicleMaintenanceRequest {
    vehicle: {
        id: number;
    };
    maintenanceDate: string;
    nextDueDate?: string;
    maintenanceType?: string;
    description: string;
    cost?: number;
    status?: {
        id: number;
    };
    notes?: string;
}
export declare function createVehicleMaintenance(data: CreateVehicleMaintenanceRequest): Promise<VehicleMaintenance>;
export interface VehicleMaintenance {
    id: number;
    vehicle: {
        id: number;
        licensePlate: string;
        [key: string]: any;
    };
    maintenanceDate: string;
    nextDueDate: string;
    maintenanceType: string;
    description: string;
    cost: number;
    status: {
        id: number;
        name: string;
        [key: string]: any;
    };
    createdAt: string;
    createdBy: any;
    updatedAt: string;
    notes: string;
}
export declare function fetchVehicleMaintenanceHistory(): Promise<VehicleMaintenance[]>;
export declare function fetchVehicleMaintenanceByVehicleId(vehicleId: number | string): Promise<VehicleMaintenance[]>;
