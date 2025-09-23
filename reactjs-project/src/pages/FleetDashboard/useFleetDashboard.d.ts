import type { Vehicle as UIVehicle } from "../../types/Operations";
interface Pagination {
    page: number;
    size: number;
    total: number;
    totalPages: number;
}
type FleetTab = "vehicles" | "maintenance" | "schedule";
type VehicleStatus = "AVAILABLE" | "MAINTENANCE" | "IN_USE" | "MAINTENANCE_PENDING";
interface FleetStats {
    total: number;
    active: number;
    maintenance: number;
    inUse: number;
}
export declare const useFleetDashboard: () => {
    tab: FleetTab;
    vehicles: UIVehicle[];
    searchTerm: string;
    statusFilter: "all" | VehicleStatus;
    isLoading: boolean;
    showAddForm: boolean;
    setShowAddForm: import("react").Dispatch<import("react").SetStateAction<boolean>>;
    showEditForm: boolean;
    editingVehicle: UIVehicle | null;
    error: string | null;
    pagination: Pagination;
    fleetStats: FleetStats & {
        needMaintenance: number;
    };
    filteredVehicles: UIVehicle[];
    handleAddVehicle: (data: Pick<UIVehicle, "licensePlate" | "type" | "capacityWeightKg" | "capacityVolumeM3">) => Promise<void>;
    handleTabChange: (newTab: string) => void;
    handleSearch: (term: string) => void;
    handleStatusFilter: (status: VehicleStatus | "all") => void;
    handlePageChange: (newPage: number) => void;
    handlePageSizeChange: (newSize: number) => void;
    handleDeleteVehicle: (vehicleId: number) => Promise<void>;
    handleEditVehicle: (vehicle: UIVehicle) => void;
    handleUpdateVehicle: (vehicleId: number, updatedData: Partial<UIVehicle>) => Promise<void>;
    handleCancelEdit: () => void;
    refreshVehicles: () => void;
};
export type { FleetTab, VehicleStatus, FleetStats };
