import React from "react";
import type { Vehicle } from "../../types/Operations";
export type FleetVehicle = Vehicle;
interface VehicleTableProps {
    vehicles: Vehicle[];
    onEdit?: (vehicle: Vehicle) => void;
    onDelete?: (vehicleId: string | number) => void;
}
declare const _default: React.NamedExoticComponent<VehicleTableProps>;
export default _default;
