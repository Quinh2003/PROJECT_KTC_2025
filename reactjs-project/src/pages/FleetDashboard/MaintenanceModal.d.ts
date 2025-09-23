import type { Vehicle } from "../../types/Operations";
interface MaintenanceModalProps {
    isOpen: boolean;
    onClose: () => void;
    vehicles: Vehicle[];
    emergencyRequestsByVehicleId: Record<string, any[]>;
    onViewDetails: (vehicle: Vehicle) => void;
    onScheduleMaintenance: (vehicle: Vehicle, emergencyRequest?: any) => void;
}
export default function MaintenanceModal({ isOpen, onClose, vehicles, emergencyRequestsByVehicleId, onViewDetails, onScheduleMaintenance, }: MaintenanceModalProps): import("react/jsx-runtime").JSX.Element | null;
export {};
