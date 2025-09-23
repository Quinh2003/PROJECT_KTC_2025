import type { Vehicle } from "../../types/Operations";
interface VehicleDetailModalProps {
    vehicle: Vehicle;
    isOpen: boolean;
    onClose: () => void;
    onScheduleMaintenance?: (vehicle: Vehicle, emergencyRequest?: any) => void;
}
export default function VehicleDetailModal({ vehicle, isOpen, onClose, onScheduleMaintenance, }: VehicleDetailModalProps): import("react/jsx-runtime").JSX.Element | null;
export {};
