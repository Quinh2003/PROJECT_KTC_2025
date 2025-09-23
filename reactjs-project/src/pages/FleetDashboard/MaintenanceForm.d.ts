import type { Vehicle } from "../../types/Operations";
interface ScheduleForm {
    vehicle: string;
    type: string;
    description: string;
    date: string;
    cost: string;
    nextMaintenance: string;
    statusId: string;
    notes: string;
}
interface MaintenanceFormProps {
    onAddMaintenance?: (data: ScheduleForm) => void;
    onMaintenanceCreated?: () => void;
    initialVehicle?: Vehicle;
    initialDescription?: string;
    initialType?: string;
    initialMaintenanceId?: number;
}
export default function MaintenanceForm({ onAddMaintenance, onMaintenanceCreated, initialVehicle, initialDescription, initialType, initialMaintenanceId }: MaintenanceFormProps): import("react/jsx-runtime").JSX.Element;
export {};
