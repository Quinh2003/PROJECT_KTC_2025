import type { Vehicle } from "./VehicleTable";
interface VehicleFormData {
    licensePlate: string;
    type: string;
    capacityWeightKg: string;
    capacityVolumeM3: string;
    notes: string;
}
type AddVehicleFormProps = {
    onSuccess?: (data: Pick<Vehicle, "licensePlate" | "type" | "capacityWeightKg" | "capacityVolumeM3">) => void | Promise<void>;
    onCancel?: () => void;
    initialValues?: Partial<VehicleFormData>;
    mode?: "add" | "edit";
    editingVehicle?: Vehicle;
    onUpdate?: (vehicleId: number, data: Partial<Vehicle>) => Promise<void>;
};
export default function AddVehicleForm({ onSuccess, onCancel, initialValues, mode, editingVehicle, onUpdate }: AddVehicleFormProps): import("react/jsx-runtime").JSX.Element;
export {};
