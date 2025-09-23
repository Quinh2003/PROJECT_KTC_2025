import type { ReactNode } from 'react';
import type { Vehicle } from '../types/Operations';
import type { User } from '../types/User';
import type { Order } from '../types/Order';
interface OrderLegacy {
    id: string;
    status: string;
    priority: string;
    customer: string;
    to: string;
    from: string;
    driver: string;
    vehicle: string;
    date: string;
}
interface DispatcherContextType {
    orders: OrderLegacy[];
    ordersLoading: boolean;
    ordersError: string;
    refreshOrders: (force?: boolean) => Promise<void>;
    selectedOrder: Order | null;
    setSelectedOrder: (order: Order | null) => void;
    vehicles: Vehicle[];
    vehiclesLoading: boolean;
    vehiclesError: string;
    refreshVehicles: (force?: boolean) => Promise<void>;
    updateVehicleInList: (vehicleId: string | number, updates: Partial<Vehicle>) => void;
    drivers: User[];
    driversLoading: boolean;
    driversError: string;
    refreshDrivers: (force?: boolean) => Promise<void>;
}
export declare const useDispatcherContext: () => DispatcherContextType;
interface DispatcherProviderProps {
    children: ReactNode;
}
export declare const DispatcherProvider: ({ children }: DispatcherProviderProps) => import("react/jsx-runtime").JSX.Element;
export {};
