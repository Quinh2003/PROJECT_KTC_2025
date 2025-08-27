import { createContext, useContext, useState, useEffect } from 'react';
import type { ReactNode } from 'react';
import { fetchOrdersRaw } from '../services/OrderAPI';
import { fetchVehicleStats } from '../services/VehicleListAPI';
import { fetchDrivers } from '../services/adminAPI';
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
  // Orders
  orders: OrderLegacy[];
  ordersLoading: boolean;
  ordersError: string;
  refreshOrders: (force?: boolean) => Promise<void>;
  
  // Selected Order for routing
  selectedOrder: Order | null;
  setSelectedOrder: (order: Order | null) => void;
  
  // Vehicles
  vehicles: Vehicle[];
  vehiclesLoading: boolean;
  vehiclesError: string;
  refreshVehicles: (force?: boolean) => Promise<void>;
  updateVehicleInList: (vehicleId: string | number, updates: Partial<Vehicle>) => void;
  
  // Drivers
  drivers: User[];
  driversLoading: boolean;
  driversError: string;
  refreshDrivers: (force?: boolean) => Promise<void>;
}

const DispatcherContext = createContext<DispatcherContextType | undefined>(undefined);

export const useDispatcherContext = () => {
  const context = useContext(DispatcherContext);
  if (!context) {
    throw new Error('useDispatcherContext must be used within a DispatcherProvider');
  }
  return context;
};

interface DispatcherProviderProps {
  children: ReactNode;
}

export const DispatcherProvider = ({ children }: DispatcherProviderProps) => {
  // Orders state
  const [orders, setOrders] = useState<OrderLegacy[]>([]);
  const [ordersLoading, setOrdersLoading] = useState(false);
  const [ordersError, setOrdersError] = useState('');
  const [ordersLastFetch, setOrdersLastFetch] = useState<number>(0);
  
  // Selected Order state
  const [selectedOrder, setSelectedOrder] = useState<Order | null>(null);
  
  // Vehicles state
  const [vehicles, setVehicles] = useState<Vehicle[]>([]);
  const [vehiclesLoading, setVehiclesLoading] = useState(false);
  const [vehiclesError, setVehiclesError] = useState('');
  const [vehiclesLastFetch, setVehiclesLastFetch] = useState<number>(0);
  
  // Drivers state
  const [drivers, setDrivers] = useState<User[]>([]);
  const [driversLoading, setDriversLoading] = useState(false);
  const [driversError, setDriversError] = useState('');
  const [driversLastFetch, setDriversLastFetch] = useState<number>(0);

  // Cache duration (5 minutes)
  const CACHE_DURATION = 5 * 60 * 1000;

  const refreshOrders = async (force = false) => {
    const now = Date.now();
    if (!force && ordersLastFetch && (now - ordersLastFetch < CACHE_DURATION)) {
      return; // Use cached data
    }

    try {
      setOrdersLoading(true);
      setOrdersError('');
      const data = await fetchOrdersRaw();
      
      const mapped = data.data.map((item: any) => ({
        id: String(item.id),
        status: item.status?.name || '',
        priority: item.priority || item.status?.statusType || '',
        customer: item.customer || item.store?.storeName || '',
        to: item.to || item.toAddress || item.address?.address || '',
        from: item.from || item.fromAddress || item.store?.address || '',
        driver: item.vehicle?.currentDriver?.fullName || item.driver || item.assignedDriver || 'Chưa phân công',
        vehicle: item.vehicle?.licensePlate || item.vehicle?.vehicleType || '',
        date: item.date || item.createdAt?.slice(0, 10) || '',
      }));
      
      setOrders(mapped);
      setOrdersLastFetch(now);
    } catch (err: any) {
      setOrdersError(err.message || 'Đã xảy ra lỗi');
    } finally {
      setOrdersLoading(false);
    }
  };

  const refreshVehicles = async (force = false) => {
    const now = Date.now();
    if (!force && vehiclesLastFetch && (now - vehiclesLastFetch < CACHE_DURATION)) {
      return; // Use cached data
    }

    try {
      setVehiclesLoading(true);
      setVehiclesError('');
      
      // Sử dụng fetchVehicleStats để lấy tất cả vehicles
      const stats = await fetchVehicleStats();
      setVehicles(stats.sampleVehicles);
      setVehiclesLastFetch(now);
    } catch (err: any) {
      setVehiclesError(err.message || 'Đã xảy ra lỗi');
    } finally {
      setVehiclesLoading(false);
    }
  };

  const refreshDrivers = async (force = false) => {
    const now = Date.now();
    if (!force && driversLastFetch && (now - driversLastFetch < CACHE_DURATION)) {
      return; // Use cached data
    }

    try {
      setDriversLoading(true);
      setDriversError('');
      const driverList = await fetchDrivers();
      const filteredDrivers = driverList.filter((u: User) => 
        u.role && typeof u.role === 'object' && u.role.roleName === 'DRIVER'
      );
      setDrivers(filteredDrivers);
      setDriversLastFetch(now);
    } catch (err: any) {
      setDriversError(err.message || 'Đã xảy ra lỗi');
    } finally {
      setDriversLoading(false);
    }
  };

  const updateVehicleInList = (vehicleId: string | number, updates: Partial<Vehicle>) => {
    setVehicles(prevVehicles =>
      prevVehicles.map(vehicle =>
        vehicle.id === vehicleId
          ? { ...vehicle, ...updates }
          : vehicle
      )
    );
  };

  // Initial load when provider mounts
  useEffect(() => {
    refreshOrders();
    refreshVehicles();
    refreshDrivers();
  }, []);

  const value: DispatcherContextType = {
    orders,
    ordersLoading,
    ordersError,
    refreshOrders,
    
    selectedOrder,
    setSelectedOrder,
    
    vehicles,
    vehiclesLoading,
    vehiclesError,
    refreshVehicles,
    updateVehicleInList,
    
    drivers,
    driversLoading,
    driversError,
    refreshDrivers,
  };

  return (
    <DispatcherContext.Provider value={value}>
      {children}
    </DispatcherContext.Provider>
  );
};