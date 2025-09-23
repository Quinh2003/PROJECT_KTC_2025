import { jsx as _jsx } from "react/jsx-runtime";
import { createContext, useContext, useState, useEffect } from 'react';
import { fetchOrdersRaw } from '../services/OrderAPI';
import { fetchVehicleStats } from '../services/VehicleListAPI';
import { fetchDrivers } from '../services/adminAPI';
const DispatcherContext = createContext(undefined);
export const useDispatcherContext = () => {
    const context = useContext(DispatcherContext);
    if (!context) {
        throw new Error('useDispatcherContext must be used within a DispatcherProvider');
    }
    return context;
};
export const DispatcherProvider = ({ children }) => {
    // Orders state
    const [orders, setOrders] = useState([]);
    const [ordersLoading, setOrdersLoading] = useState(false);
    const [ordersError, setOrdersError] = useState('');
    const [ordersLastFetch, setOrdersLastFetch] = useState(0);
    // Selected Order state
    const [selectedOrder, setSelectedOrder] = useState(null);
    // Vehicles state
    const [vehicles, setVehicles] = useState([]);
    const [vehiclesLoading, setVehiclesLoading] = useState(false);
    const [vehiclesError, setVehiclesError] = useState('');
    const [vehiclesLastFetch, setVehiclesLastFetch] = useState(0);
    // Drivers state
    const [drivers, setDrivers] = useState([]);
    const [driversLoading, setDriversLoading] = useState(false);
    const [driversError, setDriversError] = useState('');
    const [driversLastFetch, setDriversLastFetch] = useState(0);
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
            const mapped = data.data.map((item) => ({
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
        }
        catch (err) {
            setOrdersError(err.message || 'Đã xảy ra lỗi');
        }
        finally {
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
        }
        catch (err) {
            setVehiclesError(err.message || 'Đã xảy ra lỗi');
        }
        finally {
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
            const filteredDrivers = driverList.filter((u) => u.role && typeof u.role === 'object' && u.role.roleName === 'DRIVER');
            setDrivers(filteredDrivers);
            setDriversLastFetch(now);
        }
        catch (err) {
            setDriversError(err.message || 'Đã xảy ra lỗi');
        }
        finally {
            setDriversLoading(false);
        }
    };
    const updateVehicleInList = (vehicleId, updates) => {
        setVehicles(prevVehicles => prevVehicles.map(vehicle => vehicle.id === vehicleId
            ? { ...vehicle, ...updates }
            : vehicle));
    };
    // Initial load when provider mounts
    useEffect(() => {
        refreshOrders();
        refreshVehicles();
        refreshDrivers();
    }, []);
    const value = {
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
    return (_jsx(DispatcherContext.Provider, { value: value, children: children }));
};
