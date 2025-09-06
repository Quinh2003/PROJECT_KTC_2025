<<<<<<< HEAD
import { useState, useCallback, useMemo } from "react";
import { initialVehicles } from "./VehicleTable";
import type { FleetVehicle } from "../../types/dashboard";
=======

import { useState, useCallback, useMemo, useEffect } from "react";
import type { Vehicle as UIVehicle } from "./VehicleTable";
import * as VehicleListAPI from "../../services/VehicleListAPI";
import type { Vehicle as APIVehicle } from "../../types/Operations";

// Pagination type
interface Pagination {
  page: number;
  size: number;
  total: number;
  totalPages: number;
}

>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00

type FleetTab = "vehicles" | "maintenance" | "schedule";
type VehicleStatus = "Hoạt động" | "Bảo trì" | "Cần bảo trì";


interface FleetStats {
  total: number;
  active: number;
  maintenance: number;
  needMaintenance: number;
}



export const useFleetDashboard = () => {
  // State management
  const [tab, setTab] = useState<FleetTab>("vehicles");
<<<<<<< HEAD
  const [vehicles, setVehicles] = useState<FleetVehicle[]>(initialVehicles);
=======
  const [vehicles, setVehicles] = useState<UIVehicle[]>([]);
>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00
  const [searchTerm, setSearchTerm] = useState("");
  const [statusFilter, setStatusFilter] = useState<VehicleStatus | "all">("all");
  const [isLoading, setIsLoading] = useState(false);
  const [showAddForm, setShowAddForm] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [pagination, setPagination] = useState<Pagination>({
    page: 1,
    size: 5,
    total: 0,
    totalPages: 1,
  });

  // Fetch vehicles with pagination
  const fetchVehiclesWithPagination = useCallback((page: number, size: number) => {
    setIsLoading(true);
    setError(null);
    VehicleListAPI.fetchVehiclesRaw(page, size)
      .then(({ data, total }) => {
        const mapped: UIVehicle[] = data.map((v: APIVehicle) => ({
          id: typeof v.id === "string" ? parseInt(v.id as string) : v.id as number,
          licensePlate: v.licensePlate || "",
          type: v.vehicleType || "",
          capacityWeightKg: v.capacityWeightKg ?? undefined,
          capacityVolumeM3: v.capacityVolumeM3 ?? undefined,
          year: 2020,
          status:
            v.status && v.status.name === "ACTIVE"
              ? "Hoạt động"
              : v.status && v.status.name === "MAINTENANCE"
              ? "Bảo trì"
              : v.status && v.status.name === "NEED_MAINTENANCE"
              ? "Cần bảo trì"
              : "Hoạt động",
          lastMaintenance: "",
          nextMaintenance: "",
          driver:
            v.currentDriver && (v.currentDriver.fullName || v.currentDriver.username || v.currentDriver.email)
              ? v.currentDriver.fullName || v.currentDriver.username || v.currentDriver.email || ""
              : "",
          // mileage: 0,
        }));
        setVehicles(mapped);
        setPagination(prev => ({
          ...prev,
          page,
          size,
          total,
          totalPages: Math.ceil(total / size) || 1,
        }));
        setIsLoading(false);
      })
      .catch((err) => {
        setError("Không thể tải danh sách xe: " + err.message);
        setIsLoading(false);
      });
  }, []);

  // Fetch on mount and when page/size changes
  useEffect(() => {
    fetchVehiclesWithPagination(pagination.page, pagination.size);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [pagination.page, pagination.size]);

  // Handler to change page
  const handlePageChange = useCallback((newPage: number) => {
    setPagination(prev => ({ ...prev, page: newPage }));
  }, []);

  // Handler to change page size
  const handlePageSizeChange = useCallback((newSize: number) => {
    setPagination(prev => ({ ...prev, size: newSize, page: 1 }));
  }, []);

  // Memoized stats calculation
  const fleetStats = useMemo<FleetStats>(() => ({
    total: vehicles.length,
    active: vehicles.filter(v => v.status === "Hoạt động").length,
    maintenance: vehicles.filter(v => v.status === "Bảo trì").length,
    needMaintenance: vehicles.filter(v => v.status === "Cần bảo trì").length,
  }), [vehicles]);

  // Filtered vehicles based on search and status
  const filteredVehicles = useMemo(() => {
    return vehicles.filter(vehicle => {
      const matchesSearch = searchTerm === "" || 
  vehicle.licensePlate.toLowerCase().includes(searchTerm.toLowerCase()) ||
  vehicle.driver.toLowerCase().includes(searchTerm.toLowerCase());
      const matchesStatus = statusFilter === "all" || vehicle.status === statusFilter;
      return matchesSearch && matchesStatus;
    });
  }, [vehicles, searchTerm, statusFilter]);

<<<<<<< HEAD
  // Optimized add vehicle handler
  const handleAddVehicle = useCallback((data: Omit<FleetVehicle, "id" | "status" | "lastMaintenance" | "nextMaintenance" | "driver" | "mileage">) => {
    setIsLoading(true);
    
    // Simulate API call
    setTimeout(() => {
      const newVehicle: FleetVehicle = {
        id: Math.max(...vehicles.map(v => v.id), 0) + 1,
        licensePlate: data.licensePlate,
        type: data.type,
        brand: data.brand,
        model: data.model,
        year: Number(data.year),
        status: "Hoạt động",
        lastMaintenance: "",
        nextMaintenance: "",
        driver: "",
        mileage: 0,
      };
      
      setVehicles(prev => [...prev, newVehicle]);
      setShowAddForm(false);
      setIsLoading(false);
    }, 500);
  }, [vehicles]);
=======
  // Add vehicle handler (call API)
  const handleAddVehicle = useCallback(
    async (data: Omit<UIVehicle, "id" | "status" | "lastMaintenance" | "nextMaintenance" | "driver" | "mileage">) => {
      setIsLoading(true);
      setError(null);
      try {
        // Map UI form data to API format
        const apiData: any = {
          licensePlate: data.licensePlate,
          vehicleType: data.type,
          capacityWeightKg: data.capacityWeightKg,
          capacityVolumeM3: data.capacityVolumeM3,
        };
        const newVehicle: APIVehicle = await VehicleListAPI.addVehicle(apiData);
        // Map API vehicle to UI format
        const mapped: UIVehicle = {
          id: typeof newVehicle.id === "string" ? parseInt(newVehicle.id as string) : newVehicle.id as number,
          licensePlate: newVehicle.licensePlate || "",
          type: newVehicle.vehicleType || "",
          capacityWeightKg: newVehicle.capacityWeightKg ?? undefined,
          capacityVolumeM3: newVehicle.capacityVolumeM3 ?? undefined,
          year: 2020,
          status:
            newVehicle.status && newVehicle.status.name === "ACTIVE"
              ? "Hoạt động"
              : newVehicle.status && newVehicle.status.name === "MAINTENANCE"
              ? "Bảo trì"
              : newVehicle.status && newVehicle.status.name === "NEED_MAINTENANCE"
              ? "Cần bảo trì"
              : "Hoạt động",
          lastMaintenance: "",
          nextMaintenance: "",
          driver:
            newVehicle.currentDriver && (newVehicle.currentDriver.fullName || newVehicle.currentDriver.username || newVehicle.currentDriver.email)
              ? newVehicle.currentDriver.fullName || newVehicle.currentDriver.username || newVehicle.currentDriver.email || ""
              : "",
          // mileage: 0,
        };
        setVehicles(prev => [...prev, mapped]);
        setShowAddForm(false);
      } catch (err: any) {
        setError("Không thể thêm phương tiện: " + err.message);
      } finally {
        setIsLoading(false);
      }
    },
    []
  );
>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00

  // Handle tab changes
  const handleTabChange = useCallback((newTab: string) => {
    setTab(newTab as FleetTab);
  }, []);

  // Handle search
  const handleSearch = useCallback((term: string) => {
    setSearchTerm(term);
  }, []);

  // Handle status filter
  const handleStatusFilter = useCallback((status: VehicleStatus | "all") => {
    setStatusFilter(status);
  }, []);

  return {
    // State
    tab,
    vehicles,
    searchTerm,
    statusFilter,
    isLoading,
    showAddForm,
    setShowAddForm,
    error,
    pagination,

    // Computed values
    fleetStats,
    filteredVehicles,

    // Handlers
    handleAddVehicle,
    handleTabChange,
    handleSearch,
    handleStatusFilter,
    handlePageChange,
    handlePageSizeChange,
  };
};

// Export types for use in component
export type { FleetTab, VehicleStatus, FleetStats };
