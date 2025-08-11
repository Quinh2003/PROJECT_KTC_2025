import { useState, useCallback, useMemo } from "react";
import { initialVehicles } from "./VehicleTable";
import type { FleetVehicle } from "../../types/dashboard";

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
  const [vehicles, setVehicles] = useState<FleetVehicle[]>(initialVehicles);
  const [searchTerm, setSearchTerm] = useState("");
  const [statusFilter, setStatusFilter] = useState<VehicleStatus | "all">("all");
  const [isLoading, setIsLoading] = useState(false);
  const [showAddForm, setShowAddForm] = useState(false);

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
        vehicle.brand.toLowerCase().includes(searchTerm.toLowerCase()) ||
        vehicle.model.toLowerCase().includes(searchTerm.toLowerCase()) ||
        vehicle.driver.toLowerCase().includes(searchTerm.toLowerCase());
      
      const matchesStatus = statusFilter === "all" || vehicle.status === statusFilter;
      
      return matchesSearch && matchesStatus;
    });
  }, [vehicles, searchTerm, statusFilter]);

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
    
    // Computed values
    fleetStats,
    filteredVehicles,
    
    // Handlers
    handleAddVehicle,
    handleTabChange,
    handleSearch,
    handleStatusFilter,
  };
};

// Export types for use in component
export type { FleetTab, VehicleStatus, FleetStats };
