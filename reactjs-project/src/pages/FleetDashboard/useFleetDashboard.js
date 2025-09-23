import { useState, useCallback, useMemo, useEffect } from "react";
import * as VehicleListAPI from "../../services/VehicleListAPI";
// Helper function to convert status to display text
const getStatusDisplay = (status) => {
    switch (status) {
        case "AVAILABLE": return "Hoạt động";
        case "MAINTENANCE": return "Bảo trì";
        case "IN_USE": return "Đang sử dụng";
        default: return "Hoạt động";
    }
};
// Helper function to get status filter display
const getStatusFilterDisplay = (status) => {
    if (status === "all")
        return "Tất cả";
    return getStatusDisplay(status);
};
export const useFleetDashboard = () => {
    // State management
    const [tab, setTab] = useState("vehicles");
    const [vehicles, setVehicles] = useState([]);
    const [searchTerm, setSearchTerm] = useState("");
    const [statusFilter, setStatusFilter] = useState("all");
    const [isLoading, setIsLoading] = useState(false);
    const [showAddForm, setShowAddForm] = useState(false);
    const [showEditForm, setShowEditForm] = useState(false);
    const [editingVehicle, setEditingVehicle] = useState(null);
    const [error, setError] = useState(null);
    const [pagination, setPagination] = useState({
        page: 1,
        size: 5,
        total: 0,
        totalPages: 1,
    });
    // Fetch all vehicles (không phân trang, size lớn)
    const fetchAllVehicles = useCallback(async () => {
        setIsLoading(true);
        setError(null);
        try {
            const { data: all, total } = await VehicleListAPI.fetchVehiclesRaw(1, 1000);
            const mapped = all.map((v) => {
                let normalizedDriver = undefined;
                if (v.driver && 'name' in v.driver) {
                    normalizedDriver = v.driver;
                }
                else if (v.currentDriver && v.currentDriver.fullName) {
                    normalizedDriver = {
                        id: String(v.currentDriver.id || ''),
                        name: v.currentDriver.fullName,
                        phone: v.currentDriver.phone || ''
                    };
                }
                // Map status_id=51 hoặc status==='MAINTENANCE_PENDING' thành MAINTENANCE_PENDING
                let status = "AVAILABLE";
                if (v.status === "MAINTENANCE_PENDING" || v.status === 51 || v.status === "51") {
                    status = "MAINTENANCE_PENDING";
                }
                else if (typeof v.status === "object" && v.status !== null && typeof v.status.name === "string") {
                    status = v.status.name;
                }
                else if (typeof v.status === "string") {
                    status = v.status;
                }
                return {
                    ...v,
                    id: typeof v.id === "string" ? parseInt(v.id) : v.id,
                    type: v.vehicleType,
                    status,
                    driver: normalizedDriver,
                    currentDriver: v.currentDriver, // Giữ nguyên currentDriver gốc
                };
            });
            setVehicles(mapped);
            setPagination(prev => ({
                ...prev,
                page: 1,
                total: mapped.length,
                totalPages: Math.ceil(mapped.length / prev.size) || 1,
            }));
        }
        catch (err) {
            setError("Không thể tải danh sách xe: " + err.message);
        }
        finally {
            setIsLoading(false);
        }
    }, []);
    // Fetch vehicles with pagination (mặc định)
    const fetchVehiclesWithPagination = useCallback((page, size) => {
        setIsLoading(true);
        setError(null);
        VehicleListAPI.fetchVehiclesRaw(page, size)
            .then(({ data, total }) => {
            const mapped = data.map((v) => {
                // Chuẩn hóa driver type
                let normalizedDriver = undefined;
                if (v.driver && 'name' in v.driver) {
                    normalizedDriver = v.driver;
                }
                else if (v.currentDriver && v.currentDriver.fullName) {
                    normalizedDriver = {
                        id: String(v.currentDriver.id || ''),
                        name: v.currentDriver.fullName,
                        phone: v.currentDriver.phone || ''
                    };
                }
                return {
                    ...v,
                    id: typeof v.id === "string" ? parseInt(v.id) : v.id,
                    type: v.vehicleType,
                    status: typeof v.status === "object" && v.status !== null && typeof v.status.name === "string"
                        ? v.status.name
                        : (typeof v.status === "string" ? v.status : "AVAILABLE"),
                    driver: normalizedDriver,
                    currentDriver: v.currentDriver, // Giữ nguyên currentDriver gốc
                };
            });
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
        // Lắng nghe sự kiện cập nhật trạng thái xe từ form bảo trì
        if (typeof window !== 'undefined' && window.addEventListener) {
            const handler = (e) => {
                if (e?.detail?.vehicleId && e?.detail?.status) {
                    setVehicles(prev => prev.map(v => v.id === e.detail.vehicleId ? { ...v, status: "MAINTENANCE" } : v));
                }
            };
            window.addEventListener('vehicleStatusChanged', handler);
            // Cleanup listener khi unmount
            return () => window.removeEventListener('vehicleStatusChanged', handler);
        }
    }, []);
    // Fetch on mount and khi searchTerm thay đổi
    // Luôn fetch toàn bộ danh sách xe khi vào dashboard để fleetStats đúng
    useEffect(() => {
        fetchAllVehicles();
        // Đăng ký hàm refresh toàn cục để VehicleList có thể gọi trực tiếp
        if (typeof window !== 'undefined') {
            window.fleetDashboardRefresh = () => {
                console.log('🔄 FleetDashboard: Refreshing vehicles from global call...');
                fetchAllVehicles();
            };
        }
        // Lắng nghe sự kiện cập nhật gán/hủy gán tài xế từ VehicleList
        if (typeof window !== 'undefined' && window.addEventListener) {
            const handler = () => {
                console.log('🔄 FleetDashboard: Refreshing vehicles from event...');
                fetchAllVehicles();
            };
            window.addEventListener('vehicleAssignmentChanged', handler);
            return () => {
                window.removeEventListener('vehicleAssignmentChanged', handler);
                // Cleanup global function
                if (window.fleetDashboardRefresh) {
                    delete window.fleetDashboardRefresh;
                }
            };
        }
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, []);
    // Nếu muốn phân trang bảng hiển thị, chỉ slice dữ liệu khi render bảng, không ảnh hưởng fleetStats
    // Handler to change page
    const handlePageChange = useCallback((newPage) => {
        setPagination(prev => ({ ...prev, page: newPage }));
    }, []);
    // Handler to change page size
    const handlePageSizeChange = useCallback((newSize) => {
        setPagination(prev => ({ ...prev, size: newSize, page: 1 }));
    }, []);
    // Memoized stats calculation
    const fleetStats = useMemo(() => ({
        total: vehicles.length,
        active: vehicles.filter(v => v.status === "AVAILABLE").length,
        maintenance: vehicles.filter(v => v.status === "MAINTENANCE").length,
        inUse: vehicles.filter(v => v.status === "IN_USE").length,
        needMaintenance: vehicles.filter(v => v.status === "MAINTENANCE_PENDING").length,
    }), [vehicles]);
    // Filtered vehicles based on search and status
    // Hàm normalize: loại bỏ mọi ký tự không phải chữ/số, chuyển về lowercase
    function normalize(str) {
        return str.replace(/[^a-zA-Z0-9]/g, "").toLowerCase();
    }
    // Khi có searchTerm, lọc toàn bộ vehicles và phân trang lại
    const filteredVehicles = useMemo(() => {
        const normalizedSearch = normalize(searchTerm);
        console.log("DEBUG filteredVehicles - searchTerm:", searchTerm, "normalized:", normalizedSearch);
        console.log("DEBUG filteredVehicles - vehicles count:", vehicles.length);
        console.log("DEBUG filteredVehicles - vehicles sample:", vehicles.slice(0, 3).map(v => ({
            id: v.id,
            licensePlate: v.licensePlate,
            normalized: normalize(v.licensePlate || "")
        })));
        let filtered = vehicles;
        if (normalizedSearch !== "" || statusFilter !== "all") {
            filtered = vehicles.filter(vehicle => {
                const normalizedPlate = vehicle.licensePlate ? normalize(vehicle.licensePlate) : "";
                const normalizedDriver = vehicle.driver && typeof vehicle.driver === 'object' && 'name' in vehicle.driver
                    ? normalize(vehicle.driver.name)
                    : "";
                const matchesSearch = normalizedSearch === "" ||
                    normalizedPlate.includes(normalizedSearch) ||
                    normalizedDriver.includes(normalizedSearch);
                const matchesStatus = statusFilter === "all" || vehicle.status === statusFilter;
                console.log("DEBUG filter check:", {
                    licensePlate: vehicle.licensePlate,
                    normalizedPlate,
                    normalizedSearch,
                    includes: normalizedPlate.includes(normalizedSearch),
                    matchesSearch,
                    matchesStatus,
                    finalMatch: matchesSearch && matchesStatus
                });
                return matchesSearch && matchesStatus;
            });
            console.log("DEBUG filteredVehicles - filtered count:", filtered.length);
        }
        return filtered;
    }, [vehicles, searchTerm, statusFilter]);
    // Add vehicle handler (call API)
    const handleAddVehicle = useCallback(async (data) => {
        setIsLoading(true);
        setError(null);
        try {
            // Map UI form data to API format
            const apiData = {
                licensePlate: data.licensePlate,
                vehicleType: data.type,
                capacityWeightKg: data.capacityWeightKg,
                capacityVolumeM3: data.capacityVolumeM3,
            };
            const newVehicle = await VehicleListAPI.addVehicle(apiData);
            // Map API vehicle to UI format
            const mapped = {
                ...newVehicle,
                id: typeof newVehicle.id === "string" ? parseInt(newVehicle.id) : newVehicle.id,
                type: newVehicle.vehicleType,
                status: typeof newVehicle.status === "object" && newVehicle.status !== null && typeof newVehicle.status.name === "string"
                    ? newVehicle.status.name
                    : (typeof newVehicle.status === "string" ? newVehicle.status : "AVAILABLE"),
                driver: newVehicle.driver || undefined,
            };
            setVehicles(prev => [...prev, mapped]);
            setShowAddForm(false);
        }
        catch (err) {
            setError("Không thể thêm phương tiện: " + err.message);
        }
        finally {
            setIsLoading(false);
        }
    }, []);
    // Handle tab changes
    const handleTabChange = useCallback((newTab) => {
        setTab(newTab);
    }, []);
    // Handle search
    const handleSearch = useCallback((term) => {
        setSearchTerm(term);
    }, []);
    // Handle status filter
    const handleStatusFilter = useCallback((status) => {
        setStatusFilter(status);
    }, []);
    // Handle delete vehicle
    const handleDeleteVehicle = useCallback(async (vehicleId) => {
        if (!window.confirm("Bạn có chắc chắn muốn xóa phương tiện này?")) {
            return;
        }
        setIsLoading(true);
        setError(null);
        try {
            await VehicleListAPI.deleteVehicle(vehicleId);
            // Remove vehicle from local state
            setVehicles(prev => prev.filter(v => v.id !== vehicleId));
            // Update pagination if needed
            const newTotal = pagination.total - 1;
            const newTotalPages = Math.ceil(newTotal / pagination.size) || 1;
            if (pagination.page > newTotalPages) {
                setPagination(prev => ({
                    ...prev,
                    page: newTotalPages,
                    total: newTotal,
                    totalPages: newTotalPages
                }));
            }
            else {
                setPagination(prev => ({
                    ...prev,
                    total: newTotal,
                    totalPages: newTotalPages
                }));
            }
        }
        catch (err) {
            setError("Không thể xóa phương tiện: " + err.message);
        }
        finally {
            setIsLoading(false);
        }
    }, [pagination]);
    // Handle edit vehicle
    const handleEditVehicle = useCallback((vehicle) => {
        setEditingVehicle(vehicle);
        setShowEditForm(true);
        setShowAddForm(false); // Đóng form thêm nếu đang mở
    }, []);
    // Handle update vehicle
    const handleUpdateVehicle = useCallback(async (vehicleId, updatedData) => {
        setIsLoading(true);
        setError(null);
        try {
            // Map UI data to API format
            const apiData = {
                licensePlate: updatedData.licensePlate,
                vehicleType: updatedData.type,
                capacityWeightKg: updatedData.capacityWeightKg,
                capacityVolumeM3: updatedData.capacityVolumeM3,
            };
            await VehicleListAPI.editVehicle(vehicleId, apiData);
            // Update vehicle in local state
            setVehicles(prev => prev.map(v => v.id === vehicleId ? { ...v, ...updatedData } : v));
            // Close edit form
            setShowEditForm(false);
            setEditingVehicle(null);
        }
        catch (err) {
            setError("Không thể cập nhật phương tiện: " + err.message);
        }
        finally {
            setIsLoading(false);
        }
    }, []);
    // Handle cancel edit
    const handleCancelEdit = useCallback(() => {
        setShowEditForm(false);
        setEditingVehicle(null);
    }, []);
    // Handle refresh vehicles (to be called after maintenance operations)
    const refreshVehicles = useCallback(() => {
        fetchAllVehicles(); // Gọi fetchAllVehicles để cập nhật đúng fleetStats
    }, [fetchAllVehicles]);
    return {
        // State
        tab,
        vehicles,
        searchTerm,
        statusFilter,
        isLoading,
        showAddForm,
        setShowAddForm,
        showEditForm,
        editingVehicle,
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
        handleDeleteVehicle,
        handleEditVehicle,
        handleUpdateVehicle,
        handleCancelEdit,
        refreshVehicles, // Add this new handler
    };
};
