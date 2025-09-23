import { jsx as _jsx, jsxs as _jsxs, Fragment as _Fragment } from "react/jsx-runtime";
import { fetchEmergencyRequestsByVehicleId } from "../../services/VehicleMaintenanceAPI";
import React, { useState } from "react";
import { Truck, CircleDot, Wrench, AlertTriangle } from "lucide-react";
import Navbar from "../../components/Navbar";
import Sidebar from "../../components/Sidebar";
import MaintenanceHistory from "./MaintenanceHistory";
import VehicleTable from "./VehicleTable";
import AddVehicleForm from "./AddVehicleForm";
import SearchAndFilter from "./SearchAndFilter";
import Pagination from "./Pagination";
import { useFleetDashboard } from "./useFleetDashboard";
import MaintenanceForm from "./MaintenanceForm";
import MaintenanceModal from "./MaintenanceModal";
import VehicleDetailModal from "./VehicleDetailModal";
// Memoized Stats Card Component
const StatsCard = React.memo(({ title, value, icon, color, onClick, style }) => (_jsx("div", { className: `bg-white/30 hover:bg-white/40 rounded-xl p-6 shadow-lg hover:shadow-xl transition-all duration-300 border-l-4 ${color} ${onClick ? "cursor-pointer" : ""}`, onClick: onClick, style: style, children: _jsxs("div", { className: "flex items-center justify-between", children: [_jsxs("div", { children: [_jsx("p", { className: "text-sm font-medium text-gray-600 mb-1", children: title }), _jsx("p", { className: "text-3xl font-bold text-gray-900", children: value })] }), _jsx("div", { className: "flex-shrink-0", children: icon })] }) })));
StatsCard.displayName = "StatsCard";
export default function FleetDashboard({ user, onLogout, }) {
    const { 
    // State
    tab, vehicles, searchTerm, statusFilter, isLoading, showAddForm, setShowAddForm, showEditForm, editingVehicle, pagination, 
    // Computed values
    fleetStats, filteredVehicles, 
    // Handlers
    handleAddVehicle, handleTabChange, handleSearch, handleStatusFilter, handlePageChange, handleDeleteVehicle, handleEditVehicle, handleUpdateVehicle, handleCancelEdit, refreshVehicles, // Add this new handler
     } = useFleetDashboard();
    // State lưu emergency requests cho từng xe: { [vehicleId]: emergencyRequest[] }
    const [emergencyRequestsByVehicleId, setEmergencyRequestsByVehicleId] = useState({});
    // Fetch emergency requests cho tất cả xe cần bảo trì khi load dashboard
    React.useEffect(() => {
        async function fetchAllEmergencyRequests() {
            const requests = {};
            for (const v of vehicles) {
                if (v.status === "MAINTENANCE_PENDING") {
                    try {
                        const data = await fetchEmergencyRequestsByVehicleId(v.id);
                        console.log("DEBUG fetchEmergencyRequestsByVehicleId for vehicle", v.id, data);
                        requests[v.id] = data;
                    }
                    catch (e) {
                        console.log("DEBUG fetchEmergencyRequestsByVehicleId ERROR for vehicle", v.id, e);
                        requests[v.id] = [];
                    }
                }
            }
            console.log("DEBUG emergencyRequestsByVehicleId mapping:", requests);
            setEmergencyRequestsByVehicleId(requests);
        }
        if (vehicles.length > 0)
            fetchAllEmergencyRequests();
    }, [vehicles]);
    // State để lưu thông tin xe và emergency request khi đặt lịch
    const [selectedMaintenance, setSelectedMaintenance] = useState(null);
    // State for maintenance modal
    const [showMaintenanceModal, setShowMaintenanceModal] = useState(false);
    // State for vehicle detail modal
    const [showVehicleDetailModal, setShowVehicleDetailModal] = useState(false);
    const [selectedVehicle, setSelectedVehicle] = useState(null);
    // State for maintenance form modal
    const [showMaintenanceFormModal, setShowMaintenanceFormModal] = useState(false);
    function handleAddMaintenance(data) {
        // TODO: Gửi dữ liệu bảo trì lên server hoặc cập nhật state
        // Hiện tại chỉ log ra console
        console.log("Add maintenance:", data);
    }
    // Get vehicles that need maintenance
    const vehiclesNeedMaintenance = vehicles.filter((v) => v.status === "MAINTENANCE_PENDING");
    // Handler to open maintenance modal
    const handleShowMaintenanceModal = () => {
        setShowMaintenanceModal(true);
    };
    // Handler to view vehicle details
    const handleViewVehicleDetails = (vehicle) => {
        setSelectedVehicle(vehicle);
        setShowVehicleDetailModal(true);
        setShowMaintenanceModal(false); // Close maintenance modal
    };
    // Handler to schedule maintenance
    const handleScheduleMaintenance = async (vehicle, emergencyRequest) => {
        console.log("DEBUG handleScheduleMaintenance called with:", {
            vehicle: vehicle.licensePlate,
            emergencyRequest,
        });
        let finalEmergencyRequest = emergencyRequest;
        // Nếu không có emergency request từ props, fetch từ API
        if (!finalEmergencyRequest || !finalEmergencyRequest.id) {
            try {
                console.log("DEBUG Fetching emergency request for vehicle:", vehicle.id);
                const requests = await fetchEmergencyRequestsByVehicleId(vehicle.id);
                console.log("DEBUG Fetched emergency requests:", requests);
                // Tìm request có status MAINTENANCE_PENDING (id=51)
                finalEmergencyRequest = requests.find((req) => req.status?.id === 51 || req.status?.name === "MAINTENANCE_PENDING");
                console.log("DEBUG Found emergency request:", finalEmergencyRequest);
            }
            catch (error) {
                console.error("DEBUG Error fetching emergency request:", error);
                finalEmergencyRequest = null;
            }
        }
        console.log("DEBUG Setting selectedMaintenance with:", {
            vehicle: vehicle.licensePlate,
            emergencyRequestId: finalEmergencyRequest?.id,
            hasId: Boolean(finalEmergencyRequest?.id),
        });
        setSelectedMaintenance({
            vehicle,
            emergencyRequest: finalEmergencyRequest,
        });
        setShowMaintenanceFormModal(true);
        setShowMaintenanceModal(false);
    };
    return (_jsxs("div", { className: "min-h-screen flex bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100", children: [_jsx(Sidebar
            // eslint-disable-next-line @typescript-eslint/no-explicit-any
            , { 
                // eslint-disable-next-line @typescript-eslint/no-explicit-any
                activeTab: tab, 
                // eslint-disable-next-line @typescript-eslint/no-explicit-any
                onTabChange: (tab) => handleTabChange(tab), role: "fleet" }), _jsxs("main", { className: "flex-1 flex flex-col", children: [_jsx(Navbar, { user: user, onLogout: onLogout, title: "Dashboard Qu\u1EA3n l\u00FD \u0111\u1ED9i xe", subtitle: "" }), _jsxs("div", { className: "p-6 md:p-10 space-y-8", children: [tab === "vehicles" && (_jsxs(_Fragment, { children: [_jsx("div", { className: "relative mb-4", children: _jsxs("button", { onClick: refreshVehicles, className: "absolute right-0 top-0 flex items-center gap-2 px-4 py-2 bg-gradient-to-r from-violet-500 to-indigo-500 hover:from-violet-600 hover:to-indigo-600 text-white rounded-full font-semibold shadow-lg transition-all duration-200 focus:outline-none focus:ring-2 focus:ring-violet-400", title: "L\u00E0m m\u1EDBi danh s\u00E1ch ph\u01B0\u01A1ng ti\u1EC7n", children: [_jsxs("svg", { className: "animate-spin-slow", width: "20", height: "20", fill: "none", stroke: "currentColor", strokeWidth: "2", viewBox: "0 0 24 24", children: [_jsx("path", { d: "M4.93 4.93a10 10 0 1 1-1.32 2.09" }), _jsx("path", { d: "M4 4V8h4" })] }), _jsx("span", { className: "hidden md:inline", children: "L\u00E0m m\u1EDBi" })] }) }), _jsxs("div", { className: "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-6", children: [_jsx(StatsCard, { title: "T\u1ED5ng ph\u01B0\u01A1ng ti\u1EC7n", value: fleetStats.total, icon: _jsx(Truck, { size: 32, className: "text-blue-600" }), color: "border-blue-500" }), _jsx(StatsCard, { title: "\u0110ang ho\u1EA1t \u0111\u1ED9ng", value: fleetStats.inUse, icon: _jsx(CircleDot, { size: 32, className: "text-green-600" }), color: "border-green-500" }), _jsx(StatsCard, { title: "\u0110ang b\u1EA3o tr\u00EC", value: fleetStats.maintenance, icon: _jsx(Wrench, { size: 32, className: "text-yellow-600" }), color: "border-yellow-500" }), _jsx(StatsCard, { title: "C\u1EA7n b\u1EA3o tr\u00EC", value: fleetStats.needMaintenance, icon: _jsx(AlertTriangle, { size: 32, className: "text-red-600" }), color: "border-red-500", onClick: handleShowMaintenanceModal, style: { cursor: "pointer" } })] }), _jsx(SearchAndFilter, { searchTerm: searchTerm, statusFilter: statusFilter, onSearch: handleSearch, onStatusFilter: handleStatusFilter, onToggleAddForm: () => setShowAddForm(!showAddForm), showAddForm: showAddForm, resultsCount: filteredVehicles.length, totalCount: vehicles.length }), showAddForm && (_jsxs("div", { className: "bg-white rounded-xl p-6 shadow-lg border-l-4 border-violet-500", children: [_jsxs("div", { className: "flex items-center justify-between mb-6", children: [_jsxs("div", { children: [_jsx("h3", { className: "text-xl font-bold text-gray-900", children: "Th\u00EAm ph\u01B0\u01A1ng ti\u1EC7n m\u1EDBi" }), _jsx("p", { className: "text-gray-600 mt-1", children: "\u0110\u0103ng k\u00FD ph\u01B0\u01A1ng ti\u1EC7n m\u1EDBi v\u00E0o h\u1EC7 th\u1ED1ng qu\u1EA3n l\u00FD" })] }), isLoading && (_jsxs("div", { className: "flex items-center gap-2 text-violet-600", children: [_jsx("div", { className: "w-4 h-4 border-2 border-violet-600 border-t-transparent rounded-full animate-spin" }), _jsx("span", { className: "text-sm", children: "\u0110ang th\u00EAm..." })] }))] }), _jsx(AddVehicleForm, { onSuccess: handleAddVehicle })] })), showEditForm && editingVehicle && (_jsx(AddVehicleForm, { mode: "edit", editingVehicle: editingVehicle, onUpdate: handleUpdateVehicle, onCancel: handleCancelEdit })), _jsxs("div", { className: "bg-white/30 hover:bg-white/40 rounded-xl p-6 shadow-lg", children: [_jsx("div", { className: "flex items-center justify-between mb-6", children: _jsx("div", { children: _jsx("h1", { className: "text-xl font-bold text-gray-900", children: "Danh s\u00E1ch ph\u01B0\u01A1ng ti\u1EC7n" }) }) }), filteredVehicles.length === 0 ? (_jsxs("div", { className: "text-center py-12", children: [_jsx(Truck, { size: 48, className: "mx-auto text-gray-400 mb-4" }), _jsx("h4", { className: "text-lg font-medium text-gray-900 mb-2", children: searchTerm || statusFilter !== "all"
                                                            ? "Không tìm thấy phương tiện nào"
                                                            : "Chưa có phương tiện nào" }), _jsx("p", { className: "text-gray-600", children: searchTerm || statusFilter !== "all"
                                                            ? "Thử thay đổi bộ lọc hoặc từ khóa tìm kiếm"
                                                            : "Thêm phương tiện đầu tiên vào hệ thống" })] })) : (_jsxs(_Fragment, { children: [_jsx(VehicleTable, { vehicles: filteredVehicles.slice((pagination.page - 1) * pagination.size, pagination.page * pagination.size), onEdit: handleEditVehicle, onDelete: (vehicleId) => handleDeleteVehicle(Number(vehicleId)) }), _jsx(Pagination, { page: pagination.page, totalPages: pagination.totalPages, onPageChange: handlePageChange })] }))] })] })), tab === "maintenance" && (_jsx("div", { className: "animate-fadeIn", children: _jsx(MaintenanceHistory, {}) })), tab === "schedule" && (_jsx("div", { className: "animate-fadeIn", children: _jsx(MaintenanceForm, { onAddMaintenance: handleAddMaintenance, onMaintenanceCreated: refreshVehicles }) }))] })] }), _jsx(MaintenanceModal, { isOpen: showMaintenanceModal, onClose: () => setShowMaintenanceModal(false), vehicles: vehiclesNeedMaintenance, emergencyRequestsByVehicleId: emergencyRequestsByVehicleId, onViewDetails: handleViewVehicleDetails, onScheduleMaintenance: handleScheduleMaintenance }), selectedVehicle && (_jsx(VehicleDetailModal, { vehicle: selectedVehicle, isOpen: showVehicleDetailModal, onClose: () => {
                    setShowVehicleDetailModal(false);
                    setSelectedVehicle(null);
                }, onScheduleMaintenance: handleScheduleMaintenance })), showMaintenanceFormModal && selectedMaintenance && (_jsx("div", { className: "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50", children: _jsxs("div", { className: "bg-white rounded-lg p-6 w-full max-w-2xl max-h-[90vh] overflow-y-auto", children: [_jsxs("div", { className: "flex justify-between items-center mb-6", children: [_jsxs("h2", { className: "text-xl font-semibold text-gray-900", children: ["\u0110\u1EB7t l\u1ECBch b\u1EA3o tr\u00EC - ", selectedMaintenance.vehicle.licensePlate] }), _jsx("button", { onClick: () => {
                                        setShowMaintenanceFormModal(false);
                                        setSelectedMaintenance(null);
                                    }, className: "text-gray-400 hover:text-gray-600", children: "\u00D7" })] }), _jsx(MaintenanceForm, { initialVehicle: selectedMaintenance.vehicle, initialDescription: selectedMaintenance.emergencyRequest?.description || "", initialType: selectedMaintenance.emergencyRequest?.maintenanceType || "", initialMaintenanceId: selectedMaintenance.emergencyRequest?.id, onAddMaintenance: handleAddMaintenance, onMaintenanceCreated: () => {
                                console.log("DEBUG: Maintenance created/updated, closing modal and refreshing");
                                setShowMaintenanceFormModal(false);
                                setSelectedMaintenance(null);
                                refreshVehicles();
                            } })] }) }))] }));
}
