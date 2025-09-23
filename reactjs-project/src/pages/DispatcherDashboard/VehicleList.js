import { jsx as _jsx, jsxs as _jsxs, Fragment as _Fragment } from "react/jsx-runtime";
import { useEffect, useState } from "react";
import { fetchVehiclesRaw, assignDriverToVehicle, updateVehicleStatus } from "../../services/VehicleListAPI";
import { useDispatcherContext } from "../../contexts/DispatcherContext";
import { useQueryClient } from "@tanstack/react-query";
export default function VehicleList() {
    const { drivers, driversLoading, refreshDrivers, refreshVehicles: refreshContextVehicles } = useDispatcherContext();
    const queryClient = useQueryClient();
    // Compute status logic: MAINTENANCE > IN_USE (if has driver) > AVAILABLE
    // Map numeric status to string
    const statusMap = {
        17: 'AVAILABLE',
        18: 'IN_USE',
        19: 'MAINTENANCE',
        51: 'MAINTENANCE_PENDING',
        'AVAILABLE': 'AVAILABLE',
        'IN_USE': 'IN_USE',
        'MAINTENANCE': 'MAINTENANCE',
        'MAINTENANCE_PENDING': 'MAINTENANCE_PENDING',
    };
    const getComputedStatus = (vehicle) => {
        // Ưu tiên trạng thái cần bảo trì
        if (vehicle.status === 'MAINTENANCE_PENDING' ||
            vehicle.status === 51 ||
            vehicle.status === '51' ||
            (typeof vehicle.status === 'object' && vehicle.status !== null && vehicle.status.id === 51) ||
            (typeof vehicle.status === 'object' && vehicle.status !== null && vehicle.status.name === 'MAINTENANCE_PENDING')) {
            return 'MAINTENANCE_PENDING';
        }
        if (vehicle.status !== undefined && vehicle.status !== null) {
            const mapped = statusMap[vehicle.status];
            if (mapped)
                return mapped;
        }
        const today = new Date();
        if (vehicle.lastMaintenance && vehicle.nextMaintenance) {
            const maintenanceStart = new Date(vehicle.lastMaintenance);
            const maintenanceEnd = new Date(vehicle.nextMaintenance);
            if (today >= maintenanceStart && today <= maintenanceEnd) {
                return 'MAINTENANCE';
            }
            else if (vehicle.driver?.name || vehicle.currentDriver?.fullName) {
                return 'IN_USE';
            }
            else {
                return 'AVAILABLE';
            }
        }
        else if (vehicle.driver?.name || vehicle.currentDriver?.fullName) {
            return 'IN_USE';
        }
        return 'AVAILABLE';
    };
    // State hooks
    const [showAssignModal, setShowAssignModal] = useState(false);
    const [selectedVehicle, setSelectedVehicle] = useState(null);
    const [selectedDriverId, setSelectedDriverId] = useState(null);
    const [assignError, setAssignError] = useState("");
    const [assignSuccess, setAssignSuccess] = useState("");
    const [assigning, setAssigning] = useState(false);
    const [searchTerm, setSearchTerm] = useState("");
    const [statusFilter, setStatusFilter] = useState("all");
    const [updatingStatus, setUpdatingStatus] = useState(null);
    // Pagination and vehicles
    const [currentPage, setCurrentPage] = useState(1);
    const [itemsPerPage, setItemsPerPage] = useState(5);
    const [vehicles, setVehicles] = useState([]);
    const [totalVehicles, setTotalVehicles] = useState(0);
    const [vehiclesLoading, setVehiclesLoading] = useState(false);
    const [vehiclesError, setVehiclesError] = useState("");
    // Fetch vehicles with pagination
    const fetchVehicles = async (page = 1, size = 5) => {
        setVehiclesLoading(true);
        setVehiclesError("");
        try {
            const res = await fetchVehiclesRaw(page, size);
            setVehicles(res.data);
            setTotalVehicles(res.total);
        }
        catch (err) {
            setVehicles([]);
            setTotalVehicles(0);
            setVehiclesError(err.message || "Đã xảy ra lỗi khi tải phương tiện");
        }
        finally {
            setVehiclesLoading(false);
        }
    };
    useEffect(() => {
        fetchVehicles(currentPage, itemsPerPage);
        // eslint-disable-next-line react-hooks/exhaustive-deps
    }, [currentPage, itemsPerPage]);
    // Assign driver handler (fixed placement)
    const handleAssignDriver = async () => {
        if (!selectedVehicle)
            return;
        setAssigning(true);
        setAssignError("");
        setAssignSuccess("");
        try {
            if (!selectedDriverId) {
                // Nếu chọn 'Chưa gán tài xế', gửi driverId là null
                await assignDriverToVehicle(selectedVehicle.id, null);
                // Nếu bỏ gán tài xế, chuyển trạng thái về AVAILABLE
                await updateVehicleStatus(selectedVehicle.id, "AVAILABLE");
                setAssignSuccess("Đã bỏ gán tài xế!");
            }
            else {
                const driverObj = drivers.find(d => String(d.id) === String(selectedDriverId));
                if (!driverObj)
                    throw new Error("Không tìm thấy tài xế");
                await assignDriverToVehicle(selectedVehicle.id, driverObj.id ?? "");
                // Sau khi gán tài xế, chuyển trạng thái xe sang IN_USE
                await updateVehicleStatus(selectedVehicle.id, "IN_USE");
                setAssignSuccess("Gán tài xế thành công!");
            }
            // Refresh vehicles after assignment (local, context, và dashboard fleet)
            fetchVehicles(currentPage, itemsPerPage);
            queryClient.refetchQueries({ queryKey: ['vehicles'] });
            queryClient.refetchQueries({ queryKey: ['ordersForList'] });
            refreshContextVehicles(true);
            // Gọi thêm hàm cập nhật dashboard fleet nếu có (với delay để backend cập nhật)
            setTimeout(() => {
                if (window && typeof window.dispatchEvent === 'function') {
                    window.dispatchEvent(new CustomEvent('vehicleAssignmentChanged'));
                }
                // Gọi trực tiếp refreshVehicles của FleetDashboard nếu có
                if (window && window.fleetDashboardRefresh) {
                    window.fleetDashboardRefresh();
                }
            }, 500); // Delay 500ms để backend cập nhật
            setTimeout(() => {
                closeAssignModal();
            }, 1000);
        }
        catch (err) {
            setAssignError(err.message || "Gán tài xế thất bại");
        }
        finally {
            setAssigning(false);
        }
    };
    const openAssignModal = async (vehicle) => {
        setSelectedVehicle(vehicle);
        setShowAssignModal(true);
        setAssignError("");
        setSelectedDriverId(null);
        // Refresh drivers if needed
        if (drivers.length === 0) {
            await refreshDrivers(true);
        }
    };
    const closeAssignModal = () => {
        setShowAssignModal(false);
        setSelectedVehicle(null);
        setSelectedDriverId(null);
        setAssignError("");
        setAssignSuccess("");
    };
    const toggleVehicleStatus = async (vehicleId, currentStatus) => {
        try {
            setUpdatingStatus(vehicleId);
            // Chỉ chuyển đổi giữa AVAILABLE <-> IN_USE
            let newStatus = "AVAILABLE";
            if (currentStatus?.name === "AVAILABLE") {
                newStatus = "IN_USE";
            }
            else if (currentStatus?.name === "IN_USE") {
                newStatus = "AVAILABLE";
            }
            else if (currentStatus?.name === "MAINTENANCE") {
                // Nếu đang bảo trì, chuyển về AVAILABLE (hoặc có thể bỏ qua tuỳ nghiệp vụ)
                newStatus = "AVAILABLE";
            }
            await updateVehicleStatus(vehicleId, newStatus);
            // Refresh vehicles after status update
            fetchVehicles(currentPage, itemsPerPage);
            // Force refetch React Query cache để các component khác cũng cập nhật ngay lập tức
            queryClient.refetchQueries({ queryKey: ['vehicles'] });
            queryClient.refetchQueries({ queryKey: ['ordersForList'] }); // Cập nhật OrderList
            // Cập nhật Context vehicles
            refreshContextVehicles(true);
        }
        catch (err) {
            console.error("Failed to update vehicle status:", err);
            alert(`Lỗi cập nhật trạng thái xe: ${err.message}`);
        }
        finally {
            setUpdatingStatus(null);
        }
    };
    // Local search/filter (client-side, for now)
    const filteredVehicles = vehicles.filter(vehicle => {
        const computedStatus = getComputedStatus(vehicle);
        const matchesSearch = vehicle.licensePlate.toLowerCase().includes(searchTerm.toLowerCase()) ||
            vehicle.vehicleType.toLowerCase().includes(searchTerm.toLowerCase()) ||
            (vehicle.currentDriver?.fullName || "").toLowerCase().includes(searchTerm.toLowerCase());
        const matchesStatus = statusFilter === "all" ||
            (statusFilter === "available" && computedStatus === "AVAILABLE") ||
            (statusFilter === "assigned" && computedStatus === "IN_USE") ||
            (statusFilter === "unassigned" && computedStatus !== "IN_USE");
        return matchesSearch && matchesStatus;
    });
    const totalPages = Math.ceil(totalVehicles / itemsPerPage);
    const currentVehicles = filteredVehicles;
    // Reset to first page when filters change
    const handleSearchChange = (term) => {
        setSearchTerm(term);
        setCurrentPage(1);
    };
    const handleStatusFilterChange = (status) => {
        setStatusFilter(status);
        setCurrentPage(1);
    };
    // Compute status logic: MAINTENANCE > IN_USE (if has driver) > AVAILABLE
    const getStatusBadge = (vehicle) => {
        const computedStatus = getComputedStatus(vehicle);
        const isUpdating = updatingStatus === vehicle.id;
        let badgeProps = {
            color: '',
            text: '',
            border: '',
            bg: '',
            icon: '',
        };
        switch (computedStatus) {
            case 'MAINTENANCE_PENDING':
                badgeProps = {
                    color: 'red-800',
                    text: 'Cần bảo trì',
                    border: 'red-200',
                    bg: 'red-100',
                    icon: 'red-500',
                };
                break;
            case 'AVAILABLE':
                badgeProps = {
                    color: 'green-800',
                    text: 'Sẵn sàng',
                    border: 'green-200',
                    bg: 'green-100',
                    icon: 'green-500',
                };
                break;
            case 'IN_USE':
                badgeProps = {
                    color: 'red-800',
                    text: 'Đang sử dụng',
                    border: 'red-200',
                    bg: 'red-100',
                    icon: 'red-500',
                };
                break;
            case 'MAINTENANCE':
                badgeProps = {
                    color: 'gray-800',
                    text: 'Bảo trì',
                    border: 'gray-200',
                    bg: 'gray-100',
                    icon: 'gray-500',
                };
                break;
            default:
                badgeProps = {
                    color: 'gray-800',
                    text: 'Không xác định',
                    border: 'gray-200',
                    bg: 'gray-100',
                    icon: 'gray-500',
                };
        }
        return (_jsxs("button", { onClick: () => toggleVehicleStatus(vehicle.id, computedStatus), disabled: isUpdating, className: `inline-flex items-center gap-1 px-3 py-1 rounded-full text-xs font-semibold bg-${badgeProps.bg} text-${badgeProps.color} border border-${badgeProps.border} hover:bg-${badgeProps.bg.replace('100', '200')} hover:scale-105 transition-all duration-200 disabled:opacity-50 disabled:cursor-not-allowed`, children: [isUpdating ? (_jsx("div", { className: "animate-spin w-2 h-2 border border-current border-t-transparent rounded-full" })) : (_jsx("div", { className: `w-2 h-2 bg-${badgeProps.icon} rounded-full` })), badgeProps.text] }));
    };
    return (_jsxs("div", { className: "space-y-6", children: [_jsx("div", { className: "bg-gradient-to-r from-blue-600 to-indigo-700 rounded-2xl p-6 text-white shadow-lg", children: _jsx("div", { className: "flex flex-col md:flex-row md:items-center md:justify-between gap-4", children: _jsx("div", { children: _jsx("h2", { className: "text-2xl font-bold mb-2", children: "Qu\u1EA3n l\u00FD ph\u01B0\u01A1ng ti\u1EC7n" }) }) }) }), _jsx("div", { className: "bg-white/80 backdrop-blur-sm rounded-xl p-6 shadow-md border border-white/50", children: _jsxs("div", { className: "flex flex-col md:flex-row gap-4 items-center", children: [_jsxs("div", { className: "relative flex-1", children: [_jsx("div", { className: "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none", children: _jsx("svg", { className: "h-5 w-5 text-gray-400", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" }) }) }), _jsx("input", { type: "text", placeholder: "T\u00ECm ki\u1EBFm theo bi\u1EC3n s\u1ED1, lo\u1EA1i xe ho\u1EB7c t\u00E0i x\u1EBF...", value: searchTerm, onChange: (e) => handleSearchChange(e.target.value), className: "w-full pl-10 pr-4 py-3 border border-gray-200 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent bg-white/90 backdrop-blur-sm" })] }), _jsxs("div", { className: "flex items-center gap-2", children: [_jsx("label", { className: "text-sm font-medium text-gray-700", children: "Tr\u1EA1ng th\u00E1i:" }), _jsxs("select", { value: statusFilter, onChange: (e) => handleStatusFilterChange(e.target.value), className: "px-3 py-2 border border-gray-200 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent bg-white/90 backdrop-blur-sm text-sm", children: [_jsx("option", { value: "all", children: "T\u1EA5t c\u1EA3" }), _jsx("option", { value: "available", children: "S\u1EB5n s\u00E0ng" }), _jsx("option", { value: "assigned", children: "\u0110ang s\u1EED d\u1EE5ng" }), _jsx("option", { value: "unassigned", children: "Ch\u01B0a g\u00E1n" })] })] })] }) }), _jsx("div", { className: "bg-white/80 backdrop-blur-sm rounded-xl shadow-md border border-white/50", children: vehiclesLoading ? (_jsx("div", { className: "flex items-center justify-center p-12", children: _jsxs("div", { className: "flex items-center gap-3", children: [_jsx("div", { className: "animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600" }), _jsx("span", { className: "text-gray-600 font-medium", children: "\u0110ang t\u1EA3i ph\u01B0\u01A1ng ti\u1EC7n..." })] }) })) : vehiclesError ? (_jsx("div", { className: "flex items-center justify-center p-12", children: _jsxs("div", { className: "text-center", children: [_jsx("div", { className: "w-12 h-12 bg-red-100 rounded-full flex items-center justify-center mx-auto mb-4", children: _jsx("svg", { className: "w-6 h-6 text-red-600", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" }) }) }), _jsx("h3", { className: "text-lg font-semibold text-gray-900 mb-2", children: "C\u00F3 l\u1ED7i x\u1EA3y ra" }), _jsx("p", { className: "text-red-600", children: vehiclesError })] }) })) : currentVehicles.length === 0 ? (_jsx("div", { className: "flex items-center justify-center p-12", children: _jsxs("div", { className: "text-center", children: [_jsx("div", { className: "w-12 h-12 bg-gray-100 rounded-full flex items-center justify-center mx-auto mb-4", children: _jsxs("svg", { className: "w-6 h-6 text-gray-400", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: [_jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M9 17a2 2 0 11-4 0 2 2 0 014 0zM19 17a2 2 0 11-4 0 2 2 0 014 0z" }), _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M13 16V6a1 1 0 00-1-1H4a1 1 0 00-1 1v10a1 1 0 001 1h1m8-1a1 1 0 01-1 1H9m4-1V8a1 1 0 011-1h2.586a1 1 0 01.707.293l2.414 2.414a1 1 0 01.293.707V16a1 1 0 01-1 1h-1m-6-1a1 1 0 001 1h1M5 17a2 2 0 104 0M15 17a2 2 0 104 0" })] }) }), _jsx("h3", { className: "text-lg font-semibold text-gray-900 mb-2", children: "Kh\u00F4ng t\u00ECm th\u1EA5y ph\u01B0\u01A1ng ti\u1EC7n" }), _jsx("p", { className: "text-gray-500", children: searchTerm || statusFilter !== "all" ? "Không có phương tiện nào phù hợp với bộ lọc" : "Chưa có phương tiện nào trong hệ thống" })] }) })) : (_jsxs(_Fragment, { children: [_jsx("div", { className: "space-y-4 p-6", children: currentVehicles.map((vehicle) => (_jsxs("div", { className: "bg-white/90 backdrop-blur-sm rounded-xl border border-gray-200 shadow-sm hover:shadow-md transition-all duration-300 overflow-hidden", children: [_jsxs("div", { className: "flex items-center p-4 gap-4", children: [_jsxs("div", { className: "flex items-center gap-4 flex-1", children: [_jsx("div", { className: "w-12 h-12 bg-gradient-to-r from-blue-500 to-indigo-600 rounded-lg flex items-center justify-center flex-shrink-0", children: _jsxs("svg", { className: "w-6 h-6 text-white", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: [_jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M9 17a2 2 0 11-4 0 2 2 0 014 0zM19 17a2 2 0 11-4 0 2 2 0 014 0z" }), _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M13 16V6a1 1 0 00-1-1H4a1 1 0 00-1 1v10a1 1 0 001 1h1m8-1a1 1 0 01-1 1H9m4-1V8a1 1 0 011-1h2.586a1 1 0 01.707.293l2.414 2.414a1 1 0 01.293.707V16a1 1 0 01-1 1h-1m-6-1a1 1 0 001 1h1M5 17a2 2 0 104 0M15 17a2 2 0 104 0" })] }) }), _jsxs("div", { className: "min-w-0", children: [_jsx("h3", { className: "font-bold text-gray-900 text-lg", children: vehicle.licensePlate }), _jsx("p", { className: "text-sm text-gray-600 uppercase tracking-wide", children: vehicle.vehicleType })] })] }), _jsx("div", { className: "flex-shrink-0", children: getStatusBadge(vehicle) })] }), _jsx("div", { className: "px-4 pb-2", children: _jsxs("div", { className: "flex items-center gap-3", children: [_jsx("div", { className: "w-6 h-6 bg-blue-100 rounded-full flex items-center justify-center flex-shrink-0", children: _jsx("svg", { className: "w-3 h-3 text-blue-600", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M16 7a4 4 0 11-8 0 4 4 0 018 0zM12 14a7 7 0 00-7 7h14a7 7 0 00-7-7z" }) }) }), _jsxs("div", { className: "flex-1 min-w-0", children: [_jsx("span", { className: "text-sm text-gray-500", children: "T\u00E0i x\u1EBF: " }), vehicle.currentDriver ? (_jsx("span", { className: "font-medium text-gray-900", children: vehicle.currentDriver.fullName || vehicle.currentDriver.email })) : (_jsx("span", { className: "text-gray-400 italic", children: "Ch\u01B0a c\u00F3 t\u00E0i x\u1EBF" }))] })] }) }), _jsx("div", { className: "px-4 pb-2", children: _jsxs("div", { className: "flex gap-8 text-sm", children: [_jsxs("div", { children: [_jsx("span", { className: "text-gray-500", children: "Tr\u1ECDng t\u1EA3i: " }), _jsxs("span", { className: "font-semibold text-gray-900", children: [vehicle.capacityWeightKg || "-", " t\u1EA5n"] })] }), _jsxs("div", { children: [_jsx("span", { className: "text-gray-500", children: "Th\u1EC3 t\u00EDch: " }), _jsxs("span", { className: "font-semibold text-gray-900", children: [vehicle.capacityVolumeM3 || "-", " m\u00B3"] })] })] }) }), vehicle.notes && (_jsx("div", { className: "px-4 pb-2", children: _jsxs("div", { className: "bg-yellow-50 border border-yellow-200 rounded-lg p-2", children: [_jsx("span", { className: "text-xs text-yellow-700", children: "Ghi ch\u00FA: " }), _jsx("span", { className: "text-sm text-yellow-800", children: vehicle.notes })] }) })), _jsxs("div", { className: "px-4 py-3 bg-gray-50 border-t border-gray-200 flex items-center justify-between", children: [_jsxs("span", { className: "text-xs text-gray-400", children: ["C\u1EADp nh\u1EADt: ", vehicle.updatedAt ? new Date(vehicle.updatedAt).toLocaleString('vi-VN') : "-"] }), _jsx("button", { onClick: () => openAssignModal(vehicle), className: "bg-gradient-to-r from-blue-500 to-indigo-600 hover:from-blue-600 hover:to-indigo-700 text-white font-semibold py-2 px-4 rounded-lg transition-all duration-200 shadow-sm hover:shadow-md text-sm", children: "G\u00E1n t\u00E0i x\u1EBF" })] })] }, vehicle.id))) }), totalPages > 1 && (_jsx("div", { className: "px-6 py-4 bg-gray-50 border-t border-gray-200", children: _jsx("div", { className: "flex items-center justify-between", children: _jsxs("div", { className: "flex items-center gap-2", children: [_jsx("button", { onClick: () => setCurrentPage(prev => Math.max(prev - 1, 1)), disabled: currentPage === 1, className: "px-3 py-2 bg-white border border-gray-300 rounded-lg hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed transition-colors duration-200", children: _jsx("svg", { className: "w-4 h-4", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M15 19l-7-7 7-7" }) }) }), _jsxs("span", { className: "text-sm text-gray-600 mx-2", children: ["Trang ", currentPage, " / ", totalPages, " (T\u1ED5ng: ", totalVehicles, ")"] }), _jsx("button", { onClick: () => setCurrentPage(prev => Math.min(prev + 1, totalPages)), disabled: currentPage === totalPages, className: "px-3 py-2 bg-white border border-gray-300 rounded-lg hover:bg-gray-50 disabled:opacity-50 disabled:cursor-not-allowed transition-colors duration-200", children: _jsx("svg", { className: "w-4 h-4", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M9 5l7 7-7 7" }) }) })] }) }) }))] })) }), showAssignModal && (_jsx("div", { className: "fixed inset-0 bg-black/50 backdrop-blur-sm flex items-center justify-center z-50 p-4", children: _jsxs("div", { className: "bg-white rounded-2xl shadow-2xl w-full max-w-md transform transition-all duration-300 scale-100", children: [_jsxs("div", { className: "bg-gradient-to-r from-blue-500 to-indigo-600 text-white p-6 rounded-t-2xl", children: [_jsxs("div", { className: "flex items-center justify-between", children: [_jsx("h2", { className: "text-xl font-bold", children: "G\u00E1n t\u00E0i x\u1EBF cho xe" }), _jsx("button", { onClick: closeAssignModal, className: "w-8 h-8 bg-white/20 hover:bg-white/30 rounded-full flex items-center justify-center transition-colors duration-200", children: _jsx("svg", { className: "w-4 h-4", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M6 18L18 6M6 6l12 12" }) }) })] }), _jsxs("p", { className: "text-blue-100 mt-2", children: ["Bi\u1EC3n s\u1ED1: ", _jsx("span", { className: "font-semibold", children: selectedVehicle?.licensePlate })] })] }), _jsx("div", { className: "p-6", children: driversLoading ? (_jsx("div", { className: "flex items-center justify-center py-8", children: _jsx("div", { className: "flex items-center gap-3", children: _jsx("div", { className: "animate-spin rounded-full h-6 w-6 border-b-2 border-blue-600" }) }) })) : (_jsxs(_Fragment, { children: [assignError && (_jsx("div", { className: "mb-4 p-4 bg-red-50 border border-red-200 rounded-lg", children: _jsxs("div", { className: "flex items-center gap-2", children: [_jsx("svg", { className: "w-5 h-5 text-red-600", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" }) }), _jsx("span", { className: "text-red-800 font-medium", children: assignError })] }) })), assignSuccess && (_jsx("div", { className: "mb-4 p-4 bg-green-50 border border-green-200 rounded-lg", children: _jsxs("div", { className: "flex items-center gap-2", children: [_jsx("svg", { className: "w-5 h-5 text-green-600", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M5 13l4 4L19 7" }) }), _jsx("span", { className: "text-green-800 font-medium", children: assignSuccess })] }) })), _jsxs("div", { className: "space-y-4", children: [_jsx("label", { className: "block text-sm font-medium text-gray-700", children: "Ch\u1ECDn t\u00E0i x\u1EBF" }), _jsxs("select", { value: selectedDriverId ?? "", onChange: (e) => setSelectedDriverId(e.target.value), disabled: assigning, className: "w-full px-4 py-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent bg-white disabled:bg-gray-50 disabled:cursor-not-allowed", children: [_jsx("option", { value: "", children: "-- Ch\u01B0a g\u00E1n t\u00E0i x\u1EBF --" }), drivers.filter(driver => {
                                                        // Lọc theo toàn bộ danh sách xe từ context (không chỉ trang hiện tại)
                                                        // Đảm bảo tài xế đã gán cho bất kỳ xe nào cũng không xuất hiện, trừ khi đang là currentDriver của xe đang chọn
                                                        const { vehicles: allVehicles } = useDispatcherContext();
                                                        const isAssigned = allVehicles.some(v => v.currentDriver?.id === driver.id);
                                                        const isCurrent = selectedVehicle?.currentDriver?.id === driver.id;
                                                        return !isAssigned || isCurrent;
                                                    }).map(driver => (_jsxs("option", { value: driver.id, children: [driver.fullName || driver.name, " (", driver.email, ")"] }, driver.id)))] }), _jsxs("div", { className: "flex gap-3 pt-4", children: [_jsx("button", { onClick: closeAssignModal, disabled: assigning, className: "flex-1 px-4 py-3 border border-gray-300 text-gray-700 rounded-lg hover:bg-gray-50 font-medium transition-colors duration-200 disabled:opacity-50 disabled:cursor-not-allowed", children: "H\u1EE7y" }), _jsx("button", { onClick: handleAssignDriver, disabled: assigning, className: "flex-1 px-4 py-3 bg-gradient-to-r from-blue-500 to-indigo-600 hover:from-blue-600 hover:to-indigo-700 text-white rounded-lg font-medium transition-all duration-200 disabled:opacity-50 disabled:cursor-not-allowed disabled:hover:from-blue-500 disabled:hover:to-indigo-600", children: assigning ? (_jsxs("div", { className: "flex items-center justify-center gap-2", children: [_jsx("div", { className: "animate-spin rounded-full h-4 w-4 border-b-2 border-white" }), "\u0110ang g\u00E1n..."] })) : ("Xác nhận gán") })] })] })] })) })] }) }))] }));
}
