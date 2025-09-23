import { jsx as _jsx, jsxs as _jsxs, Fragment as _Fragment } from "react/jsx-runtime";
import React, { useState, useMemo } from "react";
import { Edit, MoreVertical, AlertCircle, CheckCircle, Clock, } from "lucide-react";
// Enhanced Status Badge Component
const StatusBadge = React.memo(({ status }) => {
    const statusConfig = useMemo(() => {
        switch (status) {
            case "MAINTENANCE_PENDING":
                return {
                    icon: _jsx(AlertCircle, { size: 14 }),
                    className: "bg-red-50 text-red-700 border-red-200",
                    text: "Cần bảo trì",
                };
            case "MAINTENANCE":
                return {
                    icon: _jsx(Clock, { size: 14 }),
                    className: "bg-yellow-50 text-yellow-700 border-yellow-200",
                    text: "Đang bảo trì",
                };
            case "IN_USE":
                return {
                    icon: _jsx(CheckCircle, { size: 14 }),
                    className: "bg-blue-50 text-blue-700 border-blue-200",
                    text: "Đang sử dụng",
                };
            case "AVAILABLE":
            default:
                return {
                    icon: _jsx(CheckCircle, { size: 14 }),
                    className: "bg-green-50 text-green-700 border-green-200",
                    text: "Sẵn sàng sử dụng",
                };
        }
    }, [status]);
    return (_jsxs("span", { className: `inline-flex items-center gap-1 px-2.5 py-1 rounded-full text-xs font-medium border ${statusConfig.className}`, children: [statusConfig.icon, statusConfig.text] }));
});
StatusBadge.displayName = "StatusBadge";
// Action Dropdown Component
const ActionDropdown = React.memo(({ vehicle, onEdit, onDelete }) => {
    const [isOpen, setIsOpen] = useState(false);
    return (_jsxs("div", { className: "relative", children: [_jsx("button", { onClick: () => setIsOpen(!isOpen), className: "p-2 rounded-full hover:bg-gray-100 transition-colors", "aria-label": "T\u00F9y ch\u1ECDn", children: _jsx(MoreVertical, { size: 18 }) }), isOpen && (_jsxs(_Fragment, { children: [_jsx("div", { className: "fixed inset-0 z-10", onClick: () => setIsOpen(false) }), _jsxs("div", { className: "absolute right-0 top-full mt-1 w-48 bg-white rounded-lg shadow-xl border border-gray-200 py-1 z-30", children: [onEdit && (_jsxs("button", { onClick: () => {
                                    onEdit(vehicle);
                                    setIsOpen(false);
                                }, className: "w-full px-4 py-2 text-left text-sm hover:bg-gray-50 flex items-center gap-2", children: [_jsx(Edit, { size: 16 }), "Ch\u1EC9nh s\u1EEDa"] })), onDelete && (_jsxs("button", { onClick: () => {
                                    onDelete(vehicle.id);
                                    setIsOpen(false);
                                }, className: "w-full px-4 py-2 text-left text-sm hover:bg-gray-50 flex items-center gap-2 text-red-600", children: [_jsx("svg", { width: "16", height: "16", fill: "none", stroke: "currentColor", strokeWidth: "2", viewBox: "0 0 24 24", children: _jsx("path", { d: "M3 6h18M9 6V4a1 1 0 0 1 1-1h4a1 1 0 0 1 1 1v2m2 0v14a2 2 0 0 1-2 2H7a2 2 0 0 1-2-2V6h14z" }) }), "X\u00F3a"] }))] })] }))] }));
});
ActionDropdown.displayName = "ActionDropdown";
// Main Vehicle Table Component
const VehicleTable = ({ vehicles, onEdit, onDelete, }) => {
    // Calculate days until next maintenance
    const getDaysUntilMaintenance = (nextMaintenance) => {
        if (!nextMaintenance)
            return null;
        const today = new Date();
        const maintenanceDate = new Date(nextMaintenance);
        const diffTime = maintenanceDate.getTime() - today.getTime();
        const diffDays = Math.ceil(diffTime / (1000 * 60 * 60 * 24));
        return diffDays;
    };
    // Get maintenance urgency color
    const getMaintenanceUrgencyColor = (days) => {
        if (days === null)
            return "text-gray-500";
        if (days < 0)
            return "text-red-600 font-semibold";
        if (days <= 7)
            return "text-orange-600 font-semibold";
        if (days <= 30)
            return "text-yellow-600";
        return "text-green-600";
    };
    return (_jsx("div", { className: "space-y-4", children: vehicles.map((vehicle) => {
            const daysUntilMaintenance = getDaysUntilMaintenance(vehicle.nextMaintenance);
            const maintenanceUrgencyColor = getMaintenanceUrgencyColor(daysUntilMaintenance);
            // Logic xác định trạng thái xe - ưu tiên trạng thái từ database
            let computedStatus = "AVAILABLE";
            console.log("Vehicle data:", vehicle); // Debug log để kiểm tra dữ liệu
            // Nếu có trạng thái từ database, sử dụng nó
            if (vehicle.status) {
                computedStatus = vehicle.status;
                console.log("Using status from database:", vehicle.status); // Debug log
            }
            else {
                console.log("No status from database, using fallback logic"); // Debug log
                // Fallback: tính toán dựa trên thời gian bảo trì và tài xế
                const today = new Date();
                // Kiểm tra xem xe có đang trong thời gian bảo trì không
                if (vehicle.lastMaintenance && vehicle.nextMaintenance) {
                    const maintenanceStartDate = new Date(vehicle.lastMaintenance);
                    const maintenanceEndDate = new Date(vehicle.nextMaintenance);
                    // Nếu hôm nay nằm trong khoảng thời gian bảo trì
                    if (today >= maintenanceStartDate && today <= maintenanceEndDate) {
                        computedStatus = "MAINTENANCE";
                    }
                    else if (vehicle.driver?.name ||
                        vehicle.currentDriver?.fullName) {
                        computedStatus = "IN_USE";
                    }
                    else {
                        computedStatus = "AVAILABLE";
                    }
                }
                else if (vehicle.driver?.name || vehicle.currentDriver?.fullName) {
                    computedStatus = "IN_USE";
                }
            }
            return (_jsx("div", { className: "bg-white border border-gray-200 rounded-xl p-6 hover:shadow-md transition-all duration-200", children: _jsxs("div", { className: "flex flex-col lg:flex-row lg:items-center lg:justify-between gap-4", children: [_jsxs("div", { className: "flex-1 space-y-3", children: [_jsxs("div", { className: "flex flex-wrap items-center gap-3", children: [_jsx("h3", { className: "text-xl font-bold text-gray-900", children: vehicle.licensePlate }), _jsx(StatusBadge, { status: computedStatus })] }), _jsxs("div", { className: "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4 text-sm", children: [_jsxs("div", { className: "space-y-1", children: [_jsx("span", { className: "text-gray-500 font-medium", children: "Lo\u1EA1i xe:" }), _jsx("div", { className: "text-gray-900", children: vehicle.type })] }), _jsxs("div", { className: "space-y-1", children: [_jsx("span", { className: "text-gray-500 font-medium", children: "Tr\u1ECDng t\u1EA3i (kg):" }), _jsxs("div", { className: "text-gray-900 font-semibold", children: [vehicle.capacityWeightKg?.toLocaleString() ?? "-", " kg"] })] }), _jsxs("div", { className: "space-y-1", children: [_jsx("span", { className: "text-gray-500 font-medium", children: "Th\u1EC3 t\u00EDch (m\u00B3):" }), _jsxs("div", { className: "text-gray-900 font-semibold", children: [vehicle.capacityVolumeM3?.toLocaleString() ?? "-", " m\u00B3"] })] }), _jsxs("div", { className: "space-y-1", children: [_jsx("span", { className: "text-gray-500 font-medium", children: "T\u00E0i x\u1EBF:" }), _jsx("div", { className: "text-gray-900", children: vehicle.driver?.name ||
                                                        vehicle.currentDriver?.fullName || (_jsx("span", { className: "text-gray-400 italic", children: "Ch\u01B0a g\u00E1n" })) })] })] }), _jsxs("div", { className: "grid grid-cols-1 md:grid-cols-2 gap-4 text-sm pt-2 border-t border-gray-100", children: [_jsxs("div", { className: "space-y-1", children: [_jsx("span", { className: "text-gray-500 font-medium", children: "B\u1EA3o tr\u00EC g\u1EA7n nh\u1EA5t:" }), _jsx("div", { className: "text-gray-900", children: vehicle.lastMaintenance || (_jsx("span", { className: "text-gray-400 italic", children: "Ch\u01B0a c\u00F3 th\u00F4ng tin" })) })] }), _jsxs("div", { className: "space-y-1", children: [_jsx("span", { className: "text-gray-500 font-medium", children: "B\u1EA3o tr\u00EC ti\u1EBFp theo:" }), _jsx("div", { className: maintenanceUrgencyColor, children: vehicle.nextMaintenance ? (_jsxs("div", { className: "flex items-center gap-2", children: [_jsx("span", { children: vehicle.nextMaintenance }), daysUntilMaintenance !== null && (_jsx("span", { className: "text-xs px-2 py-1 rounded-full bg-gray-100", children: daysUntilMaintenance < 0
                                                                    ? `Quá hạn ${Math.abs(daysUntilMaintenance)} ngày`
                                                                    : daysUntilMaintenance === 0
                                                                        ? "Hôm nay"
                                                                        : `Còn ${daysUntilMaintenance} ngày` }))] })) : (_jsx("span", { className: "text-gray-400 italic", children: "Ch\u01B0a l\u1EADp l\u1ECBch" })) })] })] })] }), _jsx("div", { className: "flex-shrink-0", children: _jsx(ActionDropdown, { vehicle: vehicle, onEdit: onEdit, onDelete: onDelete }) })] }) }, vehicle.id));
        }) }));
};
export default React.memo(VehicleTable);
