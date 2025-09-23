import { jsxs as _jsxs, jsx as _jsx, Fragment as _Fragment } from "react/jsx-runtime";
import { X, Truck, User, Phone, Calendar, Wrench, FileText, } from "lucide-react";
import { useEffect, useState } from "react";
import { fetchVehicleMaintenanceByVehicleId } from "../../services/VehicleMaintenanceAPI";
export default function VehicleDetailModal({ vehicle, isOpen, onClose, onScheduleMaintenance, }) {
    const [maintenances, setMaintenances] = useState([]);
    const [emergencyRequests, setEmergencyRequests] = useState([]);
    const [loading, setLoading] = useState(false);
    useEffect(() => {
        if (isOpen && vehicle?.id) {
            setLoading(true);
            // Chỉ gọi 1 API để lấy tất cả maintenance records
            fetchVehicleMaintenanceByVehicleId(vehicle.id)
                .then((allMaintenanceData) => {
                console.log("allMaintenanceData:", allMaintenanceData);
                // Phân loại dựa trên status
                const emergencyRequests = allMaintenanceData.filter((m) => m.status?.id === 51 || m.status?.name === "MAINTENANCE_PENDING");
                const regularMaintenance = allMaintenanceData.filter((m) => m.status?.id !== 51 && m.status?.name !== "MAINTENANCE_PENDING");
                setEmergencyRequests(emergencyRequests);
                setMaintenances(regularMaintenance);
            })
                .catch((error) => {
                console.error("Error fetching maintenance data:", error);
                setEmergencyRequests([]);
                setMaintenances([]);
            })
                .finally(() => setLoading(false));
        }
        else {
            setMaintenances([]);
            setEmergencyRequests([]);
        }
    }, [isOpen, vehicle]);
    if (!isOpen)
        return null;
    const formatDate = (dateString) => {
        if (!dateString)
            return "Chưa có";
        return new Date(dateString).toLocaleDateString("vi-VN");
    };
    const getStatusText = (status) => {
        if (typeof status === "string") {
            return status === "MAINTENANCE_PENDING"
                ? "Cần bảo trì"
                : status === "MAINTENANCE"
                    ? "Đang bảo trì"
                    : status === "IN_USE"
                        ? "Đang sử dụng"
                        : "Sẵn sàng";
        }
        if (typeof status === "object" && status?.name) {
            return status.name === "MAINTENANCE_PENDING"
                ? "Cần bảo trì"
                : status.name === "MAINTENANCE"
                    ? "Đang bảo trì"
                    : status.name === "IN_USE"
                        ? "Đang sử dụng"
                        : "Sẵn sàng";
        }
        return "Không xác định";
    };
    const getStatusColor = (status) => {
        const statusText = getStatusText(status);
        if (statusText === "Cần bảo trì")
            return "text-red-600 bg-red-100";
        if (statusText === "Đang bảo trì")
            return "text-orange-600 bg-orange-100";
        if (statusText === "Đang sử dụng")
            return "text-blue-600 bg-blue-100";
        return "text-green-600 bg-green-100";
    };
    return (_jsx("div", { className: "fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50", children: _jsxs("div", { className: "bg-white rounded-lg p-6 w-full max-w-2xl max-h-[80vh] overflow-y-auto", children: [_jsxs("div", { className: "flex justify-between items-center mb-6", children: [_jsxs("h2", { className: "text-xl font-semibold text-gray-900", children: ["Chi ti\u1EBFt xe ", vehicle.licensePlate] }), _jsx("button", { onClick: onClose, className: "text-gray-400 hover:text-gray-600", children: _jsx(X, { size: 24 }) })] }), _jsxs("div", { className: "grid grid-cols-1 md:grid-cols-2 gap-6", children: [_jsxs("div", { className: "space-y-4", children: [_jsx("h3", { className: "font-medium text-gray-900 border-b pb-2", children: "Th\u00F4ng tin xe" }), _jsxs("div", { className: "flex items-center space-x-3", children: [_jsx(Truck, { className: "text-gray-400", size: 20 }), _jsxs("div", { children: [_jsx("p", { className: "text-sm text-gray-500", children: "Bi\u1EC3n s\u1ED1 xe" }), _jsx("p", { className: "font-medium", children: vehicle.licensePlate })] })] }), _jsxs("div", { className: "flex items-center space-x-3", children: [_jsx("div", { className: "w-5 h-5 bg-gray-400 rounded-full" }), _jsxs("div", { children: [_jsx("p", { className: "text-sm text-gray-500", children: "Lo\u1EA1i xe" }), _jsx("p", { className: "font-medium", children: vehicle.vehicleType || "N/A" })] })] }), _jsxs("div", { className: "flex items-center space-x-3", children: [_jsx("div", { className: "w-5 h-5 bg-gray-400 rounded-full" }), _jsxs("div", { children: [_jsx("p", { className: "text-sm text-gray-500", children: "T\u1EA3i tr\u1ECDng" }), _jsxs("p", { className: "font-medium", children: [vehicle.capacityWeightKg || "N/A", " kg"] })] })] }), _jsxs("div", { className: "flex items-center space-x-3", children: [_jsx("div", { className: "w-5 h-5 bg-gray-400 rounded-full" }), _jsxs("div", { children: [_jsx("p", { className: "text-sm text-gray-500", children: "Tr\u1EA1ng th\u00E1i" }), _jsx("span", { className: `inline-block px-2 py-1 rounded-full text-xs font-medium ${getStatusColor(vehicle.status)}`, children: getStatusText(vehicle.status) })] })] })] }), _jsxs("div", { className: "space-y-4", children: [_jsx("h3", { className: "font-medium text-gray-900 border-b pb-2", children: "Th\u00F4ng tin t\u00E0i x\u1EBF" }), vehicle.driver || vehicle.currentDriver ? (_jsxs(_Fragment, { children: [_jsxs("div", { className: "flex items-center space-x-3", children: [_jsx(User, { className: "text-gray-400", size: 20 }), _jsxs("div", { children: [_jsx("p", { className: "text-sm text-gray-500", children: "T\u00EAn t\u00E0i x\u1EBF" }), _jsx("p", { className: "font-medium", children: vehicle.driver?.name ||
                                                                vehicle.currentDriver?.fullName ||
                                                                "N/A" })] })] }), _jsxs("div", { className: "flex items-center space-x-3", children: [_jsx(Phone, { className: "text-gray-400", size: 20 }), _jsxs("div", { children: [_jsx("p", { className: "text-sm text-gray-500", children: "S\u1ED1 \u0111i\u1EC7n tho\u1EA1i" }), _jsx("p", { className: "font-medium", children: vehicle.driver?.phone ||
                                                                vehicle.currentDriver?.phone ||
                                                                "N/A" })] })] })] })) : (_jsx("div", { className: "text-gray-500 italic", children: "Ch\u01B0a c\u00F3 t\u00E0i x\u1EBF \u0111\u01B0\u1EE3c ph\u00E2n c\u00F4ng" }))] }), _jsxs("div", { className: "space-y-4 md:col-span-2", children: [_jsxs("h3", { className: "font-medium text-gray-900 border-b pb-2 flex items-center gap-2", children: [_jsx(FileText, { className: "w-5 h-5 text-blue-400" }), "Th\u00F4ng tin b\u1EA3o tr\u00EC"] }), _jsxs("div", { className: "grid grid-cols-1 md:grid-cols-2 gap-4 mb-2", children: [_jsxs("div", { className: "flex items-center space-x-3", children: [_jsx(Calendar, { className: "text-gray-400", size: 20 }), _jsxs("div", { children: [_jsx("p", { className: "text-sm text-gray-500", children: "B\u1EA3o tr\u00EC g\u1EA7n nh\u1EA5t" }), _jsx("p", { className: "font-medium", children: formatDate(vehicle.lastMaintenance) })] })] }), _jsxs("div", { className: "flex items-center space-x-3", children: [_jsx(Wrench, { className: "text-gray-400", size: 20 }), _jsxs("div", { children: [_jsx("p", { className: "text-sm text-gray-500", children: "B\u1EA3o tr\u00EC ti\u1EBFp theo" }), _jsx("p", { className: "font-medium", children: formatDate(vehicle.nextMaintenance) })] })] })] }), _jsxs("div", { className: "mt-2", children: [_jsx("h4", { className: "font-semibold text-red-700 mb-2", children: "Y\u00EAu c\u1EA7u b\u1EA3o tr\u00EC kh\u1EA9n c\u1EA5p" }), loading ? (_jsx("div", { className: "text-gray-500 italic", children: "\u0110ang t\u1EA3i..." })) : emergencyRequests.length === 0 ? (_jsx("div", { className: "text-gray-500 italic", children: "Kh\u00F4ng c\u00F3 y\u00EAu c\u1EA7u b\u1EA3o tr\u00EC kh\u1EA9n c\u1EA5p" })) : (_jsx("div", { className: "space-y-2 max-h-32 overflow-y-auto", children: emergencyRequests.map((e) => (_jsxs("div", { className: "border rounded p-2 bg-red-50 flex flex-col md:flex-row md:items-center md:justify-between", children: [_jsxs("div", { children: [_jsx("span", { className: "font-medium text-red-700", children: e.maintenanceType || "Khẩn cấp" }), _jsx("span", { className: "mx-2 text-gray-400", children: "|" }), _jsx("span", { className: "text-gray-700", children: e.description })] }), _jsx("div", { className: "flex flex-col md:items-end md:justify-end", children: _jsx("div", { className: "text-xs text-gray-500 mt-1 md:mt-0", children: e.createdAt
                                                                ? new Date(e.createdAt).toLocaleString("vi-VN")
                                                                : "" }) })] }, e.id))) }))] })] })] }), _jsx("div", { className: "flex justify-end mt-6 pt-4 border-t", children: _jsx("button", { onClick: onClose, className: "px-4 py-2 text-gray-600 bg-gray-100 rounded-lg hover:bg-gray-200 transition-colors", children: "\u0110\u00F3ng" }) })] }) }));
}
