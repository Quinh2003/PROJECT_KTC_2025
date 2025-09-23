import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useEffect, useState } from "react";
import { fetchVehicleMaintenanceHistory } from "../../services/VehicleMaintenanceAPI";
// Mapping status name sang màu sắc
const statusMap = {
    "Completed": { label: "Hoàn thành", color: "bg-green-100 text-green-700" },
    "In Progress": { label: "Đang thực hiện", color: "bg-yellow-100 text-yellow-700" },
    // Thêm các trạng thái khác nếu có
};
// TODO: Map vehicleId sang biển số xe nếu cần (cần API hoặc dữ liệu xe)
export default function MaintenanceHistory() {
    const [data, setData] = useState([]);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState(null);
    useEffect(() => {
        setLoading(true);
        fetchVehicleMaintenanceHistory()
            .then(setData)
            .catch(() => setError("Không thể tải dữ liệu bảo trì."))
            .finally(() => setLoading(false));
    }, []);
    return (_jsxs("div", { className: "bg-white/30 backdrop-blur-lg rounded-2xl p-6 border border-white/30 shadow-xl", children: [_jsx("div", { className: "text-xl font-bold mb-2", children: "L\u1ECBch s\u1EED b\u1EA3o tr\u00EC" }), loading && _jsx("div", { children: "\u0110ang t\u1EA3i d\u1EEF li\u1EC7u..." }), error && _jsx("div", { className: "text-red-500", children: error }), _jsx("div", { className: "flex flex-col gap-4", children: data.map((item) => (_jsx("div", { className: "bg-white border rounded-xl p-4 flex flex-col md:flex-row md:items-center md:justify-between gap-4", children: _jsxs("div", { className: "flex-1 min-w-0", children: [_jsxs("div", { className: "text-lg font-bold flex items-center gap-2", children: [item.vehicle?.licensePlate || `Xe #${item.vehicle?.id}`, _jsx("span", { className: `px-2 py-1 rounded text-xs font-semibold ${statusMap[item.status?.name]?.color || "bg-gray-100 text-gray-700"}`, children: statusMap[item.status?.name]?.label || item.status?.name || "Không rõ" })] }), _jsxs("div", { className: "grid grid-cols-1 md:grid-cols-4 gap-2 mt-2 text-sm", children: [_jsxs("div", { children: [_jsx("span", { className: "font-semibold", children: "Lo\u1EA1i:" }), " ", item.maintenanceType, _jsxs("div", { children: [_jsx("span", { className: "font-semibold", children: "N\u1ED9i dung b\u1EA3o tr\u00EC:" }), " ", item.description] })] }), _jsxs("div", { children: [_jsx("span", { className: "font-semibold", children: "Chi ph\u00ED:" }), _jsxs("div", { className: "font-bold", children: [item.cost?.toLocaleString(), " VN\u0110"] })] }), _jsxs("div", { className: "flex flex-col gap-1", children: [_jsxs("div", { children: [_jsx("span", { className: "font-semibold", children: "Th\u1EDDi gian b\u1EA3o tr\u00EC:" }), " ", item.maintenanceDate?.slice(0, 10)] }), _jsxs("div", { children: [_jsx("span", { className: "font-semibold", children: "L\u1ECBch b\u1EA3o tr\u00EC s\u1EAFp t\u1EDBi:" }), " ", item.nextDueDate?.slice(0, 10)] })] }), _jsxs("div", { children: [_jsx("span", { className: "font-semibold", children: "Notes: " }), _jsx("div", { children: item.notes || _jsx("span", { className: "italic text-gray-400", children: "Kh\u00F4ng c\u00F3" }) })] })] })] }) }, item.id))) })] }));
}
