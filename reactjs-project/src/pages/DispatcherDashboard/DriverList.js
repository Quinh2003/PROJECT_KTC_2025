import { jsx as _jsx, jsxs as _jsxs, Fragment as _Fragment } from "react/jsx-runtime";
import { useState } from "react";
import { editUser, updateUserStatus } from "../../services/adminAPI";
import { useDispatcherContext } from "../../contexts/DispatcherContext";
export default function DriverList() {
    const { drivers, driversLoading, driversError, refreshDrivers } = useDispatcherContext();
    const [searchTerm, setSearchTerm] = useState("");
    const [updatingStatus, setUpdatingStatus] = useState(null);
    const toggleDriverStatus = async (driverId, currentStatus) => {
        try {
            setUpdatingStatus(Number(driverId));
            // Xác định trạng thái mới dựa trên status hiện tại
            const currentStatusName = currentStatus?.name?.toLowerCase() || "";
            let newStatus;
            if (currentStatusName === "active") {
                newStatus = "Inactive";
            }
            else if (currentStatusName === "inactive") {
                newStatus = "Active";
            }
            else if (currentStatusName === "pending") {
                newStatus = "Inactive"; // Pending -> Inactive
            }
            else {
                newStatus = "Active"; // Default to Active
            }
            console.log(`[DriverList] Updating driver ${driverId} from ${currentStatus?.name} to ${newStatus}`);
            try {
                // Thử dùng endpoint riêng cho status trước
                console.log("[DriverList] Trying updateUserStatus endpoint...");
                try {
                    await updateUserStatus(driverId, newStatus);
                    console.log("[DriverList] updateUserStatus successful");
                }
                catch (statusError) {
                    console.log("[DriverList] updateUserStatus failed, using editUser fallback...", statusError);
                    throw statusError; // Force fallback to editUser
                }
            }
            catch (statusError) {
                console.log("[DriverList] Using editUser fallback...", statusError);
                // Nếu endpoint status không có, fallback về editUser
                const currentDriver = drivers.find(d => d.id === driverId);
                if (!currentDriver) {
                    throw new Error("Không tìm thấy tài xế");
                }
                console.log("[DriverList] Current driver found:", currentDriver);
                // Map newStatus to correct statusId
                const statusMap = {
                    "Active": 7,
                    "Inactive": 8,
                    "Pending": 1
                };
                const updatedDriver = {
                    ...currentDriver,
                    status: {
                        id: statusMap[newStatus] || 7,
                        name: newStatus,
                        statusType: "USER",
                        description: (typeof currentDriver.status === 'object' && currentDriver.status?.description) ? currentDriver.status.description : ""
                    }
                };
                console.log("[DriverList] Payload for editUser:", updatedDriver);
                await editUser(driverId, updatedDriver);
                console.log("[DriverList] editUser successful");
            }
            console.log("[DriverList] API call successful, refreshing data...");
            // Refresh drivers data from context
            await refreshDrivers(true);
            console.log("[DriverList] Data refreshed successfully");
        }
        catch (err) {
            console.error("Failed to update driver status:", err);
            // Thông báo lỗi cho user
            alert(`Lỗi cập nhật trạng thái tài xế: ${err.message}`);
        }
        finally {
            setUpdatingStatus(null);
        }
    };
    const filteredDrivers = drivers.filter(driver => {
        const name = (driver.fullName ?? driver.name ?? "");
        return (name.toLowerCase().includes(searchTerm.toLowerCase()) ||
            (driver.email?.toLowerCase().includes(searchTerm.toLowerCase())) ||
            (driver.phone && driver.phone.includes(searchTerm)));
    });
    const getStatusBadge = (status, driverId) => {
        // Kiểm tra nhiều trường hợp status name
        const statusName = status?.name?.toLowerCase() || "";
        const isActive = statusName === "active" || statusName === "pending"; // Coi Pending như Active cho test
        const isUpdating = updatingStatus === Number(driverId);
        console.log(`[DriverList] Status for driver ${driverId}:`, status, "isActive:", isActive);
        return (_jsxs("button", { onClick: () => toggleDriverStatus(driverId, status), disabled: isUpdating, className: `inline-flex items-center gap-2 px-3 py-1.5 rounded-full text-sm font-medium transition-all duration-200 hover:scale-105 disabled:opacity-50 disabled:cursor-not-allowed ${isActive
                ? "bg-green-100 text-green-800 border border-green-200 hover:bg-green-200"
                : "bg-gray-100 text-gray-600 border border-gray-200 hover:bg-gray-200"}`, children: [isUpdating ? (_jsx("div", { className: "animate-spin w-2 h-2 border border-current border-t-transparent rounded-full" })) : (_jsx("div", { className: `w-2 h-2 rounded-full ${isActive ? "bg-green-500" : "bg-gray-400"}` })), _jsx("span", { className: "text-xs opacity-75", children: status?.name || "N/A" })] }));
    };
    return (_jsxs("div", { className: "space-y-6", children: [_jsx("div", { className: "bg-gradient-to-r from-blue-600 to-indigo-700 rounded-2xl p-6 text-white shadow-lg", children: _jsx("div", { className: "flex flex-col md:flex-row md:items-center md:justify-between gap-4", children: _jsx("div", { children: _jsx("h2", { className: "text-2xl font-bold mb-2", children: "Qu\u1EA3n l\u00FD t\u00E0i x\u1EBF" }) }) }) }), _jsx("div", { className: "bg-white/80 backdrop-blur-sm rounded-xl p-6 shadow-md border border-white/50", children: _jsx("div", { className: "flex flex-col md:flex-row gap-4 items-center", children: _jsxs("div", { className: "relative flex-1", children: [_jsx("div", { className: "absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none", children: _jsx("svg", { className: "h-5 w-5 text-gray-400", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z" }) }) }), _jsx("input", { type: "text", placeholder: "T\u00ECm ki\u1EBFm theo t\u00EAn, email ho\u1EB7c s\u1ED1 \u0111i\u1EC7n tho\u1EA1i...", value: searchTerm, onChange: (e) => setSearchTerm(e.target.value), className: "w-full pl-10 pr-4 py-3 border border-gray-200 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent bg-white/90 backdrop-blur-sm" })] }) }) }), _jsx("div", { className: "bg-white/80 backdrop-blur-sm rounded-xl shadow-md border border-white/50 overflow-hidden", children: driversLoading ? (_jsx("div", { className: "flex items-center justify-center p-12", children: _jsxs("div", { className: "flex items-center gap-3", children: [_jsx("div", { className: "animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600" }), _jsx("span", { className: "text-gray-600 font-medium", children: "\u0110ang t\u1EA3i d\u1EEF li\u1EC7u..." })] }) })) : driversError ? (_jsx("div", { className: "flex items-center justify-center p-12", children: _jsxs("div", { className: "text-center", children: [_jsx("div", { className: "w-12 h-12 bg-red-100 rounded-full flex items-center justify-center mx-auto mb-4", children: _jsx("svg", { className: "w-6 h-6 text-red-600", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M12 8v4m0 4h.01M21 12a9 9 0 11-18 0 9 9 0 0118 0z" }) }) }), _jsx("h3", { className: "text-lg font-semibold text-gray-900 mb-2", children: "C\u00F3 l\u1ED7i x\u1EA3y ra" }), _jsx("p", { className: "text-red-600", children: driversError })] }) })) : filteredDrivers.length === 0 ? (_jsx("div", { className: "flex items-center justify-center p-12", children: _jsxs("div", { className: "text-center", children: [_jsx("div", { className: "w-12 h-12 bg-gray-100 rounded-full flex items-center justify-center mx-auto mb-4", children: _jsx("svg", { className: "w-6 h-6 text-gray-400", fill: "none", stroke: "currentColor", viewBox: "0 0 24 24", children: _jsx("path", { strokeLinecap: "round", strokeLinejoin: "round", strokeWidth: 2, d: "M17 20h5v-2a3 3 0 00-5.356-1.857M17 20H7m10 0v-2c0-.656-.126-1.283-.356-1.857M7 20H2v-2a3 3 0 015.356-1.857M7 20v-2c0-.656.126-1.283.356-1.857m0 0a5.002 5.002 0 019.288 0M15 7a3 3 0 11-6 0 3 3 0 016 0zm6 3a2 2 0 11-4 0 2 2 0 014 0zM7 10a2 2 0 11-4 0 2 2 0 014 0z" }) }) }), _jsx("h3", { className: "text-lg font-semibold text-gray-900 mb-2", children: "Kh\u00F4ng t\u00ECm th\u1EA5y t\u00E0i x\u1EBF" }), _jsx("p", { className: "text-gray-500", children: searchTerm ? "Không có tài xế nào phù hợp với từ khóa tìm kiếm" : "Chưa có tài xế nào trong hệ thống" })] }) })) : (_jsxs(_Fragment, { children: [_jsx("div", { className: "hidden md:block overflow-x-auto", children: _jsxs("table", { className: "min-w-full divide-y divide-gray-200", children: [_jsx("thead", { className: "bg-gray-50/80 backdrop-blur-sm", children: _jsxs("tr", { children: [_jsx("th", { className: "px-6 py-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider", children: "T\u00E0i x\u1EBF" }), _jsx("th", { className: "px-6 py-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider", children: "Th\u00F4ng tin li\u00EAn h\u1EC7" }), _jsx("th", { className: "px-6 py-4 text-left text-xs font-medium text-gray-500 uppercase tracking-wider", children: "Tr\u1EA1ng th\u00E1i" })] }) }), _jsx("tbody", { className: "bg-white/60 backdrop-blur-sm divide-y divide-gray-200", children: filteredDrivers.map((driver) => (_jsxs("tr", { className: "hover:bg-white/80 transition-colors duration-200", children: [_jsx("td", { className: "px-6 py-4 whitespace-nowrap", children: _jsxs("div", { className: "flex items-center", children: [_jsx("div", { className: "flex-shrink-0 h-12 w-12", children: _jsx("div", { className: "h-12 w-12 rounded-full bg-gradient-to-r from-blue-500 to-indigo-600 flex items-center justify-center text-white font-semibold text-lg", children: ((driver.fullName ?? driver.name ?? "").toString().charAt(0) || "?").toUpperCase() }) }), _jsxs("div", { className: "ml-4", children: [_jsx("div", { className: "text-sm font-medium text-gray-900", children: driver.fullName ?? driver.name ?? "(Không tên)" }), _jsxs("div", { className: "text-sm text-gray-500", children: ["ID: ", driver.id] })] })] }) }), _jsxs("td", { className: "px-6 py-4 whitespace-nowrap", children: [_jsx("div", { className: "text-sm text-gray-900", children: driver.email }), _jsx("div", { className: "text-sm text-gray-500", children: driver.phone || "Chưa cập nhật" })] }), _jsx("td", { className: "px-6 py-4 whitespace-nowrap", children: getStatusBadge(driver.status, driver.id ?? "") })] }, driver.id))) })] }) }), _jsx("div", { className: "md:hidden space-y-4 p-4", children: filteredDrivers.map((driver) => (_jsx("div", { className: "bg-white/80 backdrop-blur-sm rounded-xl p-4 shadow-sm border border-gray-200", children: _jsxs("div", { className: "flex items-start gap-4", children: [_jsx("div", { className: "flex-shrink-0 h-12 w-12 rounded-full bg-gradient-to-r from-blue-500 to-indigo-600 flex items-center justify-center text-white font-semibold text-lg", children: ((driver.fullName ?? driver.name ?? "").toString().charAt(0) || "?").toUpperCase() }), _jsxs("div", { className: "flex-1 min-w-0", children: [_jsxs("div", { className: "flex items-start justify-between", children: [_jsxs("div", { children: [_jsx("h3", { className: "text-sm font-medium text-gray-900 truncate", children: driver.fullName ?? driver.name }), _jsxs("p", { className: "text-sm text-gray-500", children: ["ID: ", driver.id] })] }), getStatusBadge(driver.status, driver.id ?? "")] }), _jsxs("div", { className: "mt-2 space-y-1", children: [_jsx("p", { className: "text-sm text-gray-600", children: driver.email }), _jsx("p", { className: "text-sm text-gray-600", children: driver.phone || "Chưa cập nhật SĐT" })] })] })] }) }, driver.id))) })] })) })] }));
}
