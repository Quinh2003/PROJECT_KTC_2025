import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useEffect, useState } from "react";
import AdminDashboard from "./AdminDashboard/AdminDashboard";
import DispatcherDashboard from "./DispatcherDashboard/DispatcherDashboard";
import FleetDashboard from "./FleetDashboard/FleetDashboard";
import DriverDashboard from "./DriverDashboard/DriverDashboard";
import OperationsDashboard from "./OperationsDashboard/OperationsDashboard";
export default function Dashboard({ user: userProp, onLogout, }) {
    // Lấy user từ localStorage nếu chưa truyền qua props
    const [user, setUser] = useState(userProp || JSON.parse(localStorage.getItem("user") || "null"));
    const [protectedData, setProtectedData] = useState(null);
    const [error, setError] = useState("");
    // Ví dụ fetch API protected khi vào dashboard
    useEffect(() => {
        const token = localStorage.getItem("token");
        if (!token)
            return;
        fetch("http://localhost:8080/api/protected/profile", {
            headers: {
                Authorization: `Bearer ${token}`,
                "Content-Type": "application/json",
            },
        })
            .then((res) => {
            if (!res.ok)
                throw new Error("Không có quyền truy cập hoặc token hết hạn!");
            return res.json();
        })
            .then(setProtectedData)
            .catch((err) => setError(err.message));
    }, []);
    // Phân quyền giao diện theo role
    if (!user) {
        return (_jsx("div", { className: "min-h-screen flex items-center justify-center bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100", children: _jsxs("div", { className: "bg-white/30 backdrop-blur-lg rounded-2xl p-8 border border-white/30 shadow-xl text-center", children: [_jsx("div", { className: "text-2xl font-bold text-gray-800 mb-2", children: "\u26A0\uFE0F Ch\u01B0a \u0111\u0103ng nh\u1EADp" }), _jsx("div", { className: "text-gray-600", children: "B\u1EA1n c\u1EA7n \u0111\u0103ng nh\u1EADp \u0111\u1EC3 truy c\u1EADp dashboard!" })] }) }));
    }
    if (user.role === "ADMIN")
        return (_jsx("div", { className: "min-h-screen bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100", children: _jsx(AdminDashboard, { user: user, onLogout: onLogout }) }));
    if (user.role === "DISPATCHER")
        return (_jsx("div", { className: "min-h-screen", children: _jsx(DispatcherDashboard, { user: user, onLogout: onLogout }) }));
    if (user.role === "FLEET")
        return (_jsx("div", { className: "min-h-screen bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100", children: _jsx(FleetDashboard, { user: user, onLogout: onLogout }) }));
    if (user.role === "DRIVER")
        return (_jsx("div", { className: "min-h-screen bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100", children: _jsx(DriverDashboard, { user: user, onLogout: onLogout }) }));
    if (user.role === "OPERATIONS")
        return (_jsx("div", { className: "min-h-screen bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100", children: _jsx(OperationsDashboard, { user: user, onLogout: onLogout }) }));
    return (_jsx("div", { className: "min-h-screen flex items-center justify-center bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100", children: _jsxs("div", { className: "bg-white/30 backdrop-blur-lg rounded-2xl p-8 border border-white/30 shadow-xl text-center", children: [_jsx("div", { className: "text-2xl font-bold text-gray-800 mb-2", children: "\u274C Role kh\u00F4ng h\u1EE3p l\u1EC7" }), _jsx("div", { className: "text-gray-600", children: "Kh\u00F4ng x\u00E1c \u0111\u1ECBnh \u0111\u01B0\u1EE3c quy\u1EC1n truy c\u1EADp c\u1EE7a b\u1EA1n!" })] }) }));
}
