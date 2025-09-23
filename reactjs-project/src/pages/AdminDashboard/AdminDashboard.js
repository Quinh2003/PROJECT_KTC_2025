import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useState, useEffect, useCallback } from "react";
import { fetchUsers, fetchActivityLogs } from "../../services/adminAPI";
import UserTable from "./UserTable";
import RoleTable from "./RoleTable";
import SystemConfigForm from "./SystemConfigForm";
import AuditLogTable from "./AuditLogTable";
import Navbar from "../../components/Navbar";
import { MdManageAccounts } from "react-icons/md";
import { RiShieldKeyholeLine } from "react-icons/ri";
import { AiOutlineSafetyCertificate } from "react-icons/ai";
import { HiOutlineDocumentReport } from "react-icons/hi";
import Sidebar from "../../components/Sidebar";
export default function AdminDashboard({ user, onLogout }) {
    const [active, setActive] = useState("users");
    const [users, setUsers] = useState([]);
    const [auditCount, setAuditCount] = useState(0);
    // Callback function for AuditLogTable to update audit count
    const handleAuditCountUpdate = useCallback((newCount) => {
        console.log("Updating audit count from", auditCount, "to", newCount);
        setAuditCount(newCount);
    }, [auditCount]);
    // Callback function for UserTable to update user count in real-time
    const handleUserCountUpdate = useCallback(async () => {
        try {
            const userData = await fetchUsers();
            // eslint-disable-next-line @typescript-eslint/no-explicit-any
            const mappedUsers = userData.map((u) => {
                return {
                    id: typeof u.id === 'string' ? parseInt(u.id) : u.id,
                    name: u.fullName || u.username || "",
                    email: u.email,
                    role: u.role?.roleName || "",
                    status: u.status?.name?.toLowerCase() === "active" ? "active" : "inactive",
                    lastLogin: "-",
                    phone: u.phone || "",
                    password: u.password || "",
                };
            });
            setUsers(mappedUsers);
            console.log("User count updated to:", mappedUsers.length);
        }
        catch (err) {
            console.error("Failed to update user count:", err);
        }
    }, []);
    // Chỉ fetchUsers khi lần đầu vào trang, còn lại cập nhật trực tiếp qua UserTable
    useEffect(() => {
        const fetchData = async () => {
            try {
                // Fetch users
                const userData = await fetchUsers();
                // eslint-disable-next-line @typescript-eslint/no-explicit-any
                const mappedUsers = userData.map((u) => {
                    let roleIcon = null;
                    switch (u.role?.roleName) {
                        case "DISPATCHER":
                            roleIcon = _jsx("span", { style: { fontWeight: 'bold' }, children: "D" });
                            break;
                        case "FLEET_MANAGER":
                            roleIcon = _jsx("span", { style: { fontWeight: 'bold' }, children: "F" });
                            break;
                        case "DRIVER":
                            roleIcon = _jsx("span", { style: { fontWeight: 'bold' }, children: "Dr" });
                            break;
                        case "ADMIN":
                            roleIcon = _jsx("span", { style: { fontWeight: 'bold' }, children: "A" });
                            break;
                        case "OPERATIONS_MANAGER":
                            roleIcon = _jsx("span", { style: { fontWeight: 'bold' }, children: "O" });
                            break;
                        default:
                            roleIcon = null;
                    }
                    return {
                        id: u.id,
                        name: u.fullName || u.username || "",
                        email: u.email,
                        role: u.role?.roleName || "",
                        roleIcon,
                        status: u.status?.name?.toLowerCase() === "active" ? "active" : "inactive",
                        lastLogin: u.updatedAt ? new Date(u.updatedAt).toLocaleString() : "-",
                        phone: u.phone || "",
                        password: u.password || "",
                    };
                });
                setUsers(mappedUsers);
                // Fetch audit logs count
                const logs = await fetchActivityLogs();
                console.log("Initial fetch - audit logs:", logs.length, "logs");
                setAuditCount(logs.length);
            }
            catch (err) {
                console.error("Failed to fetch initial data:", err);
            }
        };
        // Initial fetch only - no auto-refresh
        fetchData();
    }, []);
    const uniqueRoles = Array.from(new Set(users.map(u => u.role)));
    const stats = [
        {
            label: "Total Users",
            value: users.length.toLocaleString(),
            icon: _jsx(MdManageAccounts, { className: "text-3xl text-blue-600" }),
        },
        {
            label: "Total Roles",
            value: uniqueRoles.length.toLocaleString(),
            icon: _jsx(RiShieldKeyholeLine, { className: "text-3xl text-green-600" }),
        },
        {
            label: "System Config",
            value: "",
            icon: _jsx(AiOutlineSafetyCertificate, { className: "text-3xl text-yellow-600" }),
        },
        {
            label: "Audit Events",
            value: auditCount.toLocaleString(),
            icon: _jsx(HiOutlineDocumentReport, { className: "text-3xl text-purple-600" }),
        },
    ];
    return (_jsxs("div", { className: "min-h-screen flex bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100", children: [_jsx(Sidebar, { activeTab: active, onTabChange: tab => setActive(tab), role: "admin" }), _jsxs("main", { className: "flex-1 flex flex-col bg-transparent h-screen", children: [_jsx(Navbar, { user: user, onLogout: onLogout, title: active === "users"
                            ? "User Management Dashboard"
                            : active === "roles"
                                ? "Role Permissions Dashboard"
                                : active === "settings"
                                    ? "System Configuration Dashboard"
                                    : active === "logs"
                                        ? "System Logs Dashboard"
                                        : "Admin Dashboard", subtitle: "" }), _jsx("div", { className: "flex-shrink-0 grid grid-cols-1 sm:grid-cols-2 md:grid-cols-4 gap-4 mt-3 md:mt-4 px-4 md:px-10", children: stats.map((s, idx) => (_jsxs("div", { className: "bg-white/40 backdrop-blur-lg border border-white/50 shadow-lg rounded-xl px-6 py-5 flex flex-col justify-between h-full transition-all duration-200 hover:scale-[1.03] hover:shadow-xl", children: [_jsx("div", { className: "text-gray-700 text-base mb-2 font-medium drop-shadow-sm", children: s.label }), _jsxs("div", { className: "flex items-center justify-between", children: [_jsx("div", { className: "text-2xl font-bold text-blue-900 drop-shadow-sm", children: s.value }), _jsx("div", { className: "flex items-center", children: s.icon })] })] }, idx))) }), _jsx("div", { className: "flex-1 overflow-y-auto", children: _jsxs("div", { className: "p-4 md:p-10 pt-3 md:pt-4", children: [active === "users" && _jsx(UserTable, { onUserCountUpdate: handleUserCountUpdate }), active === "roles" && _jsx(RoleTable, {}), active === "settings" && _jsx(SystemConfigForm, {}), active === "logs" && _jsx(AuditLogTable, { onAuditCountUpdate: handleAuditCountUpdate })] }) })] })] }));
}
