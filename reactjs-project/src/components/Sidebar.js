import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { MdManageAccounts } from "react-icons/md";
import { RiShieldKeyholeLine } from "react-icons/ri";
import { AiOutlineSetting, AiOutlineSafetyCertificate } from "react-icons/ai";
import { FiActivity } from "react-icons/fi";
import { HiOutlineDocumentReport } from "react-icons/hi";
import logo from "../assets/logo.png";
// eslint-disable-next-line @typescript-eslint/no-explicit-any
const ALL_MENUS = {
    dispatcher: [
        { key: "orders", label: "Orders", icon: _jsx(MdManageAccounts, {}) },
        { key: "resources", label: "Resources", icon: _jsx(RiShieldKeyholeLine, {}) },
        { key: "assignment", label: "Assignment", icon: _jsx(AiOutlineSetting, {}) },
    ],
    operations: [
        { key: "overview", label: "Overview", icon: _jsx(MdManageAccounts, {}) },
        { key: "performance", label: "Performance", icon: _jsx(AiOutlineSafetyCertificate, {}) },
        { key: "monitoring", label: "Monitoring", icon: _jsx(HiOutlineDocumentReport, {}) },
        { key: "staff", label: "Staff", icon: _jsx(RiShieldKeyholeLine, {}) },
    ],
    fleet: [
        { key: "vehicles", label: "Quản lý phương tiện", icon: _jsx(MdManageAccounts, {}) },
        { key: "maintenance", label: "Bảo trì xe", icon: _jsx(AiOutlineSetting, {}) },
        { key: "schedule", label: "Lịch bảo trì", icon: _jsx(FiActivity, {}) },
    ],
    admin: [
        { key: "users", label: "User Management", icon: _jsx(MdManageAccounts, {}) },
        { key: "roles", label: "Role Permissions", icon: _jsx(RiShieldKeyholeLine, {}) },
        { key: "settings", label: "System Settings", icon: _jsx(AiOutlineSetting, {}) },
        { key: "logs", label: "Audit Logs", icon: _jsx(FiActivity, {}) },
    ],
};
function getMenu(role) {
    return ALL_MENUS[role];
}
export default function Sidebar({ activeTab, onTabChange, role, }) {
    const MENU = getMenu(role);
    return (_jsxs("aside", { className: "group ml-3 flex-shrink-0 w-20 hover:w-64 transition-all duration-300 bg-white/20 backdrop-blur-lg border-r border-white/30 text-gray-800 flex flex-col py-6 px-4 overflow-hidden h-screen sticky top-0", children: [_jsxs("div", { className: "mb-5 flex items-center -mt-3 -ml-4 gap-1", children: [_jsx("div", { className: "w-16 h-16 rounded-full flex items-center justify-center flex-shrink-0 overflow-hidden ", children: _jsx("img", { src: logo, alt: "Logo", className: "w-12 h-12 rounded-full object-cover" }) }), _jsx("span", { className: "hidden group-hover:inline-block font-bold text-lg tracking-wide transition-all duration-300 whitespace-nowrap overflow-hidden text-gray-700", style: { maxWidth: "200px" }, children: "Fast Route" })] }), _jsx("nav", { className: "flex-1 flex flex-col gap-4", children: MENU.map((item) => (_jsxs("button", { className: `flex -ml-3 items-center gap-3 font-semibold transition-all duration-300 rounded-xl p-4 ${activeTab === item.key
                        ? "text-blue-600 bg-white/40 backdrop-blur-sm border border-white/50 shadow-lg"
                        : "hover:text-blue-600 hover:bg-white/20 backdrop-blur-sm"}`, onClick: () => onTabChange(item.key), children: [_jsx("span", { className: "text-2xl flex-shrink-0", children: item.icon }), _jsx("span", { className: "hidden group-hover:inline transition-all duration-300 whitespace-nowrap overflow-hidden", style: { maxWidth: "160px" }, children: item.label })] }, item.key))) })] }));
}
