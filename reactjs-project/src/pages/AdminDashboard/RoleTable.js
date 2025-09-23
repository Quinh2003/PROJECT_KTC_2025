import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useState } from "react";
import { ROLES, ALL_PERMISSIONS, PERMISSION_DESCRIPTIONS } from "../../constants/roles";
import * as FaIcons from "react-icons/fa";
import { FaBellConcierge } from "react-icons/fa6";
function getIconComponent(iconName, className) {
    if (iconName === "FaBellConcierge")
        return _jsx(FaBellConcierge, { className: className ?? "" });
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    const Icon = FaIcons[iconName];
    return Icon ? _jsx(Icon, { className: className ?? "" }) : null;
}
export default function RoleTable() {
    const [roles, setRoles] = useState(ROLES);
    const [editingRole, setEditingRole] = useState(null);
    const [tempPermissions, setTempPermissions] = useState([]);
    const [selectedPermission, setSelectedPermission] = useState(null);
    const handlePermissionClick = (_roleName, permission) => {
        setSelectedPermission(permission);
    };
    const handleEditRole = (role) => {
        setEditingRole(role);
        setTempPermissions(role.permissions);
    };
    const handleTogglePermission = (perm) => {
        setTempPermissions((prev) => prev.includes(perm)
            ? prev.filter((p) => p !== perm)
            : [...prev, perm]);
    };
    const handleSavePermissions = () => {
        if (!editingRole)
            return;
        setRoles((prev) => prev.map((r) => r.key === editingRole.key ? { ...r, permissions: tempPermissions } : r));
        setEditingRole(null);
        setTempPermissions([]);
    };
    return (_jsx("div", { className: "p-0", children: _jsxs("div", { className: "bg-white border border-gray-200 rounded-2xl p-8 ", children: [_jsx("h2", { className: "text-2xl font-bold mb-8", children: "Role Permissions" }), _jsx("div", { className: "flex flex-col gap-8", children: roles.map((role) => (_jsxs("div", { className: "bg-white rounded-xl shadow flex flex-col md:flex-row md:items-center justify-between py-6 px-8", children: [_jsxs("div", { children: [_jsxs("div", { className: "flex items-center gap-2 text-xl font-semibold mb-4 md:mb-2", children: [_jsx("span", { children: getIconComponent(role.icon ?? "", "text-2xl") }), role.name] }), _jsx("div", { className: "flex flex-wrap gap-3", children: role.permissions.map((p) => (_jsx("button", { className: "bg-gray-100 px-4 py-1 rounded-full text-sm font-medium hover:bg-blue-100 transition", onClick: () => handlePermissionClick(role.name, p), type: "button", children: p }, p))) }), selectedPermission && role.permissions.includes(selectedPermission) && (_jsxs("div", { className: "mt-4 text-gray-600 bg-blue-50 rounded px-4 py-2", children: [_jsxs("span", { className: "font-semibold", children: [selectedPermission, ":"] }), " ", PERMISSION_DESCRIPTIONS[selectedPermission] || "No description."] }))] }), _jsx("button", { className: "mt-6 md:mt-0 px-5 py-2 rounded border font-semibold hover:bg-gray-100", onClick: () => handleEditRole(role), type: "button", children: "Edit" })] }, role.key))) }), editingRole && (_jsx("div", { className: "fixed inset-0 bg-black/30 flex items-center justify-center z-50", children: _jsxs("div", { className: "bg-white rounded-xl shadow-lg p-8 w-full max-w-md", children: [_jsxs("h2", { className: "text-xl font-bold mb-4", children: ["Set permissions for role: ", editingRole.name] }), _jsx("div", { className: "flex flex-wrap gap-2 mb-4", children: ALL_PERMISSIONS.map((perm) => (_jsx("button", { className: `px-3 py-1 rounded-full text-sm font-medium border
                      ${tempPermissions.includes(perm)
                                        ? "bg-blue-500 text-white"
                                        : "bg-gray-100 text-black"}
                    `, onClick: () => handleTogglePermission(perm), type: "button", children: perm }, perm))) }), _jsxs("div", { className: "flex gap-2 justify-end", children: [_jsx("button", { className: "px-4 py-2 rounded bg-gray-200", onClick: () => setEditingRole(null), children: "Cancel" }), _jsx("button", { className: "px-4 py-2 rounded bg-teal-600 text-white font-bold", onClick: handleSavePermissions, children: "Save" })] })] }) }))] }) }));
}
