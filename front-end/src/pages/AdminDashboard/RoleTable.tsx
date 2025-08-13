import { useState } from "react";
import { FaCrown, FaTools, FaChartBar, FaTruck } from "react-icons/fa";
import { FaBellConcierge } from "react-icons/fa6";

const rolesData = [
  {
    key: "admin",
    name: "Admin",
    icon: <FaCrown className="text-yellow-500" />,
    permissions: [
      "All permissions",
      "User management",
      "System configuration",
      "View logs",
    ],
  },
  {
    key: "dispatcher",
    name: "Dispatcher",
    icon: <FaBellConcierge className="text-pink-500" />,
    permissions: ["Create order", "Assign vehicle & driver", "View map", "Order management"],
  },
  {
    key: "fleetManager",
    name: "Fleet Manager",
    icon: <FaTools className="text-blue-500" />,
    permissions: [
      "Vehicle management",
      "Schedule maintenance",
      "View vehicle reports",
      "Update vehicle status",
    ],
  },
  {
    key: "operationsManager",
    name: "Operations Manager",
    icon: <FaChartBar className="text-green-600" />,
    permissions: [
      "View reports",
      "Data analysis",
      "Export reports",
      "Monitor KPI",
    ],
  },
  {
    key: "driver",
    name: "Driver",
    icon: <FaTruck className="text-orange-500" />,
    permissions: [
      "Receive new order",
      "Update delivery status",
      "View route",
      "Send receipt image",
      "E-signature",
    ],
  },
];

const allPermissions = Array.from(
  new Set(rolesData.flatMap((r) => r.permissions))
);

// Permission descriptions in English
const permissionDescriptions: Record<string, string> = {
  "All permissions": "Full system administration rights.",
  "User management": "Create, edit, and delete user accounts.",
  "System configuration": "Change system configuration parameters.",
  "View logs": "View system activity history.",
  "Create order": "Create a new transport order.",
  "Assign vehicle & driver": "Assign vehicle and driver to an order.",
  "View map": "View transport route map.",
  "Order management": "Manage transport orders.",
  "Vehicle management": "Manage vehicle list and status.",
  "Schedule maintenance": "Schedule vehicle maintenance.",
  "View vehicle reports": "View vehicle-related reports.",
  "Update vehicle status": "Update vehicle operational status.",
  "View reports": "View summary reports.",
  "Data analysis": "Analyze operational data.",
  "Export reports": "Export reports to file.",
  "Monitor KPI": "Monitor key performance indicators.",
  "Receive new order": "Receive new delivery orders.",
  "Update delivery status": "Update delivery order status.",
  "View route": "View delivery route.",
  "Send receipt image": "Send receipt image upon delivery.",
  "E-signature": "Electronically sign for delivery confirmation.",
};

export default function RoleTable() {
  const [roles, setRoles] = useState(rolesData);
  const [editingRole, setEditingRole] = useState<null | typeof roles[0]>(null);
  const [tempPermissions, setTempPermissions] = useState<string[]>([]);
  const [selectedPermission, setSelectedPermission] = useState<string | null>(null);

  const handlePermissionClick = (_roleName: string, permission: string) => {
    setSelectedPermission(permission);
  };

  const handleEditRole = (role: typeof roles[0]) => {
    setEditingRole(role);
    setTempPermissions(role.permissions);
  };

  const handleTogglePermission = (perm: string) => {
    setTempPermissions((prev) =>
      prev.includes(perm)
        ? prev.filter((p) => p !== perm)
        : [...prev, perm]
    );
  };

  const handleSavePermissions = () => {
    if (!editingRole) return;
    setRoles((prev) =>
      prev.map((r) =>
        r.key === editingRole.key ? { ...r, permissions: tempPermissions } : r
      )
    );
    setEditingRole(null);
    setTempPermissions([]);
  };

  return (
    <div className="p-0">
      <div className="bg-white border border-gray-200 rounded-2xl p-8 ">
        <h2 className="text-2xl font-bold mb-8">Role Permissions</h2>
        <div className="flex flex-col gap-8">
          {roles.map((role) => (
            <div
              key={role.key}
              className="bg-white rounded-xl shadow flex flex-col md:flex-row md:items-center justify-between py-6 px-8"
            >
              <div>
                <div className="flex items-center gap-2 text-xl font-semibold mb-4 md:mb-2">
                  <span>{role.icon}</span>
                  {role.name}
                </div>
                <div className="flex flex-wrap gap-3">
                  {role.permissions.map((p) => (
                    <button
                      key={p}
                      className="bg-gray-100 px-4 py-1 rounded-full text-sm font-medium hover:bg-blue-100 transition"
                      onClick={() => handlePermissionClick(role.name, p)}
                      type="button"
                    >
                      {p}
                    </button>
                  ))}
                </div>
                {/* Show permission description if selected */}
                {selectedPermission && role.permissions.includes(selectedPermission) && (
                  <div className="mt-4 text-gray-600 bg-blue-50 rounded px-4 py-2">
                    <span className="font-semibold">{selectedPermission}:</span>{" "}
                    {permissionDescriptions[selectedPermission] || "No description."}
                  </div>
                )}
              </div>
              <button
                className="mt-6 md:mt-0 px-5 py-2 rounded border font-semibold hover:bg-gray-100"
                onClick={() => handleEditRole(role)}
                type="button"
              >
                Edit
              </button>
            </div>
          ))}
        </div>

        {/* Edit permissions modal */}
        {editingRole && (
          <div className="fixed inset-0 bg-black/30 flex items-center justify-center z-50">
            <div className="bg-white rounded-xl shadow-lg p-8 w-full max-w-md">
              <h2 className="text-xl font-bold mb-4">
                Set permissions for role: {editingRole.name}
              </h2>
              <div className="flex flex-wrap gap-2 mb-4">
                {allPermissions.map((perm) => (
                  <button
                    key={perm}
                    className={`px-3 py-1 rounded-full text-sm font-medium border
                      ${
                        tempPermissions.includes(perm)
                          ? "bg-blue-500 text-white"
                          : "bg-gray-100 text-black"
                      }
                    `}
                    onClick={() => handleTogglePermission(perm)}
                    type="button"
                  >
                    {perm}
                  </button>
                ))}
              </div>
              <div className="flex gap-2 justify-end">
                <button
                  className="px-4 py-2 rounded bg-gray-200"
                  onClick={() => setEditingRole(null)}
                >
                  Cancel
                </button>
                <button
                  className="px-4 py-2 rounded bg-teal-600 text-white font-bold"
                  onClick={handleSavePermissions}
                >
                  Save
                </button>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}