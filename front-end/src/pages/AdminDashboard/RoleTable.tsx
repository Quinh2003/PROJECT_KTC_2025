import { useState } from "react";
import { ROLES, ALL_PERMISSIONS, PERMISSION_DESCRIPTIONS } from "../../constants/roles";
import * as FaIcons from "react-icons/fa";
import { FaBellConcierge } from "react-icons/fa6";

function getIconComponent(iconName: string, className?: string) {
  if (iconName === "FaBellConcierge") return <FaBellConcierge className={className ?? ""} />;
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const Icon = (FaIcons as any)[iconName];
  return Icon ? <Icon className={className ?? ""} /> : null;
}

export default function RoleTable() {
  const [roles, setRoles] = useState(ROLES);
  const [editingRole, setEditingRole] = useState<null | typeof ROLES[0]>(null);
  const [tempPermissions, setTempPermissions] = useState<string[]>([]);
  const [selectedPermission, setSelectedPermission] = useState<string | null>(null);

  const handlePermissionClick = (_roleName: string, permission: string) => {
    setSelectedPermission(permission);
  };

  const handleEditRole = (role: typeof ROLES[0]) => {
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
                  <span>{getIconComponent(role.icon ?? "", "text-2xl")}</span>
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
                    {PERMISSION_DESCRIPTIONS[selectedPermission] || "No description."}
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
                {ALL_PERMISSIONS.map((perm) => (
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