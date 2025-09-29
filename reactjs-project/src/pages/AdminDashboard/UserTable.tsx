import { useEffect, useState } from "react";
import { useTranslation } from 'react-i18next';
import UserForm from "./UserForm";
import {
  fetchUsers,
  addUser,
  editUser as apiEditUser,
  deleteUser as apiDeleteUser,
} from "../../services/adminAPI";
import { FiEdit, FiEye, FiTrash2 } from "react-icons/fi";

// Helper function to translate role names
const translateRole = (rawRole: string, t: any): string => {
  const normalizedRole = rawRole.replace(/[\s_]+/g, "").toLowerCase();
  
  switch (normalizedRole) {
    case "admin":
      return t('operations.staff.roles.admin', 'Admin');
    case "dispatcher":
      return t('operations.staff.roles.dispatcher', 'Dispatcher');
    case "fleetmanager":
    case "fleet":
      return t('operations.staff.roles.fleetManager', 'Fleet Manager');
    case "driver":
      return t('operations.staff.roles.driver', 'Driver');
    case "customer":
      return t('operations.staff.roles.customer', 'Customer');
    case "operationsmanager":
    case "operations":
      return t('operations.staff.roles.operationsManager', 'Operations Manager');
    default:
      return rawRole;
  }
};

// Helper function to get role color class
const getRoleColorClass = (rawRole: string): string => {
  const normalizedRole = rawRole.replace(/[\s_]+/g, "").toLowerCase();
  
  switch (normalizedRole) {
    case "admin":
      return "text-yellow-700";
    case "dispatcher":
      return "text-purple-700";
    case "fleetmanager":
    case "fleet":
      return "text-blue-700";
    case "driver":
      return "text-amber-700";
    case "customer":
      return "text-emerald-700";
    case "operationsmanager":
    case "operations":
      return "text-pink-700";
    default:
      return "text-gray-800";
  }
};

interface UserTableProps {
  onUserCountUpdate?: () => void;
}

// Định nghĩa interface cho user để dùng cho state
interface DashboardUser {
  id: number;
  name: string;
  email: string;
  role: string;
  roleValue?: string;
  roleIcon: any;
  status: string;
  lastLogin: string;
  phone?: string;
  password?: string;
  notes?: string | null;
  googleId?: string | null;
}

export default function UserTable({ onUserCountUpdate }: UserTableProps) {
  const { t } = useTranslation();
  
  // Helper to map API role to display name and icon
  function getRoleDisplay(roleName: string) {
    switch (roleName) {
      case "DISPATCHER":
        return { label: t('admin.roles.dispatcher', 'Dispatcher'), icon: null };
      case "FLEET_MANAGER":
      case "FLEET":
        return { label: t('admin.roles.fleetManager', 'Fleet Manager'), icon: null };
      case "DRIVER":
        return { label: t('admin.roles.driver', 'Driver'), icon: null };
      case "ADMIN":
        return { label: t('admin.roles.admin', 'Admin'), icon: null };
      case "OPERATIONS_MANAGER":
      case "OPERATIONS":
        return { label: t('admin.roles.operationsManager', 'Operations Manager'), icon: null };
      case "CUSTOMER":
        return { label: t('admin.roles.customer', 'Customer'), icon: null };
      default:
        return { label: roleName, icon: null };
    }
  }
  const [users, setUsers] = useState<DashboardUser[]>([]);
  const [search, setSearch] = useState("");
  const [showForm, setShowForm] = useState(false);
  const [editUser, setEditUser] = useState<{
    name: string;
    email: string;
    role: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    roleIcon: any;
    status: string;
    lastLogin: string;
  } | null>(null);
  const [viewUser, setViewUser] = useState<{
    name: string;
    email: string;
    role: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    roleIcon: any;
    status: string;
    lastLogin: string;
  } | null>(null);
  const [fetchError, setFetchError] = useState<string>("");

  useEffect(() => {
    fetchUsers()
      .then((data) => {
        console.log("[UserTable] API data:", data);
        setFetchError("");
        if (!Array.isArray(data)) {
          setFetchError(
            t('errors.invalidApiResponse', 'API did not return valid user data')
          );
          setUsers([]);
          return;
        }
        setUsers(
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          data.map((u: any) => {
            const roleInfo = getRoleDisplay(u.role?.roleName || "");
            const status =
              u.status?.name?.toLowerCase() === "active"
                ? "active"
                : u.status?.name?.toLowerCase() || "inactive";
            const lastLogin = u.updatedAt
              ? new Date(u.updatedAt).toLocaleString()
              : "-";
            // Map roleName từ backend về translated display name
            const roleValue = translateRole(u.role?.roleName || "", t);
            return {
              id: u.id,
              name: u.fullName || u.username || "",
              email: u.email,
              role: roleInfo.label,
              roleValue,
              roleIcon: roleInfo.icon,
              status,
              lastLogin,
              phone: u.phone || "",
              password: u.password || "",
            };
          })
        );
      })
      .catch((err) => {
        console.error("[UserTable] Fetch users error:", err);
        setFetchError(
          t('errors.fetchUsersFailed', 'Failed to fetch user data. Please try again or check backend.')
        );
        setUsers([]);
      });
  }, []);

  // Pagination state
  const [currentPage, setCurrentPage] = useState(1);
  const rowsPerPage = 20;

  const filtered = users.filter(
    (u) =>
      u.name.toLowerCase().includes(search.toLowerCase()) ||
      u.email.toLowerCase().includes(search.toLowerCase())
  );

  // Pagination logic
  const totalPages = Math.ceil(filtered.length / rowsPerPage);
  const paginated = filtered.slice(
    (currentPage - 1) * rowsPerPage,
    currentPage * rowsPerPage
  );

  const handleAddUser = async (user: {
    name: string;
    email: string;
    role: string;
    roleIcon: React.ReactNode;
    status: string;
    lastLogin: string;
  }) => {
    try {
      // Map role string sang id theo bảng roles thực tế
      const roleMap: Record<string, number> = {
        DISPATCHER: 1,
        ADMIN: 2,
        OPERATIONS_MANAGER: 3,
        CUSTOMER: 5,
        FLEET_MANAGER: 6,
        DRIVER: 7,
      };
      const statusMap: Record<string, number> = {
        active: 7,
        inactive: 8,
        suspended: 9,
      };
      // Chuẩn hóa key role: "Admin" => "ADMIN", "Fleet Manager" => "FLEET_MANAGER"
      const roleKey = user.role.replace(/ /g, "_").toUpperCase();
      const payload = {
        username: user.email.split("@")[0],
        fullName: user.name,
        email: user.email,
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        password: (user as any).password || "", // lấy từ form
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        phone: (user as any).phone || "",
        role: { id: roleMap[roleKey] },
        status: { id: statusMap[user.status] || 1 },
      };
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      await addUser(payload as any);
      // Sau khi thêm thành công, reload lại danh sách user từ API
      const data = await fetchUsers();
      setUsers(
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        data.map((u: any) => {
          const roleInfo = getRoleDisplay(u.role?.roleName || "");
          const status =
            u.status?.name?.toLowerCase() === "active"
              ? "active"
              : u.status?.name?.toLowerCase() || "inactive";
          const lastLogin = u.updatedAt
            ? new Date(u.updatedAt).toLocaleString()
            : "-";
          // Map roleName từ backend về đúng value của select
          let roleValue = "";
          const rawRole = (u.role?.roleName || "")
            .replace(/[\s_]+/g, "")
            .toLowerCase();
          roleValue = translateRole(u.role?.roleName || "", t);
          return {
            id: u.id,
            name: u.fullName || u.username || "",
            email: u.email,
            role: roleInfo.label,
            roleValue,
            roleIcon: roleInfo.icon,
            status,
            lastLogin,
            phone: u.phone || "",
            password: u.password || "",
          };
        })
      );
      // Call callback to update parent dashboard stats
      if (onUserCountUpdate) {
        onUserCountUpdate();
      }
      // eslint-disable-next-line @typescript-eslint/no-unused-vars
    } catch (err) {
      alert(t('admin.userTable.errors.addUser', 'Lỗi khi thêm user mới. Vui lòng thử lại!'));
    }
  };

  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const handleEditUser = (user: any) => {
    setEditUser({
      ...user,
      roleValue: user.roleValue,
      status: user.status === "active" ? "active" : "inactive",

    }); // Đảm bảo có trường roleValue và status đúng cho form
    setShowForm(true);
  };

  const handleUpdateUser = async (updatedUser: {
    name: string;
    email: string;
    role: string;
    roleIcon: React.ReactNode;
    status: string;
    lastLogin: string;
    password?: string;
    phone?: string;
  }) => {
    try {
      console.log("[UserTable] handleUpdateUser called with:", updatedUser);

      // Map role string sang id theo bảng roles thực tế
      const roleMap: Record<string, number> = {
        DISPATCHER: 1,
        ADMIN: 2,
        OPERATIONS_MANAGER: 3,
        CUSTOMER: 5,
        FLEET_MANAGER: 6,
        DRIVER: 7,
      };
      const statusMap: Record<string, number> = {
        active: 7,
        inactive: 8,
        suspended: 9,
      };
      // Chuẩn hóa key role: "Admin" => "ADMIN", "Fleet Manager" => "FLEET_MANAGER"
      const roleKey = updatedUser.role.replace(/ /g, "_").toUpperCase();
      console.log(
        "[UserTable] roleKey:",
        roleKey,
        "roleMap[roleKey]:",
        roleMap[roleKey]
      );

      // Tìm user gốc để lấy id và các trường không sửa
      const userOrigin = users.find((u) => u.email === updatedUser.email);
      if (!userOrigin) {
        console.error(
          "[UserTable] User not found with email:",
          updatedUser.email
        );
        throw new Error(t('errors.userNotFound', 'User not found'));
      }
      console.log("[UserTable] userOrigin:", userOrigin);

      // Tạo payload đầy đủ cho PUT
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const payload: any = {
        id: userOrigin.id,
        username: updatedUser.email.split("@")[0],
        fullName: updatedUser.name,
        email: updatedUser.email,
        password:
          updatedUser.password && updatedUser.password.trim() !== ""
            ? updatedUser.password
            : userOrigin.password || "",
        phone:
          updatedUser.phone && updatedUser.phone.trim() !== ""
            ? updatedUser.phone
            : userOrigin.phone || "",
        role: { id: roleMap[roleKey] },
        status: { id: statusMap[updatedUser.status] || 7 },
        notes: userOrigin.notes || null,
        googleId: userOrigin.googleId || null,
      };
      console.log("[UserTable] payload for update:", payload);

      await apiEditUser(userOrigin.id, payload);
      console.log("[UserTable] Update successful, reloading users...");
      // Reload lại danh sách user
      const data = await fetchUsers();
      setUsers(
        // eslint-disable-next-line @typescript-eslint/no-explicit-any
        data.map((u: any) => {
          const roleInfo = getRoleDisplay(u.role?.roleName || "");
          const statusRaw = u.status?.name?.toLowerCase() || "";
          const status = statusRaw === "active" ? "active" : "inactive";
          const lastLogin = u.updatedAt
            ? new Date(u.updatedAt).toLocaleString()
            : "-";
          // Map roleName từ backend về đúng value của select
          let roleValue = "";
          const rawRole = (u.role?.roleName || "")
            .replace(/[\s_]+/g, "")
            .toLowerCase();
          roleValue = translateRole(u.role?.roleName || "", t);
          return {
            id: u.id,
            name: u.fullName || u.username || "",
            email: u.email,
            role: roleInfo.label,
            roleValue,
            roleIcon: roleInfo.icon,
            status,
            lastLogin,
            phone: u.phone || "",
            password: u.password || "",
          };
        })
      );
      setShowForm(false);
      setEditUser(null);
    } catch (err) {
      console.error("[UserTable] Update error:", err);
      alert(
        t('admin.userTable.errors.updateUser', 'Lỗi khi cập nhật user: {{error}}. Vui lòng thử lại!', {
          error: err instanceof Error ? err.message : t('errors.unknownError', 'Unknown error')
        })
      );
    }
  };

  const handleDeleteUser = async (email: string) => {
    const user = users.find((u) => u.email === email);
    if (!user) return;
    if (window.confirm("Are you sure you want to delete this user?")) {
      try {
        await apiDeleteUser(user.id);
        // Reload lại danh sách user
        const data = await fetchUsers();
        setUsers(
          // eslint-disable-next-line @typescript-eslint/no-explicit-any
          data.map((u: any) => {
            const roleInfo = getRoleDisplay(u.role?.roleName || "");
            const status =
              u.status?.name?.toLowerCase() === "active"
                ? "active"
                : "inactive";
            const lastLogin = u.updatedAt
              ? new Date(u.updatedAt).toLocaleString()
              : "-";
            // Map roleName từ backend về đúng value của select
            let roleValue = "";
            const rawRole = (u.role?.roleName || "")
              .replace(/[\s_]+/g, "")
              .toLowerCase();
            roleValue = translateRole(u.role?.roleName || "", t);
            return {
              id: u.id,
              name: u.fullName || u.username || "",
              email: u.email,
              role: roleInfo.label,
              roleValue,
              roleIcon: roleInfo.icon,
              status,
              lastLogin,
              phone: u.phone || "",
              password: u.password || "",
            };
          })
        );
        // Call callback to update parent dashboard stats
        if (onUserCountUpdate) {
          onUserCountUpdate();
        }
        // eslint-disable-next-line @typescript-eslint/no-unused-vars
      } catch (err) {
        alert(t('admin.userTable.errors.deleteUser', 'Lỗi khi xóa user. Vui lòng thử lại!'));
      }
    }
  };

  return (
    <div className="bg-white rounded-xl shadow p-6">
      <div className="flex flex-col sm:flex-row gap-4 sm:gap-0 justify-between items-start sm:items-center mb-4">
        <input
          className="border rounded px-4 py-2 w-full sm:w-72"
          placeholder={t('admin.userTable.searchPlaceholder', 'Search user...')}
          value={search}
          onChange={(e) => setSearch(e.target.value)}
        />
        <button
          className="bg-black text-white px-4 py-2 rounded font-bold flex items-center gap-2 hover:bg-gray-800 w-full sm:w-auto justify-center"
          onClick={() => {
            setShowForm(true);
            setEditUser(null);
          }}
        >
          <span className="text-xl">+</span> <span className="hidden sm:inline">{t('admin.userTable.addUser', 'Add User')}</span><span className="sm:hidden">{t('admin.userTable.add', 'Add')}</span>
        </button>
      </div>
      {showForm && (
        <UserForm
          onAdd={editUser ? handleUpdateUser : handleAddUser}
          onClose={() => {
            setShowForm(false);
            setEditUser(null);
          }}
          user={editUser}
        />
      )}
      <div>
        <h2 className="text-2xl font-bold mb-5">{t('admin.userTable.title', 'User List')}</h2>
        {fetchError && (
          <div className="mb-4 p-3 bg-red-100 text-red-700 rounded">
            {fetchError}
          </div>
        )}
        <div className="overflow-x-auto">
          <table className="min-w-full">
            <thead>
              <tr className="text-left text-gray-600 border-b">
                <th className="py-2 pr-4">{t('admin.userTable.headers.fullName', 'Full Name')}</th>
                <th className="py-2 pr-4">{t('admin.userTable.headers.email', 'Email')}</th>
                <th className="py-2 pr-4">{t('admin.userTable.headers.role', 'Role')}</th>
                <th className="py-2 pr-4">{t('admin.userTable.headers.status', 'Status')}</th>
                <th className="py-2 pr-4 hidden sm:table-cell">{t('admin.userTable.headers.lastLogin', 'Last Login')}</th>
                <th className="py-2 pr-4">{t('admin.userTable.headers.actions', 'Actions')}</th>
              </tr>
            </thead>
            <tbody>
              {paginated.map((u, idx) => (
                <tr key={u.id || idx} className="border-b hover:bg-gray-50">
                  <td className="py-3 pr-4 font-medium">
                    <div className="truncate max-w-32 sm:max-w-none">{u.name}</div>
                  </td>
                  <td className="py-3 pr-4">
                    <div className="truncate max-w-48 sm:max-w-none">{u.email}</div>
                  </td>
                  <td className="py-3 pr-4">
                    <span
                      className={`inline-flex items-center gap-1 px-3 py-1 rounded-full text-sm font-semibold ${getRoleColorClass(u.roleValue || "")}`}
                    >
                      {u.roleIcon && <span>{u.roleIcon}</span>}
                      <span className="hidden sm:inline">{u.role}</span>
                      <span className="sm:hidden">{u.role.substring(0, 4)}</span>
                    </span>
                  </td>
                  <td className="py-3 pr-4">
                    {u.status === "active" ? (
                      <span className="inline-flex items-center px-2 sm:px-4 py-1 rounded-full bg-white border border-green-300 text-green-700 text-sm font-semibold gap-2">
                        <span className="w-4 h-4 rounded-full border border-green-300 flex items-center justify-center">
                          <span className="w-2.5 h-2.5 rounded-full bg-green-400 block"></span>
                        </span>
                        <span className="hidden sm:inline">Active</span>
                        <span className="sm:hidden">✓</span>
                      </span>
                    ) : (
                      <span className="inline-flex items-center px-2 sm:px-4 py-1 rounded-full bg-white border border-gray-300 text-gray-500 text-sm font-semibold gap-2">
                        <span className="w-4 h-4 rounded-full border border-gray-300 flex items-center justify-center">
                          <span className="w-2.5 h-2.5 rounded-full bg-gray-400 block"></span>
                        </span>
                        <span className="hidden sm:inline">Inactive</span>
                        <span className="sm:hidden">✗</span>
                      </span>
                    )}
                  </td>
                  <td className="py-3 pr-4 hidden sm:table-cell">{u.lastLogin}</td>
                  <td className="py-3 pr-4 flex gap-1 sm:gap-2">
                    <button
                      className="p-2 rounded hover:bg-gray-200"
                      title={t('common.edit', 'Edit')}
                      onClick={() => handleEditUser(u)}
                    >
                      <FiEdit size={16} className="sm:w-[18px] sm:h-[18px]" />
                    </button>
                    <button
                      className="p-2 rounded hover:bg-gray-200"
                      title={t('common.view', 'View')}
                      onClick={() => setViewUser(u)}
                    >
                      <FiEye size={16} className="sm:w-[18px] sm:h-[18px]" />
                    </button>
                    <button
                      className="p-2 rounded hover:bg-red-100"
                      title={t('common.delete', 'Delete')}
                      onClick={() => handleDeleteUser(u.email)}
                    >
                      <FiTrash2 size={16} className="sm:w-[18px] sm:h-[18px]" color="#ef4444" />{" "}
                      {/* Màu đỏ cho icon xóa */}
                    </button>
                  </td>
                </tr>
              ))}
              {filtered.length === 0 && (
                <tr>
                  <td colSpan={6} className="py-6 text-center text-gray-400 sm:table-cell">
                    <span className="hidden sm:inline">No users found.</span>
                    <span className="sm:hidden">Không có user</span>
                  </td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
        {/* Pagination controls */}
        {totalPages > 1 && (
          <div className="flex justify-center items-center gap-2 mt-4">
            <button
              className="px-3 py-1 rounded border bg-gray-100 disabled:opacity-50"
              onClick={() => setCurrentPage((p) => Math.max(1, p - 1))}
              disabled={currentPage === 1}
            >
              Previous
            </button>
            <span className="mx-2">
              Page {currentPage} / {totalPages}
            </span>
            <button
              className="px-3 py-1 rounded border bg-gray-100 disabled:opacity-50"
              onClick={() => setCurrentPage((p) => Math.min(totalPages, p + 1))}
              disabled={currentPage === totalPages}
            >
              Next
            </button>
          </div>
        )}
      </div>
      {/* User detail modal */}
      {viewUser && (
        <div className="fixed inset-0 bg-black/30 flex items-center justify-center z-50">
          <div className="bg-white rounded-xl shadow-lg p-8 w-full max-w-md">
            <h2 className="text-xl font-bold mb-2">User Information</h2>
            <div>
              <b>Name:</b> {viewUser.name}
            </div>
            <div>
              <b>Email:</b> {viewUser.email}
            </div>
            <div>
              <b>Role:</b> {viewUser.role}
            </div>
            <div>
              <b>Status:</b>{" "}
              {viewUser.status === "active" ? t('common.active', 'Active') : t('common.inactive', 'Inactive')}
            </div>
            <div>
              <b>Last Login:</b> {viewUser.lastLogin}
            </div>
            <button
              className="mt-4 px-4 py-2 rounded bg-teal-600 text-white"
              onClick={() => setViewUser(null)}
            >
              Close
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
