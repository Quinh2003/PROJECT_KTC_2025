import { useState, useEffect, useCallback } from "react";
import type { User } from "../../types/User";
import { fetchUsers, fetchActivityLogs, type User as APIUser } from "../../services/adminAPI";
import UserTable from "./UserTable";
import RoleTable from "./RoleTable";
import SystemConfigForm from "./SystemConfigForm";
import AuditLogTable from "./AuditLogTable";
import Navbar from "../../components/Navbar";
import type { AdminTab } from "../../components/Sidebar";
import { MdManageAccounts } from "react-icons/md";
import { RiShieldKeyholeLine } from "react-icons/ri";
import { AiOutlineSafetyCertificate } from "react-icons/ai";
import { HiOutlineDocumentReport } from "react-icons/hi";
import Sidebar from "../../components/Sidebar";

interface AdminDashboardProps {
  user: User;
  onLogout: () => void;
}

export default function AdminDashboard({ user, onLogout }: AdminDashboardProps) {
  const [active, setActive] = useState<AdminTab>("users");
  const [users, setUsers] = useState<User[]>([]);
  const [auditCount, setAuditCount] = useState<number>(0);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Helper function to format numbers (e.g., 1200 -> "1.2K")
  const formatNumber = (num: number): string => {
    if (num >= 1000000) {
      return (num / 1000000).toFixed(1).replace(/\.0$/, '') + 'M';
    }
    if (num >= 1000) {
      return (num / 1000).toFixed(1).replace(/\.0$/, '') + 'K';
    }
    return num.toString();
  };

  // Callback function for AuditLogTable to update audit count
  const handleAuditCountUpdate = useCallback((newCount: number) => {
    console.log("Updating audit count from", auditCount, "to", newCount);
    setAuditCount(newCount);
  }, [auditCount]);

  // Chỉ fetchUsers khi lần đầu vào trang, còn lại cập nhật trực tiếp qua UserTable
  useEffect(() => {
    const fetchData = async () => {
      setLoading(true);
      
      try {
        // Fetch users
        const userData = await fetchUsers();
        const mappedUsers = userData.map((u: APIUser) => {
          return {
            id: typeof u.id === 'string' ? parseInt(u.id) : u.id,
            name: u.fullName || u.username || "",
            email: u.email,
            role: u.role?.roleName || "",
            status: u.status?.name?.toLowerCase() === "active" ? "active" : "inactive",
            lastLogin: "-", // Will be updated from backend later
            phone: u.phone || "",
            password: u.password || "",
          } as User;
        });
        setUsers(mappedUsers);

        // Fetch audit logs count
        const logs = await fetchActivityLogs();
        console.log("Initial fetch - audit logs:", logs.length, "logs");
        setAuditCount(logs.length);
        
        setError(null);
      } catch (err: unknown) {
        console.error("Failed to fetch initial data:", err);
        setError("Failed to fetch data");
      } finally {
        setLoading(false);
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
      icon: <MdManageAccounts className="text-3xl text-blue-600" />,
    },
    {
      label: "Total Roles",
      value: uniqueRoles.length.toLocaleString(),
      icon: <RiShieldKeyholeLine className="text-3xl text-green-600" />,
    },
    {
      label: "System Config",
      value: "98%",
      icon: <AiOutlineSafetyCertificate className="text-3xl text-yellow-600" />,
    },
    {
      label: "Audit Events",
      value: formatNumber(auditCount),
      icon: <HiOutlineDocumentReport className="text-3xl text-purple-600" />,
    },
  ];

  return (
    <div className="min-h-screen flex bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
      {/* Sidebar */}
      <Sidebar
        activeTab={active}
        onTabChange={tab => setActive(tab as AdminTab)}
        role="admin"
      />
      {/* Main content */}
      <main className="flex-1 flex flex-col bg-transparent h-screen">
        {/* Header */}
        <Navbar
          user={user}
          onLogout={onLogout}
          title={
            active === "users"
              ? "User Management Dashboard"
              : active === "roles"
              ? "Role Permissions Dashboard"
              : active === "settings"
              ? "System Configuration Dashboard"
              : active === "logs"
              ? "System Logs Dashboard"
              : "Admin Dashboard"
          }
          subtitle={
            active === "users"
              ? "Create, edit, and delete user accounts"
              : active === "roles"
              ? "Manage access permissions for each role"
              : active === "settings"
              ? "Configure system parameters"
              : active === "logs"
              ? "Monitor system activities"
              : ""
          }
        />
        {/* Stats cards - Fixed */}
        <div className="flex-shrink-0 grid grid-cols-1 sm:grid-cols-2 md:grid-cols-4 gap-4 mt-3 md:mt-4 px-4 md:px-10">
          {stats.map((s, idx) => (
            <div
              key={idx}
              className="bg-white/40 backdrop-blur-lg border border-white/50 shadow-lg rounded-xl px-6 py-5 flex flex-col justify-between h-full transition-all duration-200 hover:scale-[1.03] hover:shadow-xl"
            >
              <div className="text-gray-700 text-base mb-2 font-medium drop-shadow-sm">
                {s.label}
              </div>
              <div className="flex items-center justify-between">
                <div className="text-2xl font-bold text-blue-900 drop-shadow-sm">
                  {s.value}
                </div>
                <div className="flex items-center">{s.icon}</div>
              </div>
            </div>
          ))}
        </div>
        {/* Content - Scrollable */}
        <div className="flex-1 overflow-y-auto">
          <div className="p-4 md:p-10 pt-3 md:pt-4">
            {active === "users" && <UserTable users={users} setUsers={setUsers} />}
            {active === "roles" && <RoleTable />}
            {active === "settings" && <SystemConfigForm />}
            {active === "logs" && <AuditLogTable onAuditCountUpdate={handleAuditCountUpdate} />}
          </div>
        </div>
      </main>
    </div>
  );
}
