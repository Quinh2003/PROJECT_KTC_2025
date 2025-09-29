import { useState, useEffect, useCallback } from "react";
import { useTranslation } from 'react-i18next';
import type { User } from "../../types/User";

import { fetchUsers, fetchActivityLogs } from "../../services/adminAPI";

// Simple user interface for dashboard display
interface DashboardUser {
  id: number;
  name: string;
  email: string;
  role: string;
  status: string;
  lastLogin: string;
  phone: string;
  password: string;
  roleIcon?: any;
}
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
  const { t } = useTranslation();
  const [active, setActive] = useState<AdminTab>("users");

  const [users, setUsers] = useState<DashboardUser[]>([]);
  const [auditCount, setAuditCount] = useState<number>(0);

  // Callback function for AuditLogTable to update audit count
  const handleAuditCountUpdate = useCallback((newCount: number) => {
    console.log("Updating audit count from", auditCount, "to", newCount);
    setAuditCount(newCount);
  }, [auditCount]);

  // Callback function for UserTable to update user count in real-time
  const handleUserCountUpdate = useCallback(async () => {
    try {
      const userData = await fetchUsers();
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      const mappedUsers: DashboardUser[] = userData.map((u: any) => {
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
    } catch (err) {
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
        const mappedUsers: DashboardUser[] = userData.map((u: any) => {
          let roleIcon = null;
          switch (u.role?.roleName) {
            case "DISPATCHER":
              roleIcon = <span style={{fontWeight: 'bold'}}>D</span>; break;
            case "FLEET_MANAGER":
              roleIcon = <span style={{fontWeight: 'bold'}}>F</span>; break;
            case "DRIVER":
              roleIcon = <span style={{fontWeight: 'bold'}}>Dr</span>; break;
            case "ADMIN":
              roleIcon = <span style={{fontWeight: 'bold'}}>A</span>; break;
            case "OPERATIONS_MANAGER":
              roleIcon = <span style={{fontWeight: 'bold'}}>O</span>; break;
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
      } catch (err: unknown) {
        console.error("Failed to fetch initial data:", err);
      }
    };

    // Initial fetch only - no auto-refresh
    fetchData();
  }, []);

  const uniqueRoles = Array.from(new Set(users.map(u => u.role)));
  const stats = [
    {
      label: t('dashboard.admin.stats.totalUsers', 'Total Users'),
      value: users.length.toLocaleString(),
      icon: <MdManageAccounts className="text-3xl text-blue-600" />,
    },
    {
      label: t('dashboard.admin.stats.totalRoles', 'Total Roles'),
      value: uniqueRoles.length.toLocaleString(),
      icon: <RiShieldKeyholeLine className="text-3xl text-green-600" />,
    },
    {
      label: t('dashboard.admin.stats.systemConfig', 'System Config'),
      value: "",
      icon: <AiOutlineSafetyCertificate className="text-3xl text-yellow-600" />,
    },
    {
      label: t('dashboard.admin.stats.auditEvents', 'Audit Events'),
      value: auditCount.toLocaleString(),
      icon: <HiOutlineDocumentReport className="text-3xl text-purple-600" />,
    },
  ];

  return (
    <div className="min-h-screen flex bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
      {/* Sidebar - Hidden on mobile, visible on md+ */}
      <div className="hidden md:block">
        <Sidebar
          activeTab={active}
          onTabChange={tab => setActive(tab as AdminTab)}
          role="admin"
        />
      </div>
      {/* Main content */}
      <main className="flex-1 flex flex-col bg-transparent min-h-screen w-full md:w-auto">
        {/* Header */}
        <Navbar
          user={user}
          onLogout={onLogout}
          title={active === "users"
            ? t('dashboard.admin.title.userManagement', 'User Management Dashboard')
            : active === "roles"
              ? t('dashboard.admin.title.rolePermissions', 'Role Permissions Dashboard')
              : active === "settings"
                ? t('dashboard.admin.title.systemConfiguration', 'System Configuration Dashboard')
                : active === "logs"
                  ? t('dashboard.admin.title.systemLogs', 'System Logs Dashboard')
                  : t('dashboard.admin.title.admin', 'Admin Dashboard')} 
          subtitle={t('dashboard.admin.subtitle', 'Manage system users, roles and configurations')}         
        />
        {/* Mobile Navigation - Tab bar at bottom for mobile */}
        <div className="md:hidden fixed bottom-0 left-0 right-0 bg-white/90 backdrop-blur-lg border-t border-white/30 px-4 py-2 z-50">
          <div className="flex justify-around items-center">
            <button
              onClick={() => setActive("users")}
              className={`flex flex-col items-center py-2 px-1 ${active === "users" ? "text-blue-600" : "text-gray-600"}`}
            >
              <MdManageAccounts className="text-xl mb-1" />
              <span className="text-xs">{t('dashboard.admin.tabs.users', 'Users')}</span>
            </button>
            <button
              onClick={() => setActive("roles")}
              className={`flex flex-col items-center py-2 px-1 ${active === "roles" ? "text-blue-600" : "text-gray-600"}`}
            >
              <RiShieldKeyholeLine className="text-xl mb-1" />
              <span className="text-xs">{t('dashboard.admin.tabs.roles', 'Roles')}</span>
            </button>
            <button
              onClick={() => setActive("settings")}
              className={`flex flex-col items-center py-2 px-1 ${active === "settings" ? "text-blue-600" : "text-gray-600"}`}
            >
              <AiOutlineSafetyCertificate className="text-xl mb-1" />
              <span className="text-xs">{t('dashboard.admin.tabs.settings', 'Config')}</span>
            </button>
            <button
              onClick={() => setActive("logs")}
              className={`flex flex-col items-center py-2 px-1 ${active === "logs" ? "text-blue-600" : "text-gray-600"}`}
            >
              <HiOutlineDocumentReport className="text-xl mb-1" />
              <span className="text-xs">{t('dashboard.admin.tabs.logs', 'Logs')}</span>
            </button>
          </div>
        </div>
        {/* Stats cards - Responsive grid */}
        <div className="flex-shrink-0 grid grid-cols-2 md:grid-cols-4 gap-2 md:gap-4 mt-3 md:mt-4 px-3 md:px-10">
          {stats.map((s, idx) => (
            <div
              key={idx}
              className="bg-white/40 backdrop-blur-lg border border-white/50 shadow-lg rounded-xl px-3 md:px-6 py-3 md:py-5 flex flex-col justify-between h-full transition-all duration-200 hover:scale-[1.03] hover:shadow-xl"
            >
              <div className="text-gray-700 text-xs md:text-base mb-1 md:mb-2 font-medium drop-shadow-sm">
                {s.label}
              </div>
              <div className="flex items-center justify-between">
                <div className="text-lg md:text-2xl font-bold text-blue-900 drop-shadow-sm">
                  {s.value}
                </div>
                <div className="flex items-center text-lg md:text-3xl">{s.icon}</div>
              </div>
            </div>
          ))}
        </div>

        {/* Content - Mobile optimized padding */}
        <div className="flex-1 pb-16 md:pb-0">
          <div className="p-3 md:p-10 pt-2 md:pt-4">
            {active === "users" && <UserTable onUserCountUpdate={handleUserCountUpdate} />}
            {active === "roles" && <RoleTable />}
            {active === "settings" && <SystemConfigForm />}
            {active === "logs" && <AuditLogTable onAuditCountUpdate={handleAuditCountUpdate} />}
          </div>
        </div>
      </main>
    </div>
  );
}
