import { useState } from "react";
import type { User } from "../../types/User";
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
  const [active, setActive] = useState<AdminTab>("users"); // Sử dụng AdminTab

  const stats = [
    {
      label: "Total Users",
      value: "2,345",
      icon: <MdManageAccounts className="text-3xl text-red-600" />,
    },
    {
      label: "Active Roles",
      value: "15",
      icon: <RiShieldKeyholeLine className="text-3xl text-red-500" />,
    },
    {
      label: "System Config",
      value: "98%",
      icon: <AiOutlineSafetyCertificate className="text-3xl text-red-600" />,
    },
    {
      label: "Audit Events",
      value: "1.2K",
      icon: <HiOutlineDocumentReport className="text-3xl text-red-500" />,
    },
  ];

  return (
    <div className="min-h-screen flex">
      {/* Sidebar */}
      <Sidebar
        activeTab={active}
        onTabChange={tab => setActive(tab as AdminTab)}
        role="admin"
      />
      {/* Main content */}
      <main className="flex-1 flex flex-col rounded-l-[32px] relative z-[60] -ml-10 h-screen bg-gradient-to-br from-gray-50 via-white to-gray-100">
        {/* Header - Fixed */}
        <div className="sticky top-0 z-40">
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
        </div>
        
        {/* Scrollable content area */}
        <div className="flex-1 overflow-y-auto rounded-bl-[32px]">
          {/* Stats cards */}
          <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-4 gap-4 mt-6 md:mt-8 px-4 md:px-10">
            {stats.map((s, idx) => (
              <div
                key={idx}
                className="bg-white border border-red-100 shadow-lg rounded-xl px-6 py-5 flex flex-col justify-between h-full transition-all duration-200 hover:scale-[1.03] hover:shadow-xl hover:bg-red-50"
              >
                <div className="text-gray-700 text-base mb-2 font-medium drop-shadow-sm">
                  {s.label}
                </div>
                <div className="flex items-center justify-between">
                  <div className="text-2xl font-bold text-red-600 drop-shadow-sm">
                    {s.value}
                  </div>
                  <div className="flex items-center">{s.icon}</div>
                </div>
              </div>
            ))}
          </div>
          {/* Content */}
          <div className="flex-1 p-4 md:p-10">
            {active === "users" && <UserTable />}
            {active === "roles" && <RoleTable />}
            {active === "settings" && <SystemConfigForm />}
            {active === "logs" && <AuditLogTable />}
          </div>
        </div>
      </main>
    </div>
  );
}