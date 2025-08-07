import { useState } from "react";
import type { User } from "../../types/User";
import UserTable from "./UserTable";
import RoleTable from "./RoleTable";
import SystemConfigForm from "./SystemConfigForm";
import AuditLogTable from "./AuditLogTable";
import { FiActivity, FiSettings } from "react-icons/fi";
import { AiOutlineSafetyCertificate, AiOutlineSetting } from "react-icons/ai";
import { MdManageAccounts } from "react-icons/md";
import { RiShieldKeyholeLine } from "react-icons/ri";
import { HiOutlineDocumentReport } from "react-icons/hi";

interface AdminDashboardProps {
  user: User;
  onLogout: () => void;
}

const MENU = [
  { key: "users", label: "User Management", icon: <MdManageAccounts className="text-xl" /> },
  { key: "roles", label: "Role Permissions", icon: <RiShieldKeyholeLine className="text-xl" /> },
  { key: "settings", label: "System Configuration", icon: <FiSettings className="text-xl" /> },
  { key: "logs", label: "System Logs", icon: <HiOutlineDocumentReport className="text-xl" /> },
];

export default function AdminDashboard({
  user,
  onLogout,
}: AdminDashboardProps) {
  const [active, setActive] = useState("users");

  // Demo stats, replace with real data if needed
  const stats = [
    {
      label: "Total Users",
      value: 4,
      icon: <MdManageAccounts className="text-4xl text-blue-500" />,
    },
    {
      label: "Active Users",
      value: 3,
      icon: <FiActivity className="text-4xl text-green-500" />,
    },
    {
      label: "Drivers",
      value: 1,
      icon: <AiOutlineSafetyCertificate className="text-4xl text-orange-500" />,
    },
    {
      label: "Managers",
      value: 2,
      icon: <AiOutlineSetting className="text-4xl text-purple-500" />,
    },
  ];

  return (
    <div className="h-screen flex bg-gray-50">
      {/* Sidebar */}
      <aside className="group flex-shrink-0 w-16 md:w-20 hover:w-56 md:hover:w-64 transition-all duration-200 bg-[#23272f] dark:bg-[#181c23] flex flex-col overflow-hidden">
        <div className="flex items-center gap-3 px-2 md:px-4 py-6 border-b border-[#23272f]">
          <div className="bg-teal-600 text-white rounded-full w-10 h-10 flex items-center justify-center text-2xl font-bold">
            K
          </div>
          <span
            className="hidden group-hover:inline-block font-bold text-lg text-white transition-all duration-200 whitespace-nowrap overflow-hidden"
            style={{ maxWidth: "200px" }}
          >
            Fast Route
          </span>
        </div>
        <nav className="flex-1 flex flex-col gap-1 mt-4">
          {MENU.map((item) => (
            <button
              key={item.key}
              className={`flex items-center gap-3 px-2 md:px-4 py-3 ml-3 text-base font-medium transition text-left ${
                active === item.key
                  ? "bg-[#2d3643] text-teal-400"
                  : "text-white hover:bg-[#2d3643]"
              }`}
              onClick={() => setActive(item.key)}
            >
              <span className="text-xl">{item.icon}</span>
              <span
                className="hidden group-hover:inline transition-all duration-200 whitespace-nowrap overflow-hidden"
                style={{ maxWidth: "160px" }}
              >
                {item.label}
              </span>
            </button>
          ))}
        </nav>
      </aside>
      {/* Main content */}
      <main className="flex-1 flex flex-col bg-gray-50 dark:bg-gray-900 overflow-y-auto h-screen">
        {/* Header */}
        <header className="flex flex-col md:flex-row items-start md:items-center justify-between px-6 py-4 bg-white dark:bg-gray-800 border-b">
          <div>
            <h1 className="text-2xl md:text-3xl font-bold text-gray-800">
              {active === "users" && "User Management Dashboard"}
              {active === "roles" && "Role Permissions Dashboard"}
              {active === "settings" && "System Configuration Dashboard"}
              {active === "logs" && "System Logs Dashboard"}
            </h1>
            <p className="text-gray-500 text-sm md:text-base mt-1">
              {active === "users" && "Create, edit, and delete user accounts"}
              {active === "roles" && "Manage access permissions for each role"}
              {active === "settings" && "Configure system parameters"}
              {active === "logs" && "Monitor system activities"}
            </p>
          </div>
          <div className="flex items-center gap-2 md:gap-4 mt-4 md:mt-0">
            <span className="text-gray-600 text-sm md:text-base">
              Hello, <span className="font-semibold">{user.name}</span>
            </span>
            <button
              onClick={onLogout}
              className="px-3 md:px-4 py-2 rounded bg-gray-100 text-gray-700 font-bold hover:bg-blue-100 transition text-sm md:text-base"
            >
              Logout
            </button>
          </div>
        </header>
        {/* Stats cards */}
        <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-4 gap-4 mt-6 md:mt-8 px-4 md:px-10">
          {stats.map((s, idx) => (
            <div
              key={idx}
              className="bg-white rounded-xl shadow px-6 py-5 flex flex-col justify-between h-full"
            >
              <div className="text-gray-600 text-base mb-2">{s.label}</div>
              <div className="flex items-center justify-between">
                <div className="text-2xl font-bold">{s.value}</div>
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
      </main>
    </div>
  );
}