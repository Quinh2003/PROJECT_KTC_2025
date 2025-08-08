import { useState } from "react";
import type { User } from "../../types/User";
import UserTable from "./UserTable";
import RoleTable from "./RoleTable";
import SystemConfigForm from "./SystemConfigForm";
import AuditLogTable from "./AuditLogTable";
import logo from "../../assets/logo.png";
import Navbar from "../../components/Navbar";
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
  {
    key: "users",
    label: "User Management",
    icon: <MdManageAccounts className="text-xl" />,
  },
  {
    key: "roles",
    label: "Role Permissions",
    icon: <RiShieldKeyholeLine className="text-xl" />,
  },
  {
    key: "settings",
    label: "System Configuration",
    icon: <FiSettings className="text-xl" />,
  },
  {
    key: "logs",
    label: "System Logs",
    icon: <HiOutlineDocumentReport className="text-xl" />,
  },
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
    <div className="min-h-screen flex bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
      {/* Sidebar */}
      <aside className="group flex-shrink-0 w-20 hover:w-64 transition-all duration-300 bg-white/20 backdrop-blur-lg border-r border-white/30 text-gray-800 flex flex-col py-6 px-4 overflow-hidden h-screen sticky top-0">
        <div className="mb-5 flex items-center -mt-3 -ml-3 gap-1">
          <div className="w-16 h-16 rounded-full flex items-center justify-center flex-shrink-0 overflow-hidden bg-white/30 backdrop-blur-sm border border-white/50">
            <img
              src={logo}
              alt="Logo"
              className="w-12 h-12 rounded-full object-cover"
            />
          </div>
          <span
            className="hidden group-hover:inline-block font-bold text-lg tracking-wide transition-all duration-300 whitespace-nowrap overflow-hidden text-gray-700"
            style={{ maxWidth: "200px" }}
          >
            Fast Route
          </span>
        </div>
        <nav className="flex-1 flex flex-col gap-4">
          {MENU.map((item) => (
            <button
              key={item.key}
              className={`flex items-center gap-3 font-semibold transition-all duration-300 rounded-xl p-4 ${
                active === item.key
                  ? "text-blue-600 bg-white/40 backdrop-blur-sm border border-white/50 shadow-lg"
                  : "hover:text-blue-600 hover:bg-white/20 backdrop-blur-sm"
              }`}
              onClick={() => setActive(item.key)}
            >
              <span className="text-2xl flex-shrink-0">{item.icon}</span>
              <span
                className="hidden group-hover:inline transition-all duration-300 whitespace-nowrap overflow-hidden"
                style={{ maxWidth: "160px" }}
              >
                {item.label}
              </span>
            </button>
          ))}
        </nav>
      </aside>
      {/* Main content */}
      <main className="flex-1 flex flex-col bg-transparent overflow-y-auto h-screen">
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
        {/* Stats cards */}
        <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-4 gap-4 mt-6 md:mt-8 px-4 md:px-10">
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
