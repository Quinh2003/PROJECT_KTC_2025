import { useState, useEffect } from "react";
import type { User } from "../../types/User";
<<<<<<< HEAD
import { fetchUsers } from "../../services/adminAPI";
=======
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
}
>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00
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
<<<<<<< HEAD
  const [users, setUsers] = useState<User[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Chỉ fetchUsers khi lần đầu vào trang, còn lại cập nhật trực tiếp qua UserTable
  useEffect(() => {
    setLoading(true);
    fetchUsers()
      .then(data => {
        const mappedUsers = data.map((u: any) => {
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
=======
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
>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00
          return {
            id: u.id,
            name: u.fullName || u.username || "",
            email: u.email,
            role: u.role?.roleName || "",
            roleIcon,
            status: u.status?.name?.toLowerCase() === "active" ? "active" : "inactive",
            lastLogin: u.updatedAt ? new Date(u.updatedAt).toLocaleString() : "-",
            phone: u.phone || "",
<<<<<<< HEAD
            password: "",
          };
        });
        setUsers(mappedUsers);
        setError(null);
      })
      .catch(() => {
        setError("Failed to fetch users");
      })
      .finally(() => setLoading(false));
=======
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
>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00
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
      value: "",
      icon: <AiOutlineSafetyCertificate className="text-3xl text-yellow-600" />,
    },
    {
      label: "Audit Events",
      value: auditCount.toLocaleString(),
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
          title={active === "users"
            ? "User Management Dashboard"
            : active === "roles"
              ? "Role Permissions Dashboard"
              : active === "settings"
                ? "System Configuration Dashboard"
                : active === "logs"
                  ? "System Logs Dashboard"
                  : "Admin Dashboard"} subtitle={""}         
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
<<<<<<< HEAD
        {/* Content */}
        <div className="flex-1 p-4 md:p-10">
          {active === "users" && <UserTable users={users} setUsers={setUsers} />}
          {active === "roles" && <RoleTable />}
          {active === "settings" && <SystemConfigForm />}
          {active === "logs" && <AuditLogTable />}
=======
        {/* Content - Scrollable */}
        <div className="flex-1 overflow-y-auto">
          <div className="p-4 md:p-10 pt-3 md:pt-4">
            {active === "users" && <UserTable onUserCountUpdate={handleUserCountUpdate} />}
            {active === "roles" && <RoleTable />}
            {active === "settings" && <SystemConfigForm />}
            {active === "logs" && <AuditLogTable onAuditCountUpdate={handleAuditCountUpdate} />}
          </div>
>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00
        </div>
      </main>
    </div>
  );
}
