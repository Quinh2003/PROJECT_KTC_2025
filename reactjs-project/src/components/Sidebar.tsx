import { MdManageAccounts } from "react-icons/md";
import { RiShieldKeyholeLine } from "react-icons/ri";
import { AiOutlineSetting, AiOutlineSafetyCertificate } from "react-icons/ai";
import { FiActivity } from "react-icons/fi";
import { HiOutlineDocumentReport } from "react-icons/hi";
import logo from "../assets/logo.webp";

export type DispatcherTab = "orders" | "resources" | "assignment";
export type OperationsTab = "overview" | "performance" | "monitoring" | "staff";
export type AdminTab = "users" | "roles" | "settings" | "logs";
export type FleetTab = "vehicles" | "maintenance" | "schedule";
export type TabType = DispatcherTab | OperationsTab | AdminTab | FleetTab;

export type UserRole = "dispatcher" | "operations" | "admin" | "fleet";

interface SidebarProps<T extends TabType> {
  activeTab: T;
  onTabChange: (tab: T) => void;
  role: UserRole;
}

interface MenuItem<T extends TabType> {
  key: T;
  label: string;
  icon: React.ReactNode;
}

const ALL_MENUS: Record<UserRole, MenuItem<any>[]> = {
  dispatcher: [
    { key: "orders", label: "Orders", icon: <MdManageAccounts /> },
    { key: "resources", label: "Resources", icon: <RiShieldKeyholeLine /> },
    { key: "assignment", label: "Assignment", icon: <AiOutlineSetting /> },
  ],
  operations: [
    { key: "overview", label: "Overview", icon: <MdManageAccounts /> },
    { key: "performance", label: "Performance", icon: <AiOutlineSafetyCertificate /> },
    { key: "monitoring", label: "Monitoring", icon: <HiOutlineDocumentReport /> },
    { key: "staff", label: "Staff", icon: <RiShieldKeyholeLine /> },
  ],
  fleet: [
    { key: "vehicles", label: "Quản lý phương tiện", icon: <MdManageAccounts /> },
    { key: "maintenance", label: "Bảo trì xe", icon: <AiOutlineSetting /> },
    { key: "schedule", label: "Lịch bảo trì", icon: <FiActivity /> },
  ],
  admin: [
    { key: "users", label: "User Management", icon: <MdManageAccounts /> },
    { key: "roles", label: "Role Permissions", icon: <RiShieldKeyholeLine /> },
    { key: "settings", label: "System Settings", icon: <AiOutlineSetting /> },
    { key: "logs", label: "Audit Logs", icon: <FiActivity /> },
  ],
};

function getMenu<T extends TabType>(role: UserRole): MenuItem<T>[] {
  return ALL_MENUS[role] as MenuItem<T>[];
}

export default function Sidebar<T extends TabType>({
  activeTab,
  onTabChange,
  role,
}: SidebarProps<T>) {
  const MENU = getMenu<T>(role);

  return (
    <div className="relative">
      <aside
        className="group flex-shrink-0 w-32 hover:w-64 transition-all duration-300 
                   bg-red-600 text-white 
                   flex flex-col py-6 px-4 overflow-hidden h-screen sticky top-0 z-50 shadow-2xl"
      >
        <div className="mb-5 flex items-center -mt-3 gap-1">
          <div className="w-17 h-17 flex items-center justify-center flex-shrink-0 overflow-hidden">
            <img
              src={logo}
              alt="Logo"
              className="w-12 h-12 object-cover"
            />
          </div>
          <span
            className="hidden group-hover:inline-block font-bold text-lg tracking-wide transition-all duration-300 whitespace-nowrap overflow-hidden text-white"
            style={{ maxWidth: "200px" }}
          >
            Fast Route
          </span>
        </div>
        <nav className="flex-1 flex flex-col gap-4">
          {MENU.map((item) => (
            <button
              key={item.key}
              className={`flex -ml-2 items-center gap-3 font-semibold transition-all duration-300 rounded-xl p-4 ${
                activeTab === item.key
                  ? "text-white bg-white/20"
                  : "hover:text-white hover:bg-red-700/50 text-white"
              }`}
              onClick={() => onTabChange(item.key)}
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
    </div>
  );
}