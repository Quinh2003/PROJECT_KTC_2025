import { MdInventory2, MdLocalShipping, MdDashboard, MdBarChart, MdPeople, MdSettings, MdManageAccounts } from "react-icons/md";
import { FaUserCog } from "react-icons/fa";
import logo from "../assets/logo.png";
import { RiShieldKeyholeLine } from "react-icons/ri";
import { AiOutlineSafetyCertificate } from "react-icons/ai";
import { HiOutlineDocumentReport } from "react-icons/hi";

// Define tab types for different dashboard roles
export type DispatcherTab = "orders" | "resources" | "assignment";
export type AdminTab = "users" | "roles" | "settings" | "logs";
export type OperationsTab = "overview" | "performance" | "monitoring" | "staff";

export type TabType = DispatcherTab | OperationsTab | AdminTab;

interface MenuItem<T extends TabType> {
    key: T;
    icon: React.ComponentType<{className?: string}>;
    label: string;
}

interface SidebarProps<T extends TabType> {
    activeTab: T;
    onTabChange: (tab: T) => void;
    dashboardType: "dispatcher" | "operations" | "admin";
}

export default function Sidebar<T extends TabType>({ activeTab, onTabChange, dashboardType }: SidebarProps<T>) {
    // Menu items for different dashboard types
    const getMenuItems = (): MenuItem<T>[] => {
        if (dashboardType === "dispatcher") {
            return [
                {
                    key: "orders" as T,
                    icon: MdInventory2,
                    label: "Order Management"
                },
                {
                    key: "assignment" as T,
                    icon: FaUserCog,
                    label: "Driver Assignment"
                },
                {
                    key: "resources" as T,
                    icon: MdLocalShipping,
                    label: "Resources"
                }
            ];
        } else if(dashboardType === "admin"){
            return [
                {
                    key: "users" as T,
                    icon: MdManageAccounts ,
                    label: "User Management"
                },
                {
                    key: "roles" as T,
                    icon: RiShieldKeyholeLine ,
                    label: "Role Permissions"
                },
                {
                    key: "settings" as T,
                    icon: AiOutlineSafetyCertificate ,
                    label: "System Settings"
                },
                {
                    key: "logs" as T,
                    icon: HiOutlineDocumentReport ,
                    label: "Audit Logs"
                }
            ];
        }else {
            return [
                {
                    key: "overview" as T,
                    icon: MdDashboard,
                    label: "Operations Overview"
                },
                {
                    key: "performance" as T,
                    icon: MdBarChart,
                    label: "Performance Analytics"
                },
                {
                    key: "monitoring" as T,
                    icon: MdSettings,
                    label: "Resource Monitoring"
                },
                {
                    key: "staff" as T,
                    icon: MdPeople,
                    label: "Staff Management"
                }
            ];
        }
    };

    const menuItems = getMenuItems();

    return (
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
                {menuItems.map((item) => {
                    const IconComponent = item.icon;
                    return (
                        <button
                            key={item.key}
                            className={`flex items-center gap-4 font-semibold transition-all duration-300 rounded-xl p-4 ${activeTab === item.key
                                    ? "text-blue-600 bg-white/40 backdrop-blur-sm border border-white/50 shadow-lg"
                                    : "hover:text-blue-600 hover:bg-white/20 backdrop-blur-sm"
                                }`}
                            onClick={() => onTabChange(item.key)}
                        >
                            <IconComponent className="text-2xl flex-shrink-0" />
                            <span
                                className="hidden group-hover:inline transition-all duration-300 whitespace-nowrap overflow-hidden"
                                style={{ maxWidth: "160px" }}
                            >
                                {item.label}
                            </span>
                        </button>
                    );
                })}
            </nav>
        </aside>
    );
}