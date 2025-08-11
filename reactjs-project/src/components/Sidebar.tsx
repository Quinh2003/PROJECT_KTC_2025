import { MdInventory2, MdLocalShipping, MdDashboard, MdBarChart, MdPeople, MdSettings } from "react-icons/md";
import { FaUserCog } from "react-icons/fa";
import logo from "../assets/logo.png";

// Define tab types for different dashboard roles
export type DispatcherTab = "orders" | "resources" | "assignment";
export type OperationsTab = "overview" | "performance" | "monitoring" | "staff";

export type TabType = DispatcherTab | OperationsTab;

interface MenuItem<T extends TabType> {
    key: T;
    icon: React.ComponentType<{className?: string}>;
    label: string;
}

interface SidebarProps<T extends TabType> {
    activeTab: T;
    onTabChange: (tab: T) => void;
    dashboardType: "dispatcher" | "operations";
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
        } else {
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
        <aside className="group flex-shrink-0 w-20 hover:w-64 transition-all duration-300 bg-white/30 backdrop-blur-xl border-r border-white/40 text-gray-800 flex flex-col py-8 px-5 overflow-hidden h-screen sticky top-0 shadow-xl rounded-r-3xl">
            <div className="mb-5 flex items-center -mt-3 -ml-3 gap-1">
                <div className="w-16 h-16 rounded-full flex items-center justify-center flex-shrink-0 overflow-hidden bg-white/40 backdrop-blur-md border border-white/60 shadow-lg">
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
  className={`flex items-center gap-3 font-semibold transition-all duration-300 rounded-2xl p-4 ${
    activeTab === item.key
      ? "text-blue-700 bg-white/60 backdrop-blur-lg border border-white/60 shadow-xl"
      : "hover:text-blue-700 hover:bg-white/30 backdrop-blur-md"
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