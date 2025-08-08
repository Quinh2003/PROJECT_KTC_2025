import { MdInventory2, MdLocalShipping } from "react-icons/md";
import { FaUserCog } from "react-icons/fa";
import logo from "../assets/logo.png";

interface SidebarProps {
    activeTab: "orders" | "resources" | "assignment";
    onTabChange: (tab: "orders" | "resources" | "assignment") => void;
}

export default function Sidebar({ activeTab, onTabChange }: SidebarProps) {
    const menuItems = [
        {
            key: "orders" as const,
            icon: MdInventory2,
            label: "Order Management"
        },
        {
            key: "assignment" as const,
            icon: FaUserCog,
            label: "Driver Assignment"
        },
        {
            key: "resources" as const,
            icon: MdLocalShipping,
            label: "Resources"
        }
    ];

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
                            className={`flex items-center gap-3 font-semibold transition-all duration-300 rounded-xl p-4 ${activeTab === item.key
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