import { useState } from "react";
import OrderOverview from "./OrderOverview";
import OrderList from "./OrderList";
import ResourceOverview from "./ResourceOverview";
import RouteMap from "./RouteMap";
import OrderAssignment from "./OrderAssignment";
import { MdInventory2, MdLocalShipping } from "react-icons/md";
import { FaUserCog } from "react-icons/fa";
import type { User } from "../../types/User";
import logo from "../../assets/logo.png";

interface DispatcherDashboardProps {
  user: User;
  onLogout: () => void;
}

export default function DispatcherDashboard({
  user,
  onLogout,
}: DispatcherDashboardProps) {
  // Thêm "assignment" vào state tab
  const [tab, setTab] = useState<"orders" | "resources" | "assignment">("orders");

  return (
    <div className="min-h-screen flex bg-[#f6f8fa]">
      {/* Sidebar */}
      <aside className="group flex-shrink-0 w-20 hover:w-64 transition-all duration-200 bg-[#23272f] text-white flex flex-col py-6 px-4 overflow-hidden h-screen sticky top-0">
        <div className="mb-5 flex items-center -mt-3 -ml-3 gap-1">
          <div className="w-16 h-16 rounded-full flex items-center justify-center flex-shrink-0 overflow-hidden">
            <img
              src={logo}
              alt="Logo"
              className="w-16 h-16 rounded-full object-cover"
            />
          </div>
          <span
            className="hidden group-hover:inline-block font-bold text-lg tracking-wide transition-all duration-200 whitespace-nowrap overflow-hidden"
            style={{ maxWidth: "200px" }}
          >
            Fast Route
          </span>
        </div>
        <nav className="flex-1 flex flex-col gap-7">
  <button
    className={`flex items-center gap-3 font-semibold ${
      tab === "orders" ? "text-[#58ADCC]" : "hover:text-[#58ADCC]"
    }`}
    onClick={() => setTab("orders")}
  >
    <MdInventory2 className="text-2xl ml-2 flex-shrink-0" />
    <span
      className="hidden group-hover:inline transition-all duration-200 whitespace-nowrap overflow-hidden"
      style={{ maxWidth: "160px" }}
    >
      Order Management
    </span>
  </button>
  {/* Đưa tab phân công tài xế lên trên resources */}
  <button
    className={`flex items-center gap-3 font-semibold ${
      tab === "assignment" ? "text-[#58ADCC]" : "hover:text-[#58ADCC]"
    }`}
    onClick={() => setTab("assignment")}
  >
    <FaUserCog className="text-2xl ml-2 flex-shrink-0" />
    <span
      className="hidden group-hover:inline transition-all duration-200 whitespace-nowrap overflow-hidden"
      style={{ maxWidth: "160px" }}
    >
      Driver Assignment
    </span>
  </button>
  <button
    className={`flex items-center gap-3 font-semibold ${
      tab === "resources" ? "text-[#58ADCC]" : "hover:text-[#58ADCC]"
    }`}
    onClick={() => setTab("resources")}
  >
    <MdLocalShipping className="text-2xl ml-2 flex-shrink-0" />
    <span
      className="hidden group-hover:inline transition-all duration-200 whitespace-nowrap overflow-hidden"
      style={{ maxWidth: "160px" }}
    >
      Resources
    </span>
  </button>
</nav>
      </aside>

      {/* Main content */}
      <div className="flex-1 flex flex-col">
        <header className="bg-white shadow px-6 py-4 flex items-center justify-between">
          <div>
            <h1 className="text-3xl font-bold text-gray-800 mb-1">
              Dispatcher Dashboard
            </h1>
            <div className="text-gray-500 text-base">
              Track and assign orders to drivers and vehicles
            </div>
          </div>
          <div className="flex items-center gap-4">
            <span className="text-gray-600">Hello, {user.name}</span>
            <button
              className="px-4 py-2 bg-blue-100 text-blue-800 rounded hover:bg-blue-200 transition"
              onClick={onLogout}
            >
              Logout
            </button>
          </div>
        </header>
        <main className="flex-1 p-6">
  {tab === "orders" && (
    <>
      <OrderOverview />
      <div className="mt-6 grid grid-cols-1 xl:grid-cols-2 gap-6">
        <OrderList />
        <RouteMap />
      </div>
    </>
  )}
  {tab === "resources" && <ResourceOverview />}
  {tab === "assignment" && <OrderAssignment />}
</main>
      </div>
    </div>
  );
}