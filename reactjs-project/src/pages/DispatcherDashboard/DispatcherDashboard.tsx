import { useState } from "react";
import OrderOverview from "./OrderOverview";
import OrderList from "./OrderList";
import ResourceOverview from "./ResourceOverview";
import RouteMap from "./RouteMap";
import OrderAssignment from "./OrderAssignment";
import Sidebar, { type DispatcherTab } from "../../components/Sidebar";
import type { User } from "../../types/User";


interface DispatcherDashboardProps {
  user: User;
  onLogout: () => void;
}

export default function DispatcherDashboard({
  user,
  onLogout,
}: DispatcherDashboardProps) {
  // Thêm "assignment" vào state tab
  const [tab, setTab] = useState<DispatcherTab>("orders");

  return (
    <div className="min-h-screen flex bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
      <Sidebar activeTab={tab} onTabChange={setTab} dashboardType="dispatcher" />

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