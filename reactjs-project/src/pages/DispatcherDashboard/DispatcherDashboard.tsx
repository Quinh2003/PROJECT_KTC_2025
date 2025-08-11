import { useState } from "react";
import OrderOverview from "./OrderOverview";
import OrderList from "./OrderList";
import ResourceOverview from "./ResourceOverview";
import RouteMap from "./RouteMap";
import OrderAssignment from "./OrderAssignment";
import Sidebar, { type DispatcherTab } from "../../components/Sidebar";
import Navbar from "../../components/Navbar";
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
      <Sidebar
        activeTab={tab}
        onTabChange={tab => setTab(tab as DispatcherTab)}
        role="dispatcher"
      />

      {/* Main content */}
      <div className=" flex-1 flex flex-col">
        <Navbar
          user={user}
          onLogout={onLogout}
          title="Dispatcher Dashboard"
          subtitle="Track and assign orders to drivers and vehicles"
        />
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
