import { useState } from "react";
import OrderOverview from "./OrderOverview";
import OrderList from "./OrderList";
import ResourceOverview from "./ResourceOverview";
import OrderAssignment from "./OrderAssignment";
import Sidebar, { type DispatcherTab } from "../../components/Sidebar";
import Navbar from "../../components/Navbar";
import MapboxTrackingMap from "./MapboxTrackingMap";
import MapErrorBoundary from '../../components/MapErrorBoundary';
import type { User } from "../../types/User";
import VehicleList from "./VehicleList";
import DriverList from "./DriverList";
import { DispatcherProvider } from "../../contexts/DispatcherContext";

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
    <DispatcherProvider>
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
            subtitle=""
          />
          <main className="flex-1 p-6">
            {/* Orders Tab */}
            <div className={tab === "orders" ? "block" : "hidden"}>
              <OrderOverview />
              <div className="mt-6 grid grid-cols-1 xl:grid-cols-2 gap-6">
                <OrderList />
                <MapErrorBoundary>
                  <MapboxTrackingMap />
                </MapErrorBoundary>
              </div>
            </div>

            {/* Resources Tab */}
            <div className={tab === "resources" ? "block" : "hidden"}>
              <div className="grid grid-cols-1 xl:grid-cols-2 gap-6">
                <VehicleList />
                <DriverList />
              </div>
            </div>

            {/* Assignment Tab */}
            <div className={tab === "assignment" ? "block" : "hidden"}>
              <OrderAssignment />
            </div>
          </main>
        </div>
      </div>
    </DispatcherProvider>
  );
}