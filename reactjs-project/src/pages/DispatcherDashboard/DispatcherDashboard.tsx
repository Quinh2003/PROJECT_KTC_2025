import { useState } from "react";
import { useJsApiLoader } from "@react-google-maps/api";
import OrderOverview from "./OrderOverview";
import OrderList from "./OrderList";
import ResourceOverview from "./ResourceOverview";
import RouteMap from "./RouteMap";
import OrderAssignment from "./OrderAssignment";
import RouteOptimizer from "./RouteOptimizer";
import Sidebar, { type DispatcherTab } from "../../components/Sidebar";
import Navbar from "../../components/Navbar";
import type { User } from "../../types/User";

interface DispatcherDashboardProps {
  user: User;
  onLogout: () => void;
}

export default function DispatcherDashboard({ user, onLogout }: DispatcherDashboardProps) {
  const [tab, setTab] = useState<DispatcherTab>("orders");
  const apiKey = import.meta.env.VITE_GOOGLE_MAPS_API_KEY || "";
  const { isLoaded, loadError } = useJsApiLoader({
    googleMapsApiKey: apiKey,
  });

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
                <RouteMap isLoaded={isLoaded} loadError={loadError} />
              </div>
            </>
          )}
          {tab === "resources" && <ResourceOverview />}
          {tab === "assignment" && (
            <>
              <OrderAssignment />
              <div className="mt-8">
                <RouteOptimizer isLoaded={isLoaded} loadError={loadError} />
              </div>
            </>
          )}
        </main>
      </div>
    </div>
  );
}
