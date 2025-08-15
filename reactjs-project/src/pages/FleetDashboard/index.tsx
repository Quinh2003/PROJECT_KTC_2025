import { useState } from 'react';
import VehicleTable from './VehicleTable';
import MaintenanceForm from './Maintenance_Form';
import type { User } from '../../types/User';
import type { FleetVehicle, Maintenance } from '../../types/dashboard';



interface FleetDashboardProps {
  user: User;
  onLogout: () => void;
}

export default function FleetDashboard({ user, onLogout }: FleetDashboardProps) {
  const [activeTab, setActiveTab] = useState<"vehicles" | "maintenance">("vehicles");
  const [vehicles] = useState<FleetVehicle[]>([
    {
      id: 1,
      licensePlate: '51H-12345',
      type: 'Xe tải',
      brand: '',
      model: '',
      year: 2020,
      status: 'Hoạt động',
      lastMaintenance: '',
      nextMaintenance: '',
      driver: '',
      mileage: 0,
    },
    {
      id: 2,
      licensePlate: '51H-67890',
      type: 'Xe container',
      brand: '',
      model: '',
      year: 2021,
      status: 'Bảo trì',
      lastMaintenance: '',
      nextMaintenance: '',
      driver: '',
      mileage: 0,
    },
  ]);
  const [maintenanceRecords, setMaintenanceRecords] = useState<Maintenance[]>([]);

  const handleAddMaintenance = (maintenance: Maintenance) => {
    setMaintenanceRecords([...maintenanceRecords, maintenance]);
  };

  return (
    <div className="min-h-screen flex bg-gradient-to-br from-blue-100 via-white to-blue-200">
      {/* Sidebar */}
      <aside className="group flex-shrink-0 w-20 hover:w-64 transition-all duration-300 backdrop-blur-lg bg-white/20 border-r border-white/40 text-gray-800 flex flex-col py-6 px-4 overflow-hidden h-screen sticky top-0">
        <nav className="flex-1 flex flex-col gap-7">
          <button
            onClick={() => setActiveTab("vehicles")}
            className={`flex items-center gap-3 font-semibold transition-all duration-300 ${activeTab === "vehicles" ? "text-blue-600 scale-105" : "hover:text-blue-500 hover:scale-105"}`}
          >
            <span className="text-2xl ml-2 flex-shrink-0">🚚</span>
            <span className="hidden group-hover:inline transition-all duration-200 whitespace-nowrap overflow-hidden" style={{ maxWidth: "160px" }}>
              Danh sách xe
            </span>
          </button>
          <button
            onClick={() => setActiveTab("maintenance")}
            className={`flex items-center gap-3 font-semibold transition-all duration-300 ${activeTab === "maintenance" ? "text-blue-600 scale-105" : "hover:text-blue-500 hover:scale-105"}`}
          >
            <span className="text-2xl ml-2 flex-shrink-0">🛠️</span>
            <span className="hidden group-hover:inline transition-all duration-200 whitespace-nowrap overflow-hidden" style={{ maxWidth: "160px" }}>
              Quản lý bảo trì
            </span>
          </button>
        </nav>
      </aside>

      {/* Main content */}
      <div className="flex-1 flex flex-col">
        <header className="backdrop-blur-md bg-white/30 border-b border-white/40 px-6 py-4 flex items-center justify-between transition-all duration-300">
          <div>
            <h1 className="text-3xl font-bold text-gray-800 mb-1 drop-shadow-sm">
              Fleet Manager Dashboard
            </h1>
            <div className="text-gray-600 text-base font-medium">
              Xin chào <span className="text-blue-600 font-semibold">{user.name}</span> ({user.email})
            </div>
          </div>
          <button
            onClick={onLogout}
            className="px-4 py-2 bg-red-500/80 backdrop-blur-sm text-white rounded-lg hover:bg-red-600/90 transition-all duration-300 hover:shadow-lg"
          >
            Đăng xuất
          </button>
        </header>

        <main className="flex-1 p-6 space-y-6">
          {activeTab === "vehicles" && (
            <div className="transition-all duration-300 hover:scale-[1.01]">
              <div className="rounded-xl bg-white/30 backdrop-blur-md border border-white/40 shadow-lg p-6">
                <h2 className="text-xl font-semibold mb-4 text-gray-800">Danh sách xe</h2>
                <VehicleTable vehicles={vehicles} />
              </div>
            </div>
          )}
          {activeTab === "maintenance" && (
            <div className="transition-all duration-300 hover:scale-[1.01]">
              <div className="rounded-xl bg-white/30 backdrop-blur-md border border-white/40 shadow-lg p-6">
                <h2 className="text-xl font-semibold mb-4 text-gray-800">Quản lý bảo trì</h2>
                <MaintenanceForm vehicles={vehicles} onAddMaintenance={handleAddMaintenance} />
              </div>
            </div>
          )}
        </main>
      </div>
    </div>
  );
}