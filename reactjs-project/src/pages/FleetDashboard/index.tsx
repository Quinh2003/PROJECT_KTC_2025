// index.tsx

import React, { useState } from 'react';
import VehicleTable from './VehicleTable';
import MaintenanceForm from './Maintenance_Form';

interface Vehicle {
  id: number;
  type: string;
  licensePlate: string;
  status: string;
}

interface Maintenance {
  id: number;
  vehicleId: number;
  date: string;
  description: string;
}

interface User {
  name: string;
  email: string;
}

interface FleetDashboardProps {
  user: User;
  onLogout: () => void;
}

const FleetDashboard: React.FC<FleetDashboardProps> = ({ user, onLogout }) => {
  const [vehicles] = useState<Vehicle[]>([
    { id: 1, type: 'Xe tải', licensePlate: '51H-12345', status: 'Hoạt động' },
    { id: 2, type: 'Xe container', licensePlate: '51H-67890', status: 'Bảo trì' },
  ]);
  const [maintenanceRecords, setMaintenanceRecords] = useState<Maintenance[]>([]);

  const handleAddMaintenance = (maintenance: Maintenance) => {
    setMaintenanceRecords([...maintenanceRecords, maintenance]);
  };

  return (
    <div className="p-6 bg-gray-100 min-h-screen">
      <div className="flex justify-between items-center mb-6">
        <div>
          <h1 className="text-2xl font-bold">Fleet Manager Dashboard</h1>
          <p className="text-gray-600">Xin chào {user.name} ({user.email})</p>
        </div>
        <button
          onClick={onLogout}
          className="px-4 py-2 bg-red-500 text-white rounded hover:bg-red-600"
        >
          Đăng xuất
        </button>
      </div>
      
      {/* Bảng danh sách xe */}
      <div className="mb-6">
        <h2 className="text-xl font-semibold mb-2">Danh sách xe</h2>
        <VehicleTable vehicles={vehicles} />
      </div>

      {/* Form quản lý bảo trì */}
      <div>
        <h2 className="text-xl font-semibold mb-2">Quản lý bảo trì</h2>
        <MaintenanceForm vehicles={vehicles} onAddMaintenance={handleAddMaintenance} />
      </div>
    </div>
  );
};

export default FleetDashboard;