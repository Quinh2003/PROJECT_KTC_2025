// MaintenanceForm.tsx
import React, { useState } from 'react';

interface Maintenance {
  id: number;
  vehicleId: number;
  date: string;
  description: string;
}

interface MaintenanceFormProps {
  vehicles: { id: number; licensePlate: string }[];
  onAddMaintenance: (maintenance: Maintenance) => void;
}

const MaintenanceForm: React.FC<MaintenanceFormProps> = ({ vehicles, onAddMaintenance }) => {
  const [selectedVehicleId, setSelectedVehicleId] = useState<number>(vehicles[0]?.id || 0);
  const [date, setDate] = useState<string>('');
  const [description, setDescription] = useState<string>('');
  const [maintenanceHistory, setMaintenanceHistory] = useState<Maintenance[]>([
    { id: 1, vehicleId: 1, date: '2025-08-01', description: 'Thay dầu máy' },
    { id: 2, vehicleId: 1, date: '2025-07-15', description: 'Kiểm tra phanh' },
  ]);

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    const newMaintenance = {
      id: Date.now(),
      vehicleId: selectedVehicleId,
      date,
      description,
    };
    onAddMaintenance(newMaintenance);
    setMaintenanceHistory([...maintenanceHistory, newMaintenance]);
    setDate('');
    setDescription('');
  };

  // Cảnh báo bảo trì sắp tới (giả định: trong 7 ngày tới)
  const upcomingMaintenance = maintenanceHistory.filter(
    (m) => new Date(m.date) <= new Date(Date.now() + 7 * 24 * 60 * 60 * 1000)
  );

  return (
    <div className="p-4 bg-white shadow-md rounded-lg">
      <h2 className="text-xl font-bold mb-4">Quản lý bảo trì</h2>
      
      {/* Form tạo lịch bảo trì */}
      <form onSubmit={handleSubmit} className="space-y-4">
        <div>
          <label className="block text-sm font-medium">Chọn xe</label>
          <select
            value={selectedVehicleId}
            onChange={(e) => setSelectedVehicleId(Number(e.target.value))}
            className="w-full p-2 border rounded"
          >
            {vehicles.map((vehicle) => (
              <option key={vehicle.id} value={vehicle.id}>
                {vehicle.licensePlate}
              </option>
            ))}
          </select>
        </div>
        <div>
          <label className="block text-sm font-medium">Ngày bảo trì</label>
          <input
            type="date"
            value={date}
            onChange={(e) => setDate(e.target.value)}
            className="w-full p-2 border rounded"
            required
          />
        </div>
        <div>
          <label className="block text-sm font-medium">Mô tả</label>
          <textarea
            value={description}
            onChange={(e) => setDescription(e.target.value)}
            className="w-full p-2 border rounded"
            required
          />
        </div>
        <button
          type="submit"
          className="px-4 py-2 bg-blue-500 text-white rounded hover:bg-blue-600"
        >
          Thêm lịch bảo trì
        </button>
      </form>

      {/* Cảnh báo bảo trì sắp tới */}
      {upcomingMaintenance.length > 0 && (
        <div className="mt-4 p-4 bg-yellow-100 border border-yellow-400 rounded">
          <h3 className="font-bold">Cảnh báo bảo trì sắp tới</h3>
          <ul className="list-disc pl-5">
            {upcomingMaintenance.map((m) => (
              <li key={m.id}>{`${vehicles.find((v) => v.id === m.vehicleId)?.licensePlate}: ${m.date} - ${m.description}`}</li>
            ))}
          </ul>
        </div>
      )}

      {/* Lịch sử bảo trì */}
      <div className="mt-4">
        <h3 className="font-bold">Lịch sử bảo trì</h3>
        <ul className="list-disc pl-5">
          {maintenanceHistory
            .filter((m) => m.vehicleId === selectedVehicleId)
            .map((m) => (
              <li key={m.id}>{`${m.date} - ${m.description}`}</li>
            ))}
        </ul>
      </div>
    </div>
  );
};

export default MaintenanceForm;