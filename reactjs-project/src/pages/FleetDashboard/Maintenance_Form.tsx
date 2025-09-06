import React, { useState } from 'react';
import type { FleetVehicle, Maintenance } from '../../types/dashboard';


interface MaintenanceFormProps {
  vehicles: FleetVehicle[];
  onAddMaintenance: (maintenance: Maintenance) => void;
}

const MaintenanceForm: React.FC<MaintenanceFormProps> = ({ vehicles, onAddMaintenance }) => {
  const [vehicleId, setVehicleId] = useState<number>(0);
  const [date, setDate] = useState<string>('');
  const [description, setDescription] = useState<string>('');

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    if (vehicleId && date && description) {
      onAddMaintenance({
        id: Math.random(),
        vehicleId,
        date,
        description,
      });
      setVehicleId(0);
      setDate('');
      setDescription('');
    }
  };

  return (
    <form onSubmit={handleSubmit} className="space-y-4">
      <div>
        <label className="block text-sm font-medium text-gray-700 mb-1">Chọn xe</label>
        <select
          value={vehicleId}
          onChange={(e) => setVehicleId(Number(e.target.value))}
          className="w-full px-4 py-2 rounded-lg bg-white/50 border border-gray-200/50 focus:border-blue-500/50 focus:ring focus:ring-blue-200/50 transition-all duration-300"
          required
        >
          <option value={0}>Chọn xe cần bảo trì</option>
          {vehicles.map((vehicle) => (
            <option key={vehicle.id} value={vehicle.id}>
              {vehicle.licensePlate} - {vehicle.type}
            </option>
          ))}
        </select>
      </div>

      <div>
        <label className="block text-sm font-medium text-gray-700 mb-1">Ngày bảo trì</label>
        <input
          type="date"
          value={date}
          onChange={(e) => setDate(e.target.value)}
          className="w-full px-4 py-2 rounded-lg bg-white/50 border border-gray-200/50 focus:border-blue-500/50 focus:ring focus:ring-blue-200/50 transition-all duration-300"
          required
        />
      </div>

      <div>
        <label className="block text-sm font-medium text-gray-700 mb-1">Mô tả công việc</label>
        <textarea
          value={description}
          onChange={(e) => setDescription(e.target.value)}
          className="w-full px-4 py-2 rounded-lg bg-white/50 border border-gray-200/50 focus:border-blue-500/50 focus:ring focus:ring-blue-200/50 transition-all duration-300"
          rows={4}
          required
        />
      </div>

      <button
        type="submit"
        className="w-full px-6 py-3 bg-blue-500/80 backdrop-blur-sm text-white rounded-lg hover:bg-blue-600/90 transition-all duration-300 hover:shadow-lg"
      >
        Thêm lịch bảo trì
      </button>
    </form>
  );
};

export default MaintenanceForm;