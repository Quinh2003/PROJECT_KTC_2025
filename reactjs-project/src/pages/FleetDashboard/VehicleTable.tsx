import React from 'react';

interface Vehicle {
  id: number;
  type: string;
  licensePlate: string;
  status: string;
}

interface VehicleTableProps {
  vehicles: Vehicle[];
}

const VehicleTable: React.FC<VehicleTableProps> = ({ vehicles }) => {
  return (
    <div className="overflow-hidden rounded-lg">
      <table className="min-w-full divide-y divide-gray-200/50">
        <thead className="backdrop-blur-sm bg-white/40">
          <tr>
            <th className="px-6 py-3 text-left text-sm font-semibold text-gray-700">ID</th>
            <th className="px-6 py-3 text-left text-sm font-semibold text-gray-700">Loại xe</th>
            <th className="px-6 py-3 text-left text-sm font-semibold text-gray-700">Biển số</th>
            <th className="px-6 py-3 text-left text-sm font-semibold text-gray-700">Trạng thái</th>
          </tr>
        </thead>
        <tbody className="divide-y divide-gray-200/50 bg-white/20">
          {vehicles.map((vehicle) => (
            <tr 
              key={vehicle.id}
              className="transition-all duration-300 hover:bg-white/40"
            >
              <td className="px-6 py-4 text-sm text-gray-700">{vehicle.id}</td>
              <td className="px-6 py-4 text-sm text-gray-700">{vehicle.type}</td>
              <td className="px-6 py-4 text-sm font-medium text-blue-600">{vehicle.licensePlate}</td>
              <td className="px-6 py-4">
                <span className={`inline-flex items-center px-3 py-1 rounded-full text-sm font-medium
                  ${vehicle.status === 'Hoạt động' 
                    ? 'bg-green-100/70 text-green-800' 
                    : 'bg-yellow-100/70 text-yellow-800'
                  }`}>
                  {vehicle.status}
                </span>
              </td>
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
};

export default VehicleTable;