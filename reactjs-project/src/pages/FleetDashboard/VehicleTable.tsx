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
    <div className="overflow-x-auto">
      <table className="min-w-full bg-white border border-gray-200">
        <thead>
          <tr className="bg-gray-100">
            <th className="py-2 px-4 border-b text-left">Loại xe</th>
            <th className="py-2 px-4 border-b text-left">Biển số</th>
            <th className="py-2 px-4 border-b text-left">Tình trạng</th>
          </tr>
        </thead>
        <tbody>
          {vehicles.map((vehicle) => (
            <tr key={vehicle.id} className="hover:bg-gray-50">
              <td className="py-2 px-4 border-b">{vehicle.type}</td>
              <td className="py-2 px-4 border-b">{vehicle.licensePlate}</td>
              <td className="py-2 px-4 border-b">
                <span
                  className={`px-2 py-1 rounded ${
                    vehicle.status === 'Hoạt động'
                      ? 'bg-green-100 text-green-800'
                      : 'bg-red-100 text-red-800'
                  }`}
                >
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