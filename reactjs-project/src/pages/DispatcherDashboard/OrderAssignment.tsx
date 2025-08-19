import { useState } from "react";
import { FaUserCog, FaCheck, FaTimes } from "react-icons/fa";

interface OrdersAssignmentProps {
  orders?: any[];
}

// Demo drivers and vehicles
const drivers = [
  {
    id: 1,
    name: "Demo Driver",
    vehicle: "51A-12345",
    phone: "0901234567",
  },
  {
    id: 2,
    name: "Driver B",
    vehicle: "51C-67890",
    phone: "0902345678",
  },
];

const demoOrders = [
  {
    id: 101,
    code: "FR005",
    customer: "Demo Customer",
    address: "111",
    note: "",
    date: "8/7/2025",
    from: "111",
    to: "111",
    description: "111",
    status: "Pending",
    priority: "Medium",
  },
  {
    id: 102,
    code: "FR006",
    customer: "Another Customer",
    address: "222",
    note: "",
    date: "9/7/2025",
    from: "222",
    to: "222",
    description: "222",
    status: "Pending",
    priority: "High",
  },
];

export default function OrdersAssignment({ orders }: OrdersAssignmentProps) {
  const data = orders && orders.length > 0 ? orders : demoOrders;
  // Save selected driver for each order
  const [selectedDrivers, setSelectedDrivers] = useState<Record<number, number | null>>({});

  const handleSelectDriver = (orderId: number, driverId: number | null) => {
    setSelectedDrivers((prev) => ({ ...prev, [orderId]: driverId }));
  };

  const handleAssign = (orderId: number) => {
    const driver = drivers.find((d) => d.id === selectedDrivers[orderId]);
    alert(
      driver
        ? `Assigned ${driver.name} (${driver.vehicle}) to order ${orderId}!`
        : "Please select a driver."
    );
    setSelectedDrivers((prev) => ({ ...prev, [orderId]: null }));
  };

  return (
    <div className="bg-black/30 backdrop-blur-xl rounded-2xl p-6 border border-white/10 shadow-xl hover:bg-black/40 transition-all duration-300">
      <div className="flex items-center gap-3 mb-6">
        <FaUserCog className="text-2xl text-blue-400" />
        <div>
          <h2 className="text-2xl font-bold text-white">Orders Pending Assignment</h2>
          <p className="text-gray-300">Review and assign drivers to new orders</p>
        </div>
      </div>

      <div className="bg-black/20 backdrop-blur-sm rounded-xl border border-white/10 overflow-hidden shadow-lg">
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead>
              <tr className="bg-black/30 backdrop-blur-sm border-b border-white/10">
                <th className="text-left p-4 font-semibold text-gray-200">Order Code</th>
                <th className="text-left p-4 font-semibold text-gray-200">Customer</th>
                <th className="text-left p-4 font-semibold text-gray-200">Route</th>
                <th className="text-left p-4 font-semibold text-gray-200">Priority</th>
                <th className="text-left p-4 font-semibold text-gray-200">Date</th>
                <th className="text-left p-4 font-semibold text-gray-200">Driver Assignment</th>
                <th className="text-left p-4 font-semibold text-gray-200">Actions</th>
              </tr>
            </thead>
            <tbody>
              {data.map((order, index) => {
                const selectedDriverId = selectedDrivers[order.id] ?? null;
                const selectedDriverObj = drivers.find((d) => d.id === selectedDriverId);
                
                return (
                  <tr 
                    key={order.id} 
                    className={`border-b border-white/10 hover:bg-black/20 transition-all duration-300 ${
                      index % 2 === 0 ? 'bg-black/10' : 'bg-transparent'
                    }`}
                  >
                    <td className="p-4">
                      <div className="flex items-center gap-2">
                        <span className="font-bold text-white">{order.code}</span>
                        <span className="bg-yellow-500/30 backdrop-blur-sm text-yellow-300 px-2 py-1 rounded-full text-xs font-semibold border border-yellow-400/50">
                          {order.status}
                        </span>
                      </div>
                    </td>
                    <td className="p-4">
                      <div className="font-medium text-white">{order.customer}</div>
                      <div className="text-sm text-gray-300">{order.description}</div>
                    </td>
                    <td className="p-4">
                      <div className="text-sm text-gray-300">
                        <div><span className="font-medium">From:</span> {order.from}</div>
                        <div><span className="font-medium">To:</span> {order.to}</div>
                      </div>
                    </td>
                    <td className="p-4">
                      <span className={`px-2 py-1 rounded-full text-xs font-semibold backdrop-blur-sm border ${
                        order.priority === 'High' 
                          ? 'bg-red-500/30 text-red-300 border-red-400/50'
                          : order.priority === 'Medium'
                          ? 'bg-orange-500/30 text-orange-300 border-orange-400/50'
                          : 'bg-green-500/30 text-green-300 border-green-400/50'
                      }`}>
                        {order.priority}
                      </span>
                    </td>
                    <td className="p-4">
                      <div className="text-gray-200 font-medium">{order.date}</div>
                    </td>
                    <td className="p-4">
                      <select
                        className="w-48 bg-black/20 backdrop-blur-sm border border-white/10 rounded-lg px-3 py-2 text-sm text-gray-200 focus:outline-none focus:ring-2 focus:ring-blue-400/50 transition-all duration-300"
                        value={selectedDriverId ?? ""}
                        onChange={(e) =>
                          handleSelectDriver(order.id, e.target.value ? Number(e.target.value) : null)
                        }
                      >
                        <option value="">Select driver...</option>
                        {drivers.map((driver) => (
                          <option key={driver.id} value={driver.id}>
                            {driver.name} - {driver.vehicle}
                          </option>
                        ))}
                      </select>
                      {selectedDriverObj && (
                        <div className="mt-2 text-xs text-blue-300 bg-blue-500/20 backdrop-blur-sm rounded p-2 border border-blue-400/30">
                          <div className="font-medium">{selectedDriverObj.name}</div>
                          <div>{selectedDriverObj.phone}</div>
                        </div>
                      )}
                    </td>
                    <td className="p-4">
                      <div className="flex gap-2">
                        {selectedDriverObj && (
                          <>
                            <button
                              className="bg-green-500/60 backdrop-blur-sm hover:bg-green-500/80 text-white p-2 rounded-lg font-semibold transition-all duration-300 shadow-md hover:shadow-lg border border-green-400/50"
                              onClick={() => handleAssign(order.id)}
                              title="Assign Driver"
                            >
                              <FaCheck size={14} />
                            </button>
                            <button
                              className="bg-black/20 backdrop-blur-sm border border-red-400/50 text-red-300 p-2 rounded-lg hover:bg-red-500/30 transition-all duration-300 shadow-md hover:shadow-lg"
                              onClick={() => handleSelectDriver(order.id, null)}
                              title="Cancel Selection"
                            >
                              <FaTimes size={14} />
                            </button>
                          </>
                        )}
                      </div>
                    </td>
                  </tr>
                );
              })}
            </tbody>
          </table>
        </div>
      </div>

      {data.length === 0 && (
        <div className="text-center py-8 text-gray-400">
          <FaUserCog className="text-4xl mx-auto mb-2 opacity-50" />
          <p className="text-lg">No pending orders for assignment</p>
        </div>
      )}
    </div>
  );
}