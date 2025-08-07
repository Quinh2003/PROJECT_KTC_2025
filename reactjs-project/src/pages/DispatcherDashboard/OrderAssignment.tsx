import { useState } from "react";
import { FaUserCog } from "react-icons/fa";

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
    <div className="mt-8 bg-white rounded-2xl p-8 shadow">
      <div className="text-2xl font-bold mb-1">Orders Pending Assignment</div>
      <div className="text-gray-500 mb-4 text-lg">
        Review and assign drivers to new orders
      </div>
      {data.map((order) => {
        const selectedDriverId = selectedDrivers[order.id] ?? null;
        const selectedDriverObj = drivers.find((d) => d.id === selectedDriverId);

        return (
          <div
            key={order.id}
            className="bg-white rounded-2xl border p-6 mb-6 shadow flex flex-col gap-4"
          >
            <div className="flex flex-wrap items-center justify-between gap-4">
              <div className="flex items-center gap-4">
                <span className="font-bold text-lg text-black">{order.code}</span>
                <span className="bg-yellow-100 text-yellow-800 px-3 py-1 rounded-full text-sm font-semibold">
                  {order.status}
                </span>
                <span className="bg-orange-100 text-orange-800 px-3 py-1 rounded-full text-sm font-semibold">
                  {order.priority}
                </span>
              </div>
              <div className="text-gray-400 text-lg">{order.date}</div>
            </div>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <div>
                <div>
                  Customer:{" "}
                  <span className="font-bold">{order.customer}</span>
                </div>
                <div>
                  From: <span className="font-bold">{order.from}</span>
                </div>
                <div>
                  Description: <span className="font-bold">{order.description}</span>
                </div>
              </div>
              <div>
                <div>
                  To: <span className="font-bold">{order.to}</span>
                </div>
              </div>
            </div>
            {/* Driver Assignment */}
            <div className="bg-blue-50 rounded-2xl p-6 mt-4">
              <div className="flex items-center gap-2 mb-4 text-blue-700 font-semibold text-lg">
                <FaUserCog />
                Driver Assignment
              </div>
              <div className="flex flex-row items-center gap-4">
                <select
                  className="w-full md:w-80 border rounded px-4 py-2 text-lg"
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
                  <>
                    <button
                      className="bg-[#16A34A] hover:bg-green-800 text-white px-6 py-2 rounded-lg font-semibold text-lg transition"
                      onClick={() => handleAssign(order.id)}
                    >
                      Assign
                    </button>
                    <button
                      className="bg-white border border-red-300 text-red-500 px-6 py-2 rounded-lg font-semibold text-lg hover:bg-red-50 transition"
                      onClick={() => handleSelectDriver(order.id, null)}
                    >
                      Cancel
                    </button>
                  </>
                )}
              </div>
              {selectedDriverObj && (
                <div className="mt-4 bg-white border rounded-xl p-4 text-blue-800">
                  <div>
                    <span className="font-semibold">Selected driver:</span> {selectedDriverObj.name}
                  </div>
                  <div>
                    <span className="font-semibold">Vehicle:</span> {selectedDriverObj.vehicle}
                  </div>
                  <div>
                    <span className="font-semibold">Phone:</span> {selectedDriverObj.phone}
                  </div>
                </div>
              )}
            </div>
          </div>
        );
      })}
    </div>
  );
}