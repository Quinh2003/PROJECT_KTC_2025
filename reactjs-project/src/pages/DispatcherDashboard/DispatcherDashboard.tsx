import { useState } from "react";
import StatsCards from "./StatsCards";
import OngoingDelivery from "./OngoingDelivery";
import ShipmentMapDetail from "./ShipmentMapDetail";
import TrackOrderTable from "./TrackOrderTable";

const shipments = [
  {
    id: 1,
    code: "#454EH1274",
    category: "Books",
    courier: "DHL Express",
    from: "123 Oak Street, Springfield, IL",
    to: "789 Maple Avenue, San Francisco, CA",
    distance: "300 miles",
    driver: "Ben Millett",
    weight: 9.1,
    payment: 169,
    status: "Shipping",
    date: "Oct. 2, 2024",
    truckImg: "https://img.icons8.com/ios-filled/100/000000/truck.png",
  },
  // ... các shipment khác ...
];

export default function DispatcherDashboard() {
  const [selected, setSelected] = useState(shipments[0]);

  return (
    <div className="min-h-screen bg-gradient-to-br from-gray-100 to-gray-200 p-4">
      {/* Header */}
      <div className="flex items-center justify-between mb-6">
        <div className="flex items-center gap-3">
          <img src="https://randomuser.me/api/portraits/women/44.jpg" alt="avatar" className="w-12 h-12 rounded-full" />
          <span className="font-bold text-xl">Dispatcher Dashboard</span>
        </div>
        <div className="flex items-center gap-4">
          {/* ...icon buttons... */}
        </div>
      </div>

      <StatsCards />

      <div className="flex flex-col xl:flex-row gap-6 mb-6">
        <div className="flex-1">
          <OngoingDelivery shipments={shipments} selected={selected} onSelect={setSelected} />
        </div>
        <div className="flex-1">
          <ShipmentMapDetail shipment={selected} />
        </div>
      </div>

      <TrackOrderTable shipments={shipments} />
    </div>
  );
}