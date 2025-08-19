

import { useEffect, useState } from "react";
import { fetchOrdersRaw } from "../../services/OrderAPI";
import { fetchVehicles, type Vehicle } from "../../services/VehicleListAPI";
import { PackageOpen, Truck, Hourglass, CheckCircle } from "lucide-react";

// Khai báo lại type cho Order để tránh lỗi never
type OrderStatus = string | { name: string };
type Order = {
  status: OrderStatus;
  // ...các trường khác nếu cần
};


export default function StatsCards() {
  const [orders, setOrders] = useState<Order[]>([]);
  const [vehicles, setVehicles] = useState<Vehicle[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  useEffect(() => {
    const getData = async () => {
      try {
        setLoading(true);
        setError("");
        const [ordersData, vehiclesData] = await Promise.all([
          fetchOrdersRaw(),
          fetchVehicles(),
        ]);
        setOrders(ordersData);
        setVehicles(vehiclesData);
      } catch (err: any) {
        setError(err.message || "Đã xảy ra lỗi");
      } finally {
        setLoading(false);
      }
    };
    getData();
  }, []);

  // Tính toán số lượng theo trạng thái
  const totalShipments = orders.length;
  const totalVehicles = vehicles.length;
  const pendingPackages = orders.filter(
    o => (
      typeof o.status === "object" && o.status && typeof (o.status as any).name === "string"
        ? ((o.status as { name: string }).name.toLowerCase() === "pending")
        : (typeof o.status === "string" && o.status.toLowerCase() === "pending")
    )
  ).length;

  const deliveredPackages = orders.filter(
    o => (
      typeof o.status === "object" && o.status && typeof (o.status as any).name === "string"
        ? ((o.status as { name: string }).name.toLowerCase() === "completed")
        : (typeof o.status === "string" && o.status.toLowerCase() === "completed")
    )
  ).length;
  const stats = [
    { label: "Total shipments", value: totalShipments, icon: <PackageOpen size={28} color="#6366f1" /> }, // Indigo
    { label: "Total vehicles", value: totalVehicles, icon: <Truck size={28} color="#10b981" /> }, // Green
    { label: "Pending packages", value: pendingPackages, icon: <Hourglass size={28} color="#f59e42" /> }, // Orange
    { label: "Packages delivered", value: deliveredPackages, icon: <CheckCircle size={28} color="#22c55e" /> }, // Emerald
  ];

  if (loading) {
    return <div className="mb-6">Đang tải dữ liệu...</div>;
  }
  if (error) {
    return <div className="mb-6 text-red-500">{error}</div>;
  }
  return (
    <div className="grid grid-cols-2 md:grid-cols-4 gap-6 mb-6">
      {stats.map((s, i) => (
        <div key={i} className="bg-white/30 backdrop-blur-lg rounded-2xl p-6 flex items-center gap-4 border border-white/30 shadow-lg hover:shadow-xl transition-all duration-300 hover:bg-white/40">
          <span className="text-3xl filter drop-shadow-sm">{s.icon}</span>
          <div>
            <div className="text-2xl font-bold text-gray-800">{s.value}</div>
            <div className="text-gray-600 font-medium">{s.label}</div>
          </div>
        </div>
      ))}
    </div>
  );
}