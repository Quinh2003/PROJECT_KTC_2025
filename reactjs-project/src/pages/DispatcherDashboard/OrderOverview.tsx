

import { useEffect, useState } from "react";
import { useTranslation } from 'react-i18next';
import { fetchOrderStats } from "../../services/orderAPI";
import { fetchVehicleStats } from "../../services/VehicleListAPI";
import type { Vehicle } from "../../types";
import type { Order } from "../../types/Order";
import { PackageOpen, Truck, Hourglass, CheckCircle } from "lucide-react";


interface StatsCardsProps {
  refreshTrigger?: number;
}


export default function StatsCards({ refreshTrigger }: StatsCardsProps) {
  const { t } = useTranslation();
  const [totalOrders, setTotalOrders] = useState(0);
  const [sampleOrders, setSampleOrders] = useState<Order[]>([]);
  const [vehicles, setVehicles] = useState<Vehicle[]>([]);
  const [totalVehicles, setTotalVehicles] = useState(0);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  // Key for localStorage
  const CACHE_KEY = "ktc_stats_cards_cache_v1";

  useEffect(() => {
    let didCancel = false;
    const getData = async () => {
      try {
        setLoading(true);
        setError("");
        // Nếu không có refreshTrigger, thử lấy cache
        if (!refreshTrigger) {
          const cache = localStorage.getItem(CACHE_KEY);
          if (cache) {
            const parsed = JSON.parse(cache);
            setTotalOrders(parsed.totalOrders || 0);
            setSampleOrders(parsed.sampleOrders || []);
            setVehicles(parsed.vehicles || []);
            setTotalVehicles(parsed.totalVehicles || 0);
            setLoading(false);
            // Vẫn gọi API ngầm để update cache nếu muốn, hoặc bỏ qua để tiết kiệm request
            return;
          }
        }
        // Nếu có refreshTrigger hoặc không có cache, gọi API
        const [orderStats, vehicleStats] = await Promise.all([
          fetchOrderStats(),
          fetchVehicleStats(),
        ]);
        if (didCancel) return;
        setTotalOrders(orderStats.totalRecords);
        setSampleOrders(orderStats.sampleOrders);
        setVehicles(vehicleStats.sampleVehicles);
        setTotalVehicles(vehicleStats.totalRecords || 0);
        // Lưu cache
        localStorage.setItem(CACHE_KEY, JSON.stringify({
          totalOrders: orderStats.totalRecords,
          sampleOrders: orderStats.sampleOrders,
          vehicles: vehicleStats.sampleVehicles,
          totalVehicles: vehicleStats.totalRecords || 0,
        }));
      } catch (err: any) {
        setError(err.message || t('common.error', 'An error occurred'));
      } finally {
        setLoading(false);
      }
    };
    getData();
    return () => { didCancel = true; };
  }, [refreshTrigger]);

  // Tính toán số lượng theo trạng thái từ sample (ước tính)
  const totalShipments = totalOrders; // Sử dụng tổng số thật
  // const totalVehicles = vehicles.length; // Đã lấy từ API
  
  // Tính tỷ lệ từ sample để ước tính
  const sampleSize = sampleOrders.length;

  const deliveredInSample = sampleOrders.filter(
    o => {
      if (!o.status) return false;
      if (typeof o.status === "object" && "name" in o.status) {
        return o.status.name.toLowerCase() === "completed";
      }
      return false;
    }
  ).length;
  
  // Ước tính từ sample
  const deliveredPackages = sampleSize > 0 ? Math.round((deliveredInSample / sampleSize) * totalOrders) : 0;
  const activeShipments = totalShipments - deliveredPackages;
  const stats = [
    { label: "Total shipments", value: totalShipments, icon: <PackageOpen size={28} color="#6366f1" /> }, // Indigo
    { label: "Active shipments", value: activeShipments, icon: <Hourglass size={28} color="#f59e42" /> }, // Orange
    { label: "Packages delivered", value: deliveredPackages, icon: <CheckCircle size={28} color="#22c55e" /> }, // Emerald
    { label: "Total vehicles", value: totalVehicles, icon: <Truck size={28} color="#10b981" /> }, // Green
  ];

  if (loading) {
    return <div className="mb-6">{t('common.loading')}</div>;
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