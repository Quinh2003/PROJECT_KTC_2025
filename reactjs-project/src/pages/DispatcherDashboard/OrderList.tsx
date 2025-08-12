import { useEffect, useState } from "react";
import { fetchOrders } from "../../services/orderAPI";

export default function OrderList() {
  const [orders, setOrders] = useState<any[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    setLoading(true);
    fetchOrders()
      .then(data => {
        setOrders(data);
        setError(null);
      })
      .catch(() => setError("Không thể lấy dữ liệu đơn hàng!"))
      .finally(() => setLoading(false));
  }, []);

  return (
    <div className="bg-white/30 backdrop-blur-lg rounded-2xl p-6 border border-white/30 shadow-lg">
      <div className="text-2xl font-bold mb-2 text-gray-800">Total Orders</div>
      <div className="text-gray-600 mb-4">Track the status of all orders in the system</div>
      <div className="flex flex-col gap-4">
        {loading && <div>Đang tải dữ liệu...</div>}
        {error && <div className="text-red-500">{error}</div>}
        {!loading && !error && orders.map((order) => (
          <div key={order.id} className="rounded-xl bg-white/40 backdrop-blur-sm border border-white/40 p-4 flex justify-between items-start hover:bg-white/50 transition-all duration-300 shadow-md hover:shadow-lg">
            <div>
              <div className="flex gap-2 items-center mb-1">
                <span className="font-bold text-lg text-gray-800">{order.id}</span>
                <span className="bg-blue-200/60 backdrop-blur-sm text-blue-700 text-xs px-2 py-1 rounded border border-blue-300/50">{order.status}</span>
                <span className={`bg-orange-200/60 backdrop-blur-sm text-orange-700 text-xs px-2 py-1 rounded border border-orange-300/50`}>{order.priority}</span>
              </div>
              <div className="text-sm text-gray-700">Khách hàng: <span className="font-medium">{order.customerName || order.customer || ""}</span></div>
              <div className="text-sm text-gray-700">Đến: <span className="font-medium">{order.deliveryAddress || order.to || ""}</span></div>
            </div>
            <div className="text-right min-w-[180px]">
              <div className="text-sm text-gray-600 font-medium">{order.date || order.createdAt || ""}</div>
              <div className="text-sm text-gray-700">Từ: <span className="font-medium">{order.pickupAddress || order.from || ""}</span></div>
              <div className="text-sm text-gray-700">Tài xế: <span className="font-medium">{order.driver || order.assignedDriver || ""}</span></div>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}