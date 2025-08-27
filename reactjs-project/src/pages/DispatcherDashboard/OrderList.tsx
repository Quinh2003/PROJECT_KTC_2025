// ...existing code...

import { useState, useEffect, useCallback } from "react";
import { fetchOrders, fetchOrderById } from "../../services/OrderAPI";
import { useDispatcherContext } from "../../contexts/DispatcherContext";
import type { Order } from "../../types/Order";


export default function OrderList() {
  const { selectedOrder, setSelectedOrder } = useDispatcherContext();
  const [searchId, setSearchId] = useState("");
  const [searching, setSearching] = useState(false);

  const [page, setPage] = useState(1);
  const PAGE_SIZE = 5;
  const [orders, setOrders] = useState<Order[]>([]);
  const [totalPages, setTotalPages] = useState(1);
  // const [totalRecords, setTotalRecords] = useState(0); // Không dùng
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState("");

  // Hàm chọn đơn hàng để hiển thị route
  const handleOrderClick = (order: Order) => {
    setSelectedOrder(order);
  };

  // Hàm tìm kiếm đơn hàng theo ID
  const handleSearch = async () => {
    if (!searchId.trim()) return;
    setSearching(true);
    setError("");
    try {
      const token = localStorage.getItem("token") || "";
      const foundOrder = await fetchOrderById(searchId.trim(), token);
      if (foundOrder) {
        setOrders([foundOrder]);
        setTotalPages(1);
        setPage(1);
      } else {
        setOrders([]);
        setTotalPages(1);
        setPage(1);
        setError("Không tìm thấy đơn hàng với ID đã nhập");
      }
    } catch (err) {
      setError("Lỗi khi tìm kiếm đơn hàng");
    } finally {
      setSearching(false);
    }
  };



  const fetchOrdersCallback = useCallback(async () => {
    setLoading(true);
    setError("");
    try {
      const token = localStorage.getItem("token") || "";
      const res = await fetchOrders(page, PAGE_SIZE, token);
      setOrders(res.data);
      setTotalPages(res.totalPages);
      // setTotalRecords(res.totalRecords); // Không dùng
    } catch (err) {
      setError("Không thể tải dữ liệu đơn hàng");
    } finally {
      setLoading(false);
    }
  }, [page]);

  useEffect(() => {
    fetchOrdersCallback();
    // eslint-disable-next-line
  }, [fetchOrdersCallback]);

  // Khi xóa nội dung ô tìm kiếm, tự động trả lại danh sách đơn hàng mặc định
  useEffect(() => {
    if (searchId.trim() === "") {
      fetchOrdersCallback();
    }
    // eslint-disable-next-line
  }, [searchId, fetchOrdersCallback]);

  // const handleRefresh = () => {
  //   fetchOrdersCallback();
  // };

  return (
    <div className="bg-gradient-to-br from-blue-50/80 via-white/90 to-blue-100/80 backdrop-blur-2xl rounded-3xl p-8 border border-white/40 shadow-2xl max-w-full overflow-x-auto">
      <div className="flex flex-col md:flex-row md:items-center md:justify-between mb-6 gap-4">
        <div>
          <div className="text-3xl font-extrabold mb-2 text-blue-900 tracking-tight">Danh sách đơn hàng</div>
          <div className="text-gray-500 text-base">Theo dõi trạng thái các đơn hàng trong hệ thống</div>
          <div className="text-sm text-blue-600 mt-1">💡 Nhấn vào đơn hàng để xem đường đi trên bản đồ</div>
        </div>
        {/* Tìm kiếm đơn hàng theo ID */}
        <div className="flex items-center gap-2 bg-white/80 border border-blue-100 rounded-xl px-3 py-2 shadow">
          <input
            type="text"
            placeholder="Nhập ID đơn hàng..."
            className="px-2 py-1 rounded border border-blue-200 focus:outline-none focus:ring-2 focus:ring-blue-300 text-base"
            value={searchId}
            onChange={e => setSearchId(e.target.value)}
            onKeyDown={e => { if (e.key === 'Enter') handleSearch(); }}
            disabled={loading || searching}
          />
          <button
            onClick={handleSearch}
            disabled={!searchId.trim() || loading || searching}
            className="px-3 py-1 bg-blue-500 hover:bg-blue-600 disabled:opacity-50 text-white rounded transition-colors duration-200 font-semibold"
          >
            {searching ? "Đang tìm..." : "Tìm kiếm"}
          </button>
        </div>
        {/* <button
          onClick={handleRefresh}
          disabled={loading}
          className="px-4 py-2 bg-blue-500 hover:bg-blue-600 disabled:opacity-50 text-white rounded-lg transition-colors duration-200 flex items-center gap-2"
        >
          {loading ? (
            <>
              <div className="animate-spin w-4 h-4 border-2 border-white border-t-transparent rounded-full"></div>
              Đang tải...
            </>
          ) : (
            <>
              <svg className="w-4 h-4" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M4 4v5h.582m15.356 2A8.001 8.001 0 004.582 9m0 0H9m11 11v-5h-.581m0 0a8.003 8.003 0 01-15.357-2m15.357 2H15" />
              </svg>
              Làm mới
            </>
          )}
        </button> */}
      </div>
      
      {error ? (
        <div className="text-center py-8 px-4 bg-red-100/80 border border-red-200 rounded-xl text-red-700 font-semibold shadow flex items-center justify-center gap-2">{error}</div>
      ) : (
        <div className="relative">
          {/* Order list */}
          <div className="flex flex-col gap-4">
            {orders.map((order) => (
              <div
                key={order.id}
                onClick={() => handleOrderClick(order)}
                className={`rounded-2xl border p-5 flex flex-col md:flex-row md:items-center md:justify-between gap-4 shadow hover:shadow-xl transition-all duration-200 cursor-pointer ${
                  selectedOrder?.id === order.id 
                    ? 'bg-blue-100/90 border-blue-400 ring-2 ring-blue-300' 
                    : 'bg-white/90 border-blue-100 hover:bg-blue-50/80'
                }`}
              >
                {/* Left: Order info */}
                <div className="flex-1 min-w-0 flex flex-col gap-2">
                  <div className="flex flex-wrap gap-2 items-center mb-1">
                    <span className="font-extrabold text-lg text-blue-900">#{order.id}</span>
                    <span className={`px-2 py-0.5 rounded-full text-xs font-bold border shadow-sm
                      ${order.status?.name === 'Pending'
                        ? 'bg-yellow-100 text-yellow-800 border-yellow-300'
                        : order.status?.name === 'Completed'
                        ? 'bg-green-100 text-green-800 border-green-300'
                        : 'bg-blue-100 text-blue-700 border-blue-300'}
                    `}>
                      {order.status?.name}
                    </span>
                    <span className={`px-2 py-0.5 rounded-full text-xs font-bold border shadow-sm
                      ${order.status?.statusType === 'High'
                        ? 'bg-red-100 text-red-700 border-red-300'
                        : order.status?.statusType === 'Medium'
                        ? 'bg-orange-100 text-orange-700 border-orange-300'
                        : 'bg-green-100 text-green-700 border-green-300'}
                    `}>
                      {order.status?.statusType}
                    </span>
                  </div>
                  <div className="text-sm text-gray-700">
                    <div>
                      <span className="font-semibold text-blue-700">Khách hàng:</span>
                      <span className="text-blue-800"> {order.store?.storeName}</span>
                    </div>
                    <div>
                      <span className="font-semibold text-blue-700">Từ:</span>
                      <span className="text-gray-700"> {order.store?.address}</span>
                    </div>
                    <div>
                      <span className="font-semibold text-blue-700">Đến:</span>
                      <span className="text-gray-700"> {order.address?.address}</span>
                    </div>
                  </div>
                </div>
                {/* Right: Date & driver/vehicle */}
                <div className="flex flex-col items-end min-w-[180px] gap-1">
                  <div className="text-base text-blue-900 font-bold">{order.createdAt?.slice(0, 10)}</div>
                  <div className="text-sm text-gray-700">
                    <span className="font-semibold text-gray-500">Tài xế:</span> <span className="font-semibold text-blue-800">{order.vehicle?.currentDriver?.fullName || "Chưa phân công"}</span>
                    {order.vehicle?.licensePlate && (
                      <>
                        <span className="mx-1 text-gray-400">|</span>
                        <span className="font-semibold text-blue-800">Xe: {order.vehicle.licensePlate}</span>
                      </>
                    )}
                  </div>
                </div>
              </div>
            ))}
          </div>
          {/* No spinner overlay while loading */}
          {/* Pagination controls */}
          <div className="flex justify-center items-center gap-3 mt-8">
            <button
              className="px-4 py-2 rounded-xl bg-blue-100 text-blue-700 font-bold shadow disabled:opacity-50 transition-all duration-150 hover:bg-blue-200"
              onClick={() => setPage((p) => Math.max(1, p - 1))}
              disabled={page === 1 || loading}
            >
              &lt; Trước
            </button>
            <span className="mx-2 text-blue-900 font-semibold text-base">Trang {page} / {totalPages}</span>
            {/* <span className="mx-2 text-gray-500 text-sm">Tổng số: {totalRecords}</span> */}
            <button
              className="px-4 py-2 rounded-xl bg-blue-100 text-blue-700 font-bold shadow disabled:opacity-50 transition-all duration-150 hover:bg-blue-200"
              onClick={() => setPage((p) => Math.min(totalPages, p + 1))}
              disabled={page === totalPages || loading}
            >
              Tiếp &gt;
            </button>
          </div>
        </div>
      )}
    </div>
  );
}