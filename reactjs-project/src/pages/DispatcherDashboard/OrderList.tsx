import { useState } from "react";
import { useDispatcherContext } from "../../contexts/DispatcherContext";

export default function OrderList() {
  const { orders, ordersLoading, ordersError, refreshOrders } = useDispatcherContext();
  const [page, setPage] = useState(1);
  const PAGE_SIZE = 3;

  // Pagination logic
  const totalPages = Math.ceil(orders.length / PAGE_SIZE);
  const paginatedOrders = orders.slice((page - 1) * PAGE_SIZE, page * PAGE_SIZE);

  const handleRefresh = () => {
    refreshOrders(true); // Force refresh
  };

  return (
    <div className="bg-gradient-to-br from-blue-50/80 via-white/90 to-blue-100/80 backdrop-blur-2xl rounded-3xl p-8 border border-white/40 shadow-2xl max-w-full overflow-x-auto">
      <div className="flex items-center justify-between mb-6">
        <div>
          <div className="text-3xl font-extrabold mb-2 text-blue-900 tracking-tight">Danh sách đơn hàng</div>
          <div className="text-gray-500 text-base">Theo dõi trạng thái các đơn hàng trong hệ thống</div>
        </div>
        <button
          onClick={handleRefresh}
          disabled={ordersLoading}
          className="px-4 py-2 bg-blue-500 hover:bg-blue-600 disabled:opacity-50 text-white rounded-lg transition-colors duration-200 flex items-center gap-2"
        >
          {ordersLoading ? (
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
        </button>
      </div>
      
      {ordersLoading ? (
        <div className="text-center py-12 text-gray-500 text-lg animate-pulse">Đang tải dữ liệu...</div>
      ) : ordersError ? (
        <div className="text-center py-8 px-4 bg-red-100/80 border border-red-200 rounded-xl text-red-700 font-semibold shadow flex items-center justify-center gap-2">{ordersError}</div>
      ) : (
        <>
          <div className="flex flex-col gap-3">
            {paginatedOrders.map((order) => (
              <div key={order.id} className="rounded-xl bg-white/80 border border-blue-100 p-3 flex flex-col md:flex-row md:justify-between md:items-center gap-2 hover:bg-blue-50/80 transition-all duration-200 shadow hover:shadow-lg">
                <div className="flex-1 min-w-0">
                  <div className="flex flex-wrap gap-2 items-center mb-1">
                    <span className="font-extrabold text-base text-blue-900">#{order.id}</span>
                    <span className={`px-2 py-0.5 rounded-full text-[11px] font-bold border shadow-sm
                      ${order.status === 'Pending'
                        ? 'bg-yellow-100 text-yellow-800 border-yellow-300'
                        : order.status === 'Completed'
                        ? 'bg-green-100 text-green-800 border-green-300'
                        : 'bg-blue-100 text-blue-700 border-blue-300'}
                    `}>
                      {typeof order.status === "string" ? order.status : order.status.name}
                    </span>
                    <span className={`px-2 py-0.5 rounded-full text-[11px] font-bold border shadow-sm
                      ${order.priority === 'High'
                        ? 'bg-red-100 text-red-700 border-red-300'
                        : order.priority === 'Medium'
                        ? 'bg-orange-100 text-orange-700 border-orange-300'
                        : 'bg-green-100 text-green-700 border-green-300'}
                    `}>
                      {order.priority}
                    </span>
                  </div>
                  <div className="flex flex-wrap gap-4 text-sm text-gray-700">
                    <div>Khách hàng: <span className="font-semibold text-blue-800">{order.customer}</span></div>
                    <div>Đến: <span className="font-semibold text-blue-800">{order.to}</span></div>
                  </div>
                </div>
                <div className="text-right min-w-[160px] flex flex-col gap-1">
                  <div className="text-sm text-blue-900 font-bold">{order.date}</div>
                  <div className="text-sm text-gray-700">Từ: <span className="font-semibold text-blue-800">{order.from}</span></div>
                  <div className="text-sm text-gray-700">
                    Tài xế: <span className="font-semibold text-blue-800">{order.driver || "Chưa phân công"}</span>
                    {order.vehicle && (
                      <>
                        <span className="mx-1 text-gray-400">|</span>
                        <span className="font-semibold text-blue-800">Xe: {order.vehicle}</span>
                      </>
                    )}
                  </div>
                </div>
              </div>
            ))}
          </div>
          {/* Pagination controls */}
          <div className="flex justify-center items-center gap-3 mt-8">
            <button
              className="px-4 py-2 rounded-xl bg-blue-100 text-blue-700 font-bold shadow disabled:opacity-50 transition-all duration-150 hover:bg-blue-200"
              onClick={() => setPage((p) => Math.max(1, p - 1))}
              disabled={page === 1}
            >
              &lt; Trước
            </button>
            <span className="mx-2 text-blue-900 font-semibold text-base">Trang {page} / {totalPages}</span>
            <button
              className="px-4 py-2 rounded-xl bg-blue-100 text-blue-700 font-bold shadow disabled:opacity-50 transition-all duration-150 hover:bg-blue-200"
              onClick={() => setPage((p) => Math.min(totalPages, p + 1))}
              disabled={page === totalPages}
            >
              Tiếp &gt;
            </button>
          </div>
        </>
      )}
    </div>
  );
}