

import React, { useState, useEffect } from "react";
import type { Order } from "../../types/Order";

export default function CompletedOrdersList() {
  const [orders, setOrders] = useState<Order[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");
  const [page, setPage] = useState(1);
  const PAGE_SIZE = 5;
  const [totalPages, setTotalPages] = useState(1);
  const [totalOrders, setTotalOrders] = useState(0);
  const [selectedOrderId, setSelectedOrderId] = useState<number | null>(null);
  // State cho delivery proofs
  const [deliveryProofs, setDeliveryProofs] = useState<any[]>([]);
  const [proofsLoading, setProofsLoading] = useState(false);
  const [proofsError, setProofsError] = useState("");

  useEffect(() => {
    const fetchCompletedOrders = async () => {
      setLoading(true);
      setError("");
      try {
        const token = localStorage.getItem("token") || "";
        // Gọi API lấy đơn hàng thành công có phân trang từ backend
        const res = await fetch(`http://localhost:8080/api/orders/completed/paginated?page=${page}&size=${PAGE_SIZE}`, {
          headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
        });
        if (!res.ok) throw new Error("Không thể tải danh sách đơn hàng đã giao thành công.");
        const data = await res.json();
        setOrders(data.content || []);
        setTotalPages(data.totalPages || 1);
        setTotalOrders(data.totalElements || (data.total || 0));
      } catch (err: any) {
        setError("Không thể tải danh sách đơn hàng đã giao thành công.");
      } finally {
        setLoading(false);
      }
    };
    fetchCompletedOrders();
  }, [page]);

  // Khi chọn đơn hàng, gọi API lấy proofs
  const handleSelectOrder = async (orderId: number) => {
    setSelectedOrderId(orderId);
    setProofsLoading(true);
    setProofsError("");
    setDeliveryProofs([]);
    try {
      const token = localStorage.getItem("token") || "";
      const res = await fetch(`http://localhost:8080/api/delivery-proofs/order/${orderId}`, {
        headers: token ? { "Authorization": `Bearer ${token}` } : undefined,
      });
      if (!res.ok) throw new Error("Không thể tải minh chứng giao hàng.");
      const data = await res.json();
      setDeliveryProofs(data || []);
    } catch (err: any) {
      setProofsError("Không thể tải minh chứng giao hàng.");
    } finally {
      setProofsLoading(false);
    }
  };

  return (
    <div className="p-8 w-full">
      <h2 className="text-2xl font-bold mb-6 text-gray-800">Danh sách đơn hàng đã giao thành công</h2>
      {loading ? (
        <div className="text-lg text-gray-500">Đang tải...</div>
      ) : error ? (
        <div className="text-red-600 font-semibold">{error}</div>
      ) : orders.length === 0 ? (
        <div className="text-gray-500">Không có đơn hàng nào đã giao thành công.</div>
      ) : (
        <>
          <div className="flex flex-col gap-4">
            {orders.map((order: Order) => (
              <div key={order.id}>
                <div
                  className={`flex items-center justify-between rounded-xl border shadow-md px-6 py-4 cursor-pointer transition-all duration-150 hover:shadow-lg hover:border-blue-400 bg-white ${selectedOrderId === order.id ? "ring-2 ring-blue-400" : ""}`}
                  onClick={() => handleSelectOrder(order.id)}
                >
                  {/* Left: Order info */}
                  <div className="flex flex-col gap-2 min-w-0 flex-1">
                    <div className="flex items-center gap-3 mb-1">
                      <span className="text-lg font-bold text-blue-700">#{order.id}</span>
                      <span className={`px-2 py-0.5 rounded-full text-xs font-bold border shadow-sm
                        ${order.status?.name === 'Completed'
                          ? 'bg-green-100 text-green-800 border-green-300'
                          : 'bg-gray-100 text-gray-700 border-gray-300'}
                      `}>
                        {order.status?.name}
                      </span>
                      {order.status?.statusType && (
                        <span className={`px-2 py-0.5 rounded-full text-xs font-bold border shadow-sm
                          ${order.status?.statusType === 'High'
                            ? 'bg-red-100 text-red-700 border-red-300'
                            : order.status?.statusType === 'Medium'
                            ? 'bg-orange-100 text-orange-700 border-orange-300'
                            : 'bg-green-100 text-green-700 border-green-300'}
                        `}>
                          {order.status?.statusType}
                        </span>
                      )}
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
                        <span className="text-gray-700"> {order.address?.address}{order.address?.city ? ", " + order.address.city : ""}</span>
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
                {/* Hiển thị minh chứng nếu là đơn hàng đang chọn */}
                {selectedOrderId === order.id && (
                  <div className="bg-gray-50 border-l-4 border-blue-400 mt-2 mb-4 p-4 rounded-xl">
                    <div className="font-bold text-blue-700 mb-2">Minh chứng giao hàng:</div>
                    {proofsLoading ? (
                      <div className="text-gray-500">Đang tải minh chứng...</div>
                    ) : proofsError ? (
                      <div className="text-red-600">{proofsError}</div>
                    ) : deliveryProofs.length === 0 ? (
                      <div className="text-gray-500">Chưa có minh chứng giao hàng.</div>
                    ) : (
                      <ul className="space-y-2">
                        {deliveryProofs.map((proof: any, idx: number) => (
                          <li key={proof.id || idx} className="flex items-center gap-3 p-2 bg-white rounded shadow">
                            <span className="font-semibold text-blue-800">{proof.proofType || proof.proofTypeDisplay}</span>
                            {proof.filePath && (
                              <a href={`http://localhost:8080${proof.filePath}`} target="_blank" rel="noopener noreferrer" className="text-blue-600 underline ml-2">Tệp minh chứng</a>
                            )}
                            {proof.recipientName && (
                              <span className="ml-2 text-gray-700">Người nhận: <span className="font-semibold">{proof.recipientName}</span></span>
                            )}
                            {proof.notes && (
                              <span className="ml-2 text-gray-500 italic">({proof.notes})</span>
                            )}
                            {proof.capturedAt && (
                              <span className="ml-2 text-gray-400 text-xs">{proof.capturedAt?.slice(0, 19).replace('T', ' ')}</span>
                            )}
                          </li>
                        ))}
                      </ul>
                    )}
                  </div>
                )}
              </div>
            ))}
          </div>
          {/* Pagination controls */}
          <div className="flex justify-center items-center gap-3 mt-8">
            <button
              className="px-4 py-2 rounded-xl bg-blue-100 text-blue-700 font-bold shadow disabled:opacity-50 transition-all duration-150 hover:bg-blue-200"
              onClick={() => setPage((p: number) => Math.max(1, p - 1))}
              disabled={page === 1 || loading}
            >
              &lt; Trước
            </button>
            <span className="mx-2 text-blue-900 font-semibold text-base">Trang {page} / {totalPages}</span>
            <button
              className="px-4 py-2 rounded-xl bg-blue-100 text-blue-700 font-bold shadow disabled:opacity-50 transition-all duration-150 hover:bg-blue-200"
              onClick={() => setPage((p: number) => Math.min(totalPages, p + 1))}
              disabled={page === totalPages || loading}
            >
              Tiếp &gt;
            </button>
          </div>
          {/* Tổng số đơn hàng và trang hiện tại ở dưới */}
          <div className="text-center mt-6 text-gray-600">
            Tổng số {totalOrders} đơn hàng
            {totalPages > 1 && (
              <span className="ml-2">| Trang {page} / {totalPages}</span>
            )}
          </div>
        </>
      )}
    </div>
  );
}
