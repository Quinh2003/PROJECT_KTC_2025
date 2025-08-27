import React from "react";

interface OrderDetailModalProps {
  open: boolean;
  onClose: () => void;
  orderItem: any | null;
  loading?: boolean;
  error?: string;
}

export default function OrderDetailModal({ open, onClose, orderItem, loading, error }: OrderDetailModalProps) {
  if (!open) return null;

  if (loading) {
    return (
      <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/40">
        <div className="bg-white rounded-2xl shadow-xl max-w-2xl w-full p-8 relative animate-fade-in text-center text-lg font-semibold text-blue-700">
          Đang tải chi tiết đơn hàng...
        </div>
      </div>
    );
  }
  if (error) {
    return (
      <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/40">
        <div className="bg-white rounded-2xl shadow-xl max-w-2xl w-full p-8 relative animate-fade-in text-center text-lg font-semibold text-red-700">
          {error}
          <button className="block mx-auto mt-6 px-4 py-2 bg-blue-100 rounded text-blue-700 font-bold" onClick={onClose}>Đóng</button>
        </div>
      </div>
    );
  }
  if (!orderItem) return null;

  const { id, quantity, shippingFee, notes, product, order, createdAt, updatedAt, items } = orderItem;
  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/40">
      <div className="bg-white rounded-2xl shadow-xl max-w-2xl w-full p-8 relative animate-fade-in">
        <button
          className="absolute top-4 right-4 text-gray-500 hover:text-red-500 text-2xl font-bold"
          onClick={onClose}
        >
          ×
        </button>
        <h2 className="text-2xl font-bold mb-4 text-blue-700">Chi tiết đơn hàng #{id}</h2>
        <div className="space-y-2 text-base">
          <div><b>Phí vận chuyển:</b> {shippingFee || "-"}</div>
          <div><b>Ghi chú:</b> {notes || "-"}</div>
          <div><b>Ngày tạo:</b> {createdAt ? new Date(createdAt).toLocaleString() : "-"}</div>
          <div><b>Ngày cập nhật:</b> {updatedAt ? new Date(updatedAt).toLocaleString() : "-"}</div>
        </div>
        <hr className="my-4" />
        <h3 className="font-semibold text-lg mb-2 text-blue-600">Danh sách sản phẩm</h3>
        {Array.isArray(items) && items.length > 0 ? (
          <ul className="list-disc ml-6 text-gray-800 text-sm">
            {items.map((item: any) => (
              <li key={item.id}>
                {item.product?.name || item.productName || "(Không rõ tên)"} <span className="text-gray-500">(SL: {item.quantity})</span>
              </li>
            ))}
          </ul>
        ) : <div>Không có sản phẩm</div>}
        <div className="mt-2 text-sm text-gray-700">Tổng số lượng: <span className="font-bold text-blue-700">{Array.isArray(items) ? items.reduce((sum, i) => sum + (i.quantity || 0), 0) : (quantity || 0)}</span></div>
        <hr className="my-4" />
        <h3 className="font-semibold text-lg mb-2 text-blue-600">Thông tin đơn hàng</h3>
        {order ? (
          <div className="space-y-1">
            <div><b>Mã đơn:</b> {order.id}</div>
            <div><b>Mô tả:</b> {order.description || "-"}</div>
            <div><b>Trạng thái:</b> {order.status?.name || order.status || "-"}</div>
            <div><b>Ghi chú:</b> {order.notes || "-"}</div>
            <div><b>Ngày tạo:</b> {order.createdAt ? new Date(order.createdAt).toLocaleString() : "-"}</div>
            <div><b>Người tạo:</b> {order.createdBy?.fullName || order.createdBy?.username || "-"}</div>
            <div><b>Cửa hàng:</b> {order.store?.storeName || "-"}</div>
            <div><b>Địa chỉ nhận:</b> {order.address?.address || order.toAddress || "-"}</div>
            <div><b>Tổng tiền:</b> {order.totalAmount ? order.totalAmount.toLocaleString() : "-"}</div>
            <div><b>Lợi nhuận:</b> {order.orderProfitPerOrder ? order.orderProfitPerOrder.toLocaleString() : "-"}</div>
          </div>
        ) : <div>Không có thông tin đơn hàng</div>}
        <hr className="my-4" />
        <h3 className="font-semibold text-lg mb-2 text-blue-600">Thông tin xe & tài xế</h3>
        {order?.vehicle ? (
          <div className="space-y-1">
            <div><b>Biển số xe:</b> {order.vehicle.licensePlate}</div>
            <div><b>Loại xe:</b> {order.vehicle.vehicleType}</div>
            <div><b>Sức chứa:</b> {order.vehicle.capacityWeightKg} kg</div>
            <div><b>Trạng thái xe:</b> {order.vehicle.status?.name || order.vehicle.status || "-"}</div>
            <div><b>Ghi chú xe:</b> {order.vehicle.notes || "-"}</div>
            <div><b>Tài xế:</b> {order.vehicle.currentDriver?.fullName || order.vehicle.currentDriver?.username || "-"}</div>
            <div><b>SĐT tài xế:</b> {order.vehicle.currentDriver?.phone || "-"}</div>
            <div><b>Email tài xế:</b> {order.vehicle.currentDriver?.email || "-"}</div>
          </div>
        ) : <div>Không có thông tin xe</div>}
      </div>
    </div>
  );
}
