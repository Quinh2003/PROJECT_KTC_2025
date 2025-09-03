interface OrderDetailModalProps {
  open: boolean;
  onClose: () => void;
  orderItem: {
    code: string;
    customer: string;
    status: string;
    date: string;
    address: string;
    from: string;
    to: string;
    note?: string;
    description?: string;
    assignedVehicle?: {
      licensePlate: string;
      vehicleType: string;
    };
    currentDriver?: {
      fullName?: string;
      username: string;
    };
  } | null;
}

export default function OrderDetailModal({ open, onClose, orderItem }: OrderDetailModalProps) {
  if (!open || !orderItem) return null;

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
      <div className="bg-white rounded-lg p-6 max-w-2xl w-full mx-4 max-h-[90vh] overflow-y-auto">
        <div className="flex justify-between items-center mb-4">
          <h2 className="text-2xl font-bold text-gray-900">Chi tiết đơn hàng</h2>
          <button
            onClick={onClose}
            className="text-gray-500 hover:text-gray-700 text-2xl"
          >
            ×
          </button>
        </div>
        
        <div className="space-y-4">
          <div className="grid grid-cols-2 gap-4">
            <div>
              <label className="block text-sm font-medium text-gray-700">Mã đơn</label>
              <p className="text-gray-900">{orderItem.code}</p>
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700">Khách hàng</label>
              <p className="text-gray-900">{orderItem.customer}</p>
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700">Trạng thái</label>
              <p className="text-gray-900">{orderItem.status}</p>
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700">Ngày tạo</label>
              <p className="text-gray-900">{orderItem.date}</p>
            </div>
          </div>
          
          <div>
            <label className="block text-sm font-medium text-gray-700">Địa chỉ giao hàng</label>
            <p className="text-gray-900">{orderItem.address}</p>
          </div>
          
          <div>
            <label className="block text-sm font-medium text-gray-700">Lộ trình</label>
            <div className="text-gray-900">
              <p><strong>Từ:</strong> {orderItem.from}</p>
              <p><strong>Đến:</strong> {orderItem.to}</p>
            </div>
          </div>
          
          {orderItem.note && (
            <div>
              <label className="block text-sm font-medium text-gray-700">Ghi chú</label>
              <p className="text-gray-900">{orderItem.note}</p>
            </div>
          )}
          
          {orderItem.description && (
            <div>
              <label className="block text-sm font-medium text-gray-700">Mô tả</label>
              <p className="text-gray-900">{orderItem.description}</p>
            </div>
          )}
          
          {orderItem.assignedVehicle && (
            <div>
              <label className="block text-sm font-medium text-gray-700">Xe được gán</label>
              <p className="text-gray-900">
                {orderItem.assignedVehicle.licensePlate} - {orderItem.assignedVehicle.vehicleType}
              </p>
            </div>
          )}
          
          {orderItem.currentDriver && (
            <div>
              <label className="block text-sm font-medium text-gray-700">Tài xế</label>
              <p className="text-gray-900">
                {orderItem.currentDriver.fullName || orderItem.currentDriver.username}
              </p>
            </div>
          )}
        </div>
        
        <div className="flex justify-end mt-6">
          <button
            onClick={onClose}
            className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
          >
            Đóng
          </button>
        </div>
      </div>
    </div>
  );
}
