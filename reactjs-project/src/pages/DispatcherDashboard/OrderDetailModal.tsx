import React from "react";
import OrderChecklistTimeline from "../../components/OrderChecklistTimeline";

import { useTranslation } from 'react-i18next';

interface ProductItem {
  id: number;
  product: {
    name: string;
    weight?: number;
    volume?: number;
    fragile?: boolean;
  };
  quantity: number;
  shippingFee?: number;
  notes?: string;
}

interface OrderDetailModalProps {
  open: boolean;
  onClose: () => void;
  orderItem: {
    id?: number;
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
    addressDetail?: {
      contactName?: string;
      contactPhone?: string;
    };
  } | null;
  products?: ProductItem[];
  deliveryFee?: number;
  productsPage?: number;
  productsTotalPages?: number;
  onProductsPageChange?: (page: number) => void;
}
export default function OrderDetailModal({ open, onClose, orderItem, products, deliveryFee, productsPage = 0, productsTotalPages = 1, onProductsPageChange }: OrderDetailModalProps) {

  const { t } = useTranslation();
  if (!open || !orderItem) return null;

  // Debug log để kiểm tra dữ liệu truyền vào modal
  console.log('🔍 OrderDetailModal - orderItem received:', orderItem);
  console.log('🔍 OrderDetailModal - orderItem.id:', orderItem?.id);
  console.log('🔍 OrderDetailModal - orderItem.code:', orderItem?.code);
  console.log('🔍 OrderDetailModal - orderItem.addressDetail:', orderItem.addressDetail);

  // Lấy thông tin người nhận từ orderItem.address nếu có dạng object
  let contactName: string | undefined = undefined;
  let contactPhone: string | undefined = undefined;

  // Lấy từ orderItem.address nếu có
  if (orderItem && typeof orderItem.address === 'object' && orderItem.address !== null) {
    contactName = (orderItem.address as any).contactName;
    contactPhone = (orderItem.address as any).contactPhone;
  }

  // Nếu không có, lấy từ orderItem.order.address nếu có
  if ((!contactName || !contactPhone) && (orderItem as any).order?.address) {
    contactName = (orderItem as any).order.address.contactName;
    contactPhone = (orderItem as any).order.address.contactPhone;
  }

  // Ưu tiên addressDetail từ props, nếu không có thì dùng từ address object hoặc order.address
  const addressDetail = orderItem.addressDetail || { 
    contactName: contactName || undefined, 
    contactPhone: contactPhone || undefined 
  };

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
      <div className="bg-white rounded-lg p-6 max-w-2xl w-full mx-4 max-h-[90vh] overflow-y-auto">
        <div className="flex justify-between items-center mb-4">
          <h2 className="text-2xl font-bold text-gray-900">Order Details</h2>
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
              <label className="block text-sm font-medium text-gray-700">Order Code</label>
              <p className="text-gray-900">{orderItem?.code}</p>
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700">Customer</label>
              <p className="text-gray-900">{orderItem?.customer}</p>
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700">Status</label>
              <p className="text-gray-900">{orderItem?.status}</p>
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700">Created Date</label>
              <p className="text-gray-900">{orderItem?.date}</p>
            </div>
          </div>

          {/* Thông tin người nhận */}
          {(addressDetail?.contactName || addressDetail?.contactPhone) && (
            <div className="grid grid-cols-2 gap-4">
              {addressDetail?.contactName && (
                <div>
                  <label className="block text-sm font-medium text-gray-700">Tên người nhận</label>
                  <p className="text-gray-900">{addressDetail.contactName}</p>
                </div>
              )}
              {addressDetail?.contactPhone && (
                <div>
                  <label className="block text-sm font-medium text-gray-700">Số điện thoại người nhận</label>
                  <p className="text-gray-900">{addressDetail.contactPhone}</p>
                </div>
              )}
            </div>
          )}

          <div>
            <label className="block text-sm font-medium text-gray-700">Địa chỉ giao hàng</label>
            <p className="text-gray-900">
              {typeof orderItem.address === 'object' && orderItem.address !== null
                ? (orderItem.address as any).address || JSON.stringify(orderItem.address)
                : orderItem.address}
            </p>
          </div>

          <div>
            <label className="block text-sm font-medium text-gray-700">{t('common.route', 'Route')}</label>
            <div className="text-gray-900">
              <p><strong>{t('common.from', 'From')}:</strong> {orderItem.from}</p>
              <p><strong>{t('common.to', 'To')}:</strong> {orderItem.to}
                {typeof orderItem.address === 'object' && (orderItem.address as any)?.city ? ", " + (orderItem.address as any).city : ""}
              </p>
            </div>
          </div>

          {/* Hiển thị danh sách sản phẩm */}
          {products && products.length > 0 && (
            <div>
              <label className="block text-sm font-medium text-gray-700">Products</label>
              <table className="min-w-full text-sm mt-2">
                <thead>
                  <tr>
                    <th className="text-left">Product Name</th>
                    <th className="text-left">Quantity</th>
                    <th className="text-left">Weight</th>
                    <th className="text-left">Volume</th>
                    <th className="text-left">Fragile</th>
                    <th className="text-left">Shipping Fee</th>
                    <th className="text-left">Notes</th>
                  </tr>
                </thead>
                <tbody>
                  {products.map(item => (
                    <tr key={item.id}>
                      <td>{item.product?.name}</td>
                      <td>{item.quantity}</td>
                      <td>{item.product?.weight !== undefined ? item.product.weight : ""}</td>
                      <td>{item.product?.volume !== undefined ? item.product.volume : ""}</td>
                      <td>{item.product?.fragile !== undefined ? (item.product.fragile ? t('common.yes', 'Yes') : t('common.no', 'No')) : ""}</td>
                      <td>{
                        item.shippingFee
                          ? Number(String(item.shippingFee).replace(/,/g, "")).toLocaleString() + " đ"
                          : ""
                      }</td>
                      <td>{item.notes || ""}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
              {/* Phân trang cho sản phẩm */}
              {productsTotalPages > 1 && onProductsPageChange && (
                <div className="flex justify-center items-center gap-2 mt-4">
                  <button
                    onClick={() => onProductsPageChange(productsPage - 1)}
                    disabled={productsPage === 0}
                    className="px-3 py-1 rounded bg-blue-100 hover:bg-blue-200 disabled:opacity-50 text-blue-700 font-semibold text-sm"
                  >
                    &lt; Previous
                  </button>
                  <span className="text-sm text-gray-600">
                    Page {productsPage + 1} / {productsTotalPages}
                  </span>
                  <button
                    onClick={() => onProductsPageChange(productsPage + 1)}
                    disabled={productsPage >= productsTotalPages - 1}
                    className="px-3 py-1 rounded bg-blue-100 hover:bg-blue-200 disabled:opacity-50 text-blue-700 font-semibold text-sm"
                  >
                    Next &gt;
                  </button>
                </div>
              )}
            </div>
          )}

          {/* Hiển thị phí giao hàng */}
          {typeof deliveryFee === "number" && (
            <div>
              <label className="block text-sm font-medium text-gray-700">Shipping Fee</label>
              <p className="text-gray-900">{deliveryFee.toLocaleString()} đ</p>
            </div>
          )}

          {orderItem?.note && (
            <div>
              <label className="block text-sm font-medium text-gray-700">Notes</label>
              <p className="text-gray-900">{orderItem?.note}</p>
            </div>
          )}

          {orderItem?.description && (
            <div>
              <label className="block text-sm font-medium text-gray-700">Description</label>
              <p className="text-gray-900">{orderItem?.description}</p>
            </div>
          )}

          {orderItem?.assignedVehicle && (
            <div>
              <label className="block text-sm font-medium text-gray-700">Assigned Vehicle</label>
              <p className="text-gray-900">
                {orderItem?.assignedVehicle.licensePlate} - {orderItem?.assignedVehicle.vehicleType}
              </p>
            </div>
          )}

          {orderItem?.currentDriver && (
            <div>
              <label className="block text-sm font-medium text-gray-700">Driver</label>
              <p className="text-gray-900">
                {orderItem?.currentDriver.fullName || orderItem?.currentDriver.username}
              </p>
            </div>
          )}
        </div>


          {/* Checklist timeline dọc */}
          <div>
            <h3 className="text-lg font-semibold mb-4">Chi tiết tiến trình đơn hàng</h3>
            {orderItem?.id ? (
              <OrderChecklistTimeline orderId={orderItem.id} />
            ) : (
              <div className="text-center py-4 text-gray-500">
                Không thể tải chi tiết tiến trình đơn hàng (thiếu ID đơn hàng)
              </div>
            )}
          </div>

        <div className="flex justify-end mt-6">
          <button
            onClick={onClose}
            className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
          >
            Close
          </button>
        </div>
      </div>
    </div>
  );
}
