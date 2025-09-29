"use client";
import { useEffect, useState } from "react";

interface Address {
  address: string;
  city?: string;
  state?: string;
  country?: string;
  region?: string;
  postalCode?: string;
  contactName?: string;
  contactPhone?: string;
  contactEmail?: string;
  floorNumber?: string;
  latitude?: number;
  longitude?: number;
  createdAt?: string;
  updatedAt?: string;
}

interface OrderDetail {
  orderCode?: string | null;
  id: number;
  vehicle?: {
    id: number;
    licensePlate: string;
    model?: string | null;
    vehicleType?: string | null;
    currentDriver?: {
      id: number;
      username: string;
      fullName?: string;
      email?: string;
    };
  };
  address?: Address;
  description?: string | null;
  notes?: string | null;
  orderProfitPerOrder?: number | string;
  benefitPerOrder?: number | string | null;
  status?: {
    id: number;
    statusType: string;
    name: string;
    description?: string;
  };
  store?: {
    id: number;
    storeName: string;
    email?: string;
    phone?: string;
    address?: string;
  };
  totalAmount?: number | string | null;
  createdBy?: {
    id: number;
    username: string;
    fullName?: string;
    email?: string;
  };
  createdAt?: string;
  updatedAt?: string;
  deliveryDistance?: number;
}

interface ProductItem {
  id: number;
  product: {
    name: string;
    weight?: number;
    volume?: number;
    fragile?: boolean;
  };
  quantity: number;
  shippingFee?: number | string;
  notes?: string;
}

interface OrderDetailModalProps {
  orderId: number;
  onClose: () => void;
}

export default function OrderDetailModal({
  orderId,
  onClose,
}: OrderDetailModalProps) {
  const [orderItem, setOrderItem] = useState<OrderDetail | null>(null);
  const [products, setProducts] = useState<ProductItem[]>([]);
  const [loading, setLoading] = useState(true);

  const [deliveryFee, setDeliveryFee] = useState<number | string | null>(null);
  const [serviceType, setServiceType] = useState<string | null>(null);

  useEffect(() => {
    async function fetchData() {
      setLoading(true);
      try {
        const token =
          typeof window !== "undefined" ? localStorage.getItem("token") : "";
        const resOrder = await fetch(
          `http://localhost:8080/api/orders/${orderId}`,
          {
            headers: token ? { Authorization: `Bearer ${token}` } : {},
          }
        );
        const orderData = await resOrder.json();
        setOrderItem(orderData as OrderDetail);

        const resDelivery = await fetch(
          `http://localhost:8080/api/deliveries/order/${orderId}`,
          {
            headers: token ? { Authorization: `Bearer ${token}` } : {},
          }
        );
        const deliveryData = await resDelivery.json();
        if (deliveryData && deliveryData.length > 0) {
          if (deliveryData[0].deliveryFee)
            setDeliveryFee(deliveryData[0].deliveryFee);
          if (deliveryData[0].serviceType)
            setServiceType(deliveryData[0].serviceType);
        }

        const resProducts = await fetch(
          `http://localhost:8080/api/order-items/order/${orderId}/paged?page=0&size=10`,
          { headers: token ? { Authorization: `Bearer ${token}` } : {} }
        );
        const productsData = await resProducts.json();
        const productItems = (productsData.content || []) as ProductItem[];
        setProducts(productItems);
      } catch (err) {
        console.error("Fetch order detail failed:", err);
        setOrderItem(null);
        setProducts([]);
        setDeliveryFee(null);
        setServiceType(null);
      } finally {
        setLoading(false);
      }
    }
    if (orderId) fetchData();
  }, [orderId]);

  if (!orderId) return null;

  return (
    <div
      className="fixed inset-0 bg-[rgba(0,0,0,0.15)] flex items-center justify-center z-[100]"
      onClick={onClose}
    >
      <div
        className="bg-white rounded-lg p-4 max-w-3xl w-full mx-2 max-h-[80vh] overflow-y-auto shadow-lg relative"
        onClick={(e) => e.stopPropagation()}
      >
        <button
          className="absolute top-2 right-2 text-gray-500 hover:text-gray-700 text-xl"
          onClick={onClose}
          aria-label="Đóng"
        >
          ×
        </button>
        <div className="flex justify-between items-center mb-4">
          <h2 className="text-2xl font-bold text-gray-900">
            Chi tiết đơn hàng
          </h2>
        </div>

        {loading ? (
          <div className="min-h-[200px] flex items-center justify-center text-lg text-gray-500">
            Đang tải dữ liệu...
          </div>
        ) : !orderItem ? (
          <div className="min-h-[200px] flex items-center justify-center text-lg text-red-500">
            Không tìm thấy đơn hàng
          </div>
        ) : (
          <div className="space-y-6">
            {/* Thông tin giao hàng */}
            <div>
              <h3 className="text-lg font-bold text-gray-800 mb-3">
                Thông tin giao hàng
              </h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-8">
                {/* Địa chỉ lấy hàng */}
                <div className="space-y-4">
                  <div>
                    <div className="flex items-center">
                      <span className="text-rose-500 mr-2">📍</span>
                      <h4 className="text-blue-600 font-medium">
                        Địa chỉ lấy hàng
                      </h4>
                    </div>
                    <div className="pl-6 space-y-1 text-sm">
                      <p>
                        <span className="font-medium">Tên cửa hàng:</span>{" "}
                        {orderItem.store?.storeName || "Store"}
                      </p>
                      <p>
                        <span className="font-medium">Số điện thoại:</span>{" "}
                        {orderItem.store?.phone || "0905257378"}
                      </p>
                      <p>
                        <span className="font-medium">Email:</span>{" "}
                        {orderItem.store?.email || "banhcanh@gmail.com"}
                      </p>
                      <p>
                        <span className="font-medium">Địa chỉ:</span>{" "}
                        {orderItem.store?.address ||
                          "85 Lê Thành Tôn, Q1, TP.HCM"}
                      </p>
                    </div>
                  </div>
                </div>
                {/* Địa chỉ nhận hàng */}
                <div className="space-y-4">
                  <div>
                    <div className="flex items-center">
                      <span className="text-green-500 mr-2">🎁</span>
                      <h4 className="text-blue-600 font-medium">
                        Địa chỉ nhận hàng
                      </h4>
                    </div>
                    <div className="pl-6 space-y-1 text-sm">
                      <p>
                        <span className="font-medium">Tên người nhận:</span>{" "}
                        {orderItem.address?.contactName || "Ngọc Ngọc"}
                      </p>
                      <p>
                        <span className="font-medium">Số điện thoại:</span>{" "}
                        {orderItem.address?.contactPhone || "0934836173"}
                      </p>
                      <p>
                        <span className="font-medium">Email:</span>{" "}
                        {orderItem.address?.contactEmail || "lien@gmail.com"}
                      </p>
                      <p>
                        <span className="font-medium">Địa chỉ:</span>{" "}
                        {orderItem.address?.address || "Xã Bằng Vân, Bắc Kạn"}
                      </p>
                    </div>
                  </div>
                </div>
              </div>
            </div>

            {/* Danh sách sản phẩm */}
            <div>
              <h3 className="text-lg font-bold text-gray-800 mb-3">
                Danh sách sản phẩm
              </h3>
              <div className="overflow-x-auto">
                <table className="min-w-full text-sm border-collapse">
                  <thead className="bg-gray-100">
                    <tr>
                      <th className="py-2 px-3 text-left border border-gray-200">
                        Tên sản phẩm
                      </th>
                      <th className="py-2 px-3 text-left border border-gray-200">
                        Số lượng
                      </th>
                      <th className="py-2 px-3 text-left border border-gray-200">
                        Cân nặng (kg)
                      </th>
                      <th className="py-2 px-3 text-left border border-gray-200">
                        Thể tích (cm³)
                      </th>
                      <th className="py-2 px-3 text-left border border-gray-200">
                        Hàng dễ vỡ
                      </th>
                      {products?.some((item) => item.shippingFee) && (
                        <th className="py-2 px-3 text-left border border-gray-200">
                          Phí vận chuyển
                        </th>
                      )}
                    </tr>
                  </thead>
                  <tbody>
                    {products && products.length > 0 ? (
                      products.map((item, index) => {
                        const formattedVolume = item.product?.volume
                          ? item.product.volume.toLocaleString("vi-VN")
                          : "";
                        return (
                          <tr
                            key={item.id}
                            className={
                              index % 2 === 0 ? "bg-white" : "bg-gray-50"
                            }
                          >
                            <td className="py-2 px-3 border border-gray-200">
                              {item.product?.name || `Sản phẩm ${index + 1}`}
                            </td>
                            <td className="py-2 px-3 border border-gray-200">
                              {item.quantity}
                            </td>
                            <td className="py-2 px-3 border border-gray-200">
                              {item.product?.weight
                                ? `${item.product.weight} kg`
                                : ""}
                            </td>
                            <td className="py-2 px-3 border border-gray-200">
                              {formattedVolume ? `${formattedVolume} cm³` : ""}
                            </td>
                            <td className="py-2 px-3 border border-gray-200">
                              {item.product?.fragile !== undefined
                                ? item.product.fragile
                                  ? "Có"
                                  : "Không"
                                : ""}
                            </td>
                            {products.some((item) => item.shippingFee) && (
                              <td className="py-2 px-3 border border-gray-200 text-blue-600">
                                {(() => {
                                  if (!item.shippingFee) return "";
                                  const fee =
                                    typeof item.shippingFee === "string"
                                      ? Number(
                                          item.shippingFee.replace(/,/g, "")
                                        )
                                      : Number(item.shippingFee);
                                  return isNaN(fee)
                                    ? ""
                                    : `${fee.toLocaleString("vi-VN")} đ`;
                                })()}
                              </td>
                            )}
                          </tr>
                        );
                      })
                    ) : (
                      <tr className="bg-white">
                        <td
                          colSpan={6}
                          className="py-4 px-3 border border-gray-200 text-center text-gray-500"
                        >
                          Không có sản phẩm nào
                        </td>
                      </tr>
                    )}
                  </tbody>
                </table>
              </div>
            </div>

            {/* Chi phí */}
            <div>
              <h3 className="text-lg font-bold text-gray-800 mb-3">Chi phí</h3>
              <div className="bg-gray-50 p-4 rounded-lg">
                {serviceType && (
                  <div className="mb-2">
                    <div className="flex items-center mb-2">
                      <span className="text-red-500 mr-2">❇️</span>
                      <label className="block text-sm font-medium text-gray-700">
                        Loại dịch vụ
                      </label>
                    </div>
                    <div className="border border-gray-300 rounded p-2 bg-white">
                      <p className="text-gray-900">
                        {serviceType === "STANDARD"
                          ? "Tiêu chuẩn"
                          : serviceType}
                      </p>
                    </div>
                  </div>
                )}
                <div className="flex justify-between items-center font-bold mt-4">
                  <span>Tổng phí vận chuyển:</span>
                  <span className="text-blue-600">
                    {(() => {
                      if (deliveryFee) {
                        const fee =
                          typeof deliveryFee === "string"
                            ? Number(deliveryFee.replace(/,/g, ""))
                            : Number(deliveryFee);
                        return isNaN(fee) ? "0" : fee.toLocaleString("vi-VN");
                      }
                      return "0";
                    })()}{" "}
                    đ
                  </span>
                </div>
              </div>
            </div>

            {/* Vận chuyển */}
            {orderItem.vehicle && (
              <div>
                <h3 className="text-lg font-bold text-gray-800 mb-3">
                  Thông tin vận chuyển
                </h3>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div>
                    <label className="block text-sm font-medium text-gray-700">
                      Biển số - Loại xe
                    </label>
                    <p className="text-gray-900">
                      {orderItem.vehicle.licensePlate}
                      {orderItem.vehicle.vehicleType
                        ? ` - ${orderItem.vehicle.vehicleType}`
                        : ""}
                    </p>
                  </div>
                  <div>
                    <label className="block text-sm font-medium text-gray-700">
                      Tài xế
                    </label>
                    <p className="text-gray-900">
                      {orderItem.vehicle.currentDriver
                        ? orderItem.vehicle.currentDriver.fullName ||
                          orderItem.vehicle.currentDriver.username
                        : ""}
                    </p>
                  </div>
                </div>
              </div>
            )}

            {orderItem.description && (
              <div>
                <h3 className="text-lg font-bold text-gray-800 mb-3">
                  Ghi chú đơn hàng
                </h3>
                <p className="text-gray-900 bg-gray-50 p-3 rounded">
                  {orderItem.description}
                </p>
              </div>
            )}
          </div>
        )}
      </div>
    </div>
  );
}
