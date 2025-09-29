import { useEffect, useState } from "react";
import { fetchOrderItemsByOrderIdPaged, fetchOrderTotalQuantity, type ProductItem } from "../../services/OrderItemAPI";

interface OrderRowProps {
  orderId: number | string;
}

export default function OrderRow({ orderId }: OrderRowProps) {
  const [previewItems, setPreviewItems] = useState<ProductItem[]>([]);
  const [totalQuantity, setTotalQuantity] = useState<number>(0);
  const [itemCount, setItemCount] = useState<number>(0);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  useEffect(() => {
    setLoading(true);
    setError("");
    Promise.all([
      fetchOrderItemsByOrderIdPaged(orderId, 0, 3), // Chỉ load 3 sản phẩm đầu tiên để preview
      fetchOrderTotalQuantity(orderId)
    ])
      .then(([pagedResult, total]) => {
        setPreviewItems(pagedResult.content);
        setItemCount(pagedResult.totalElements);
        setTotalQuantity(total);
      })
      .catch((err) => setError(err.message || "Lỗi tải dữ liệu"))
      .finally(() => setLoading(false));
  }, [orderId]);

  if (loading) return <div>Đang tải sản phẩm...</div>;
  if (error) return <div className="text-red-500">{error}</div>;

  return (
    <div>
      <div className="font-semibold text-blue-900 mb-1">Sản phẩm:</div>
      <ul className="list-disc ml-6 text-gray-800 text-sm">
        {previewItems.map(item => {
          // Lấy tên sản phẩm từ ProductItem
          const name = item.product?.name || "(Không rõ tên)";
          return (
            <li key={item.id}>{name} <span className="text-gray-500">(SL: {item.quantity})</span></li>
          );
        })}
        {itemCount > 3 && (
          <li className="text-blue-600 font-medium">... và {itemCount - 3} sản phẩm khác</li>
        )}
      </ul>
      <div className="mt-2 text-sm text-gray-700">
        Tổng số lượng: <span className="font-bold text-blue-700">{totalQuantity}</span>
        {itemCount > 0 && (
          <span className="ml-2 text-gray-500">({itemCount} sản phẩm)</span>
        )}
      </div>
    </div>
  );
}
