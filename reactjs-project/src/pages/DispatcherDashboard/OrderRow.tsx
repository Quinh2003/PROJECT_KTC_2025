import { useEffect, useState } from "react";
import { fetchOrderItemsByOrderId, fetchOrderTotalQuantity } from "../../services/OrderItemAPI";
import type { OrderItem } from "../../types/OrderItem";

interface OrderRowProps {
  orderId: number | string;
}

export default function OrderRow({ orderId }: OrderRowProps) {
  const [items, setItems] = useState<OrderItem[]>([]);
  const [totalQuantity, setTotalQuantity] = useState<number>(0);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState("");

  useEffect(() => {
    setLoading(true);
    setError("");
    Promise.all([
      fetchOrderItemsByOrderId(orderId),
      fetchOrderTotalQuantity(orderId)
    ])
      .then(([items, total]) => {
        setItems(items);
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
        {items.map(item => {
          // Fallback lấy tên sản phẩm
          const name = (item.product && 'name' in item.product ? item.product.name : undefined) || item.productName || "(Không rõ tên)";
          return (
            <li key={item.id}>{name} <span className="text-gray-500">(SL: {item.quantity})</span></li>
          );
        })}
      </ul>
      <div className="mt-2 text-sm text-gray-700">Tổng số lượng: <span className="font-bold text-blue-700">{totalQuantity}</span></div>
    </div>
  );
}
