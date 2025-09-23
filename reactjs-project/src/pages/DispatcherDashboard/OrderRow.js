import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useEffect, useState } from "react";
import { fetchOrderItemsByOrderIdPaged, fetchOrderTotalQuantity } from "../../services/OrderItemAPI";
export default function OrderRow({ orderId }) {
    const [previewItems, setPreviewItems] = useState([]);
    const [totalQuantity, setTotalQuantity] = useState(0);
    const [itemCount, setItemCount] = useState(0);
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
    if (loading)
        return _jsx("div", { children: "\u0110ang t\u1EA3i s\u1EA3n ph\u1EA9m..." });
    if (error)
        return _jsx("div", { className: "text-red-500", children: error });
    return (_jsxs("div", { children: [_jsx("div", { className: "font-semibold text-blue-900 mb-1", children: "S\u1EA3n ph\u1EA9m:" }), _jsxs("ul", { className: "list-disc ml-6 text-gray-800 text-sm", children: [previewItems.map(item => {
                        // Lấy tên sản phẩm từ ProductItem
                        const name = item.product?.name || "(Không rõ tên)";
                        return (_jsxs("li", { children: [name, " ", _jsxs("span", { className: "text-gray-500", children: ["(SL: ", item.quantity, ")"] })] }, item.id));
                    }), itemCount > 3 && (_jsxs("li", { className: "text-blue-600 font-medium", children: ["... v\u00E0 ", itemCount - 3, " s\u1EA3n ph\u1EA9m kh\u00E1c"] }))] }), _jsxs("div", { className: "mt-2 text-sm text-gray-700", children: ["T\u1ED5ng s\u1ED1 l\u01B0\u1EE3ng: ", _jsx("span", { className: "font-bold text-blue-700", children: totalQuantity }), itemCount > 0 && (_jsxs("span", { className: "ml-2 text-gray-500", children: ["(", itemCount, " s\u1EA3n ph\u1EA9m)"] }))] })] }));
}
