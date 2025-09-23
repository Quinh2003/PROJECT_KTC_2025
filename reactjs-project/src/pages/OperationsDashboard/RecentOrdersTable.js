import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import DataTable, { TableRow, TableCell } from '../../components/DataTable';
import GlassButton from '../../components/GlassButton';
function getStatusColor(status) {
    switch (status) {
        case 'DELIVERED': return 'text-green-600';
        case 'IN_TRANSIT': return 'text-blue-600';
        case 'PICKED_UP': return 'text-yellow-600';
        case 'ASSIGNED': return 'text-purple-600';
        case 'PENDING': return 'text-orange-600';
        case 'CANCELLED': return 'text-red-600';
        default: return 'text-gray-800';
    }
}
function getStatusText(status) {
    switch (status) {
        case 'DELIVERED': return 'Đã giao';
        case 'IN_TRANSIT': return 'Đang giao';
        case 'PICKED_UP': return 'Đã lấy hàng';
        case 'ASSIGNED': return 'Đã phân công';
        case 'PENDING': return 'Chờ xử lý';
        case 'CANCELLED': return 'Đã hủy';
        default: return status;
    }
}
function getPriorityColor(priority) {
    switch (priority) {
        case 'URGENT': return 'text-red-400 bg-red-500/20';
        case 'HIGH': return 'text-orange-400 bg-orange-500/20';
        case 'MEDIUM': return 'text-yellow-400 bg-yellow-500/20';
        case 'LOW': return 'text-green-400 bg-green-500/20';
        default: return 'text-white bg-white/20';
    }
}
function calculateEfficiency(order) {
    if (!order.actualDeliveryTime || !order.estimatedDeliveryTime)
        return 100;
    const estimated = new Date(order.estimatedDeliveryTime).getTime();
    const actual = new Date(order.actualDeliveryTime).getTime();
    const diff = (actual - estimated) / (1000 * 60); // minutes
    if (diff <= 0)
        return 100;
    if (diff <= 30)
        return 95;
    if (diff <= 60)
        return 85;
    return Math.max(50, 100 - Math.floor(diff / 30) * 10);
}
function getEfficiencyColor(efficiency) {
    if (efficiency >= 90)
        return 'text-green-400';
    if (efficiency >= 75)
        return 'text-yellow-400';
    return 'text-red-400';
}
function formatDuration(start, end) {
    if (!end)
        return '--';
    const startTime = new Date(start).getTime();
    const endTime = new Date(end).getTime();
    const diff = Math.abs(endTime - startTime) / (1000 * 60 * 60); // hours
    return `${diff.toFixed(1)}h`;
}
export default function RecentOrdersTable({ orders, onRefresh, loading }) {
    return (_jsxs("div", { className: "space-y-4", children: [_jsxs("div", { className: "flex items-center justify-between", children: [_jsx("h3", { className: "text-lg font-medium", children: "\u0110\u01A1n h\u00E0ng g\u1EA7n \u0111\u00E2y" }), _jsx(GlassButton, { size: "sm", variant: "secondary", onClick: onRefresh, disabled: loading, children: "\uD83D\uDD04 L\u00E0m m\u1EDBi" })] }), _jsx(DataTable, { headers: ['Mã đơn', 'Khách hàng', 'Tuyến đường', 'Thời gian', 'Hiệu suất', 'Ưu tiên', 'Trạng thái', 'Hành động'], children: orders.map((order) => (_jsxs(TableRow, { children: [_jsxs(TableCell, { children: [_jsx("div", { className: "font-medium", children: order.id }), _jsx("div", { className: "text-gray-600 text-xs", children: new Date(order.createdAt).toLocaleDateString('vi-VN') })] }), _jsxs(TableCell, { children: [_jsx("div", { className: "font-medium", children: order.customerName }), _jsx("div", { className: "text-gray-600 text-xs", children: order.customerPhone })] }), _jsx(TableCell, { children: _jsxs("div", { className: "text-sm", children: [order.pickupAddress, " \u2192 ", order.deliveryAddress] }) }), _jsx(TableCell, { children: formatDuration(order.createdAt, order.actualDeliveryTime) }), _jsx(TableCell, { children: _jsxs("span", { className: `font-medium ${getEfficiencyColor(calculateEfficiency(order))}`, children: [calculateEfficiency(order), "%"] }) }), _jsx(TableCell, { children: _jsx("span", { className: `px-2 py-1 rounded-full text-xs font-medium ${getPriorityColor(order.priority)}`, children: order.priority }) }), _jsx(TableCell, { children: _jsx("span", { className: `font-medium ${getStatusColor(order.status)}`, children: getStatusText(order.status) }) }), _jsx(TableCell, { children: _jsxs("div", { className: "flex gap-2", children: [_jsx(GlassButton, { size: "sm", variant: "ocean", children: "Chi ti\u1EBFt" }), _jsx(GlassButton, { size: "sm", variant: "green", children: "B\u00E1o c\u00E1o" })] }) })] }, order.id))) })] }));
}
