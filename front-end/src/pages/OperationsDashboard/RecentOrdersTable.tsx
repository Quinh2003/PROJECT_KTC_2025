import DataTable, { TableRow, TableCell } from '../../components/DataTable';
import GlassButton from '../../components/GlassButton';
import type { Order } from '../../types/dashboard';

interface RecentOrdersTableProps {
  orders: Order[];
  onRefresh: () => void;
  loading: boolean;
}

function getStatusColor(status: Order['status']) {
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

function getStatusText(status: Order['status']) {
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

function getPriorityColor(priority: Order['priority']) {
  switch (priority) {
    case 'URGENT': return 'text-red-400 bg-red-500/20';
    case 'HIGH': return 'text-orange-400 bg-orange-500/20';
    case 'MEDIUM': return 'text-yellow-400 bg-yellow-500/20';
    case 'LOW': return 'text-green-400 bg-green-500/20';
    default: return 'text-white bg-white/20';
  }
}

function calculateEfficiency(order: Order) {
  if (!order.actualDeliveryTime || !order.estimatedDeliveryTime) return 100;
  const estimated = new Date(order.estimatedDeliveryTime).getTime();
  const actual = new Date(order.actualDeliveryTime).getTime();
  const diff = (actual - estimated) / (1000 * 60); // minutes
  if (diff <= 0) return 100;
  if (diff <= 30) return 95;
  if (diff <= 60) return 85;
  return Math.max(50, 100 - Math.floor(diff / 30) * 10);
}

function getEfficiencyColor(efficiency: number) {
  if (efficiency >= 90) return 'text-green-400';
  if (efficiency >= 75) return 'text-yellow-400';
  return 'text-red-400';
}

function formatDuration(start: string, end?: string) {
  if (!end) return '--';
  const startTime = new Date(start).getTime();
  const endTime = new Date(end).getTime();
  const diff = Math.abs(endTime - startTime) / (1000 * 60 * 60); // hours
  return `${diff.toFixed(1)}h`;
}

export default function RecentOrdersTable({ orders, onRefresh, loading }: RecentOrdersTableProps) {
  return (
    <div className="space-y-4">
      <div className="flex items-center justify-between">
        <h3 className="text-lg font-medium">Đơn hàng gần đây</h3>
        <GlassButton size="sm" variant="secondary" onClick={onRefresh} disabled={loading}>
          🔄 Làm mới
        </GlassButton>
      </div>
      <DataTable headers={['Mã đơn', 'Khách hàng', 'Tuyến đường', 'Thời gian', 'Hiệu suất', 'Ưu tiên', 'Trạng thái', 'Hành động']}>
        {orders.map((order) => (
          <TableRow key={order.id}>
            <TableCell>
              <div className="font-medium">{order.id}</div>
              <div className="text-gray-600 text-xs">
                {new Date(order.createdAt).toLocaleDateString('vi-VN')}
              </div>
            </TableCell>
            <TableCell>
              <div className="font-medium">{order.customerName}</div>
              <div className="text-gray-600 text-xs">{order.customerPhone}</div>
            </TableCell>
            <TableCell>
              <div className="text-sm">
                {order.pickupAddress} → {order.deliveryAddress}
              </div>
            </TableCell>
            <TableCell>
              {formatDuration(order.createdAt, order.actualDeliveryTime)}
            </TableCell>
            <TableCell>
              <span className={`font-medium ${getEfficiencyColor(calculateEfficiency(order))}`}>
                {calculateEfficiency(order)}%
              </span>
            </TableCell>
            <TableCell>
              <span className={`px-2 py-1 rounded-full text-xs font-medium ${getPriorityColor(order.priority)}`}>
                {order.priority}
              </span>
            </TableCell>
            <TableCell>
              <span className={`font-medium ${getStatusColor(order.status)}`}>
                {getStatusText(order.status)}
              </span>
            </TableCell>
            <TableCell>
              <div className="flex gap-2">
                <GlassButton size="sm" variant="ocean">
                  Chi tiết
                </GlassButton>
                <GlassButton size="sm" variant="green">
                  Báo cáo
                </GlassButton>
              </div>
            </TableCell>
          </TableRow>
        ))}
      </DataTable>
    </div>
  );
}
