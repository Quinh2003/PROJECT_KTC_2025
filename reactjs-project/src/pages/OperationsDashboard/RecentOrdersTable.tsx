import { useTranslation } from 'react-i18next';
import DataTable, { TableRow, TableCell } from '../../components/DataTable';
import type { Order } from '../../types/dashboard';

interface RecentOrdersTableProps {
  orders: Order[];
}

// Trả về class màu cho status tiếng Việt từ backend
function getStatusColor(status: string) {
  switch (status) {
    case 'Hoàn thành':
      return 'text-green-600';
    case 'Đã giao':
      return 'text-green-700';
    case 'Đang giao':
      return 'text-blue-600';
    case 'Chờ xử lý':
      return 'text-orange-500';
    case 'Đang xử lý':
      return 'text-purple-500';
    case 'Đã hủy':
      return 'text-red-600';
    case 'Chưa xác định':
      return 'text-gray-400';
    default:
      return 'text-gray-800';
  }
}

function getStatusText(status: string) {
  // Convert Vietnamese status from backend to English for display
  switch (status) {
    case 'Hoàn thành': return 'Completed';
    case 'Đã giao': return 'Delivered';
    case 'Đang giao': return 'Shipping';
    case 'Chờ xử lý': return 'Pending';
    case 'Đang xử lý': return 'Processing';
    case 'Đã hủy': return 'Cancelled';
    case 'Chưa xác định': return 'Unknown';
    default: return status;
  }
}

function formatDateTime(dateString: string) {
  if (!dateString) {
    return {
      date: '--',
      time: '--'
    };
  }
  const date = new Date(dateString);
  return {
    date: date.toLocaleDateString('en-US', {
      day: '2-digit',
      month: '2-digit',
      year: 'numeric'
    }),
    time: date.toLocaleTimeString('en-US', {
      hour: '2-digit',
      minute: '2-digit',
      hour12: false
    })
  };
}

export default function RecentOrdersTable({ orders }: RecentOrdersTableProps) {
  const { t } = useTranslation();
  return (
    <div className="space-y-4">
      <div className="flex items-center">
        <h3 className="text-lg font-medium">{t('dashboard.operations.performance.recentOrders', 'Recent Orders')}</h3>
      </div>
      <div className="overflow-x-auto">
        <DataTable headers={[
          t('dashboard.operations.performance.headers.orderId', 'Order ID'),
          t('dashboard.operations.performance.headers.customer', 'Customer'),
          t('dashboard.operations.performance.headers.route', 'Route'),
          t('dashboard.operations.performance.headers.createdTime', 'Created Time'),
          t('dashboard.operations.performance.headers.status', 'Status')
        ]} className="min-w-full">
          {orders.map((order) => {
            const createdTime = formatDateTime(order.createdAt);
            
            return (
              <TableRow key={order.id}>
                <TableCell>
                  <div className="font-medium">{order.id}</div>
                  <div className="text-gray-600 text-xs">
                    {order.orderCode || `DH${order.id}`}
                  </div>
                </TableCell>
                <TableCell>
                  <div className="font-medium truncate max-w-32 sm:max-w-none">{order.customerName}</div>
                  <div className="text-gray-600 text-xs">{order.customerPhone}</div>
                </TableCell>
                <TableCell>
                    <div className="text-sm max-w-32 sm:max-w-64 break-words">
                      {order.pickupAddress} → {order.deliveryAddress}
                    </div>
                </TableCell>
                <TableCell>
                  <div className="text-sm">
                    <div className="font-medium text-blue-900">{createdTime.date}</div>
                    <div className="text-blue-600 text-xs">{createdTime.time}</div>
                  </div>
                </TableCell>
                <TableCell>
                  <span className={`font-medium ${getStatusColor(order.status)}`}>
                    {getStatusText(order.status)}
                  </span>
                </TableCell>
              </TableRow>
            );
          })}
        </DataTable>
      </div>
    </div>
  );
}
