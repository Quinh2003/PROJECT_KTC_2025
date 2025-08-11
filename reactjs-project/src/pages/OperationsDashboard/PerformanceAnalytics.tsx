import { useState, useEffect } from 'react';
import GlassCard from '../../components/GlassCard';
import StatCard from '../../components/StatCard';
import DataTable, { TableRow, TableCell } from '../../components/DataTable';
import GlassButton from '../../components/GlassButton';
import { operationsAPI, type Order } from '../../services/operationsAPI';

interface PerformanceMetrics {
  deliverySuccessRate: number;
  avgDeliveryTime: number;
  costPerKm: number;
  customerSatisfaction: number;
  onTimeDeliveryRate: number;
  fuelEfficiency: number;
  target: {
    deliverySuccessRate: number;
    avgDeliveryTime: number;
    costPerKm: number;
    customerSatisfaction: number;
  };
}

export default function PerformanceAnalytics() {
  const [selectedMetric, setSelectedMetric] = useState('delivery');
  const [timeRange, setTimeRange] = useState('7d');
  const [metrics, setMetrics] = useState<PerformanceMetrics | null>(null);
  const [recentOrders, setRecentOrders] = useState<Order[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');

  const fetchPerformanceData = async () => {
    try {
      setLoading(true);
      const [metricsData, ordersData] = await Promise.all([
        operationsAPI.getPerformanceMetrics(timeRange),
        operationsAPI.getOrders({ limit: 10 })
      ]);
      setMetrics(metricsData);
      setRecentOrders(ordersData);
      setError('');
    } catch {
      setError('Không thể tải dữ liệu hiệu suất. Sử dụng dữ liệu mẫu.');
      // Fallback data
      setMetrics({
        deliverySuccessRate: 94.5,
        avgDeliveryTime: 28,
        costPerKm: 12500,
        customerSatisfaction: 4.6,
        onTimeDeliveryRate: 87.3,
        fuelEfficiency: 8.5,
        target: {
          deliverySuccessRate: 95,
          avgDeliveryTime: 30,
          costPerKm: 13000,
          customerSatisfaction: 4.5,
        }
      });
      setRecentOrders([
        { 
          id: 'DH001', 
          customerName: 'Công ty ABC',
          customerPhone: '0912345678',
          pickupAddress: 'Hà Nội', 
          deliveryAddress: 'Hải Phòng', 
          status: 'DELIVERED', 
          priority: 'HIGH',
          estimatedDeliveryTime: '2024-08-08T16:00:00Z',
          actualDeliveryTime: '2024-08-08T15:30:00Z',
          weight: 500,
          value: 2000000,
          createdAt: '2024-08-08T08:00:00Z',
          updatedAt: '2024-08-08T15:30:00Z',
          assignedVehicle: 'VT-001',
          assignedDriver: 'D001'
        },
        { 
          id: 'DH002', 
          customerName: 'Cửa hàng XYZ',
          customerPhone: '0987654321',
          pickupAddress: 'Hà Nội', 
          deliveryAddress: 'Hưng Yên', 
          status: 'IN_TRANSIT', 
          priority: 'MEDIUM',
          estimatedDeliveryTime: '2024-08-08T18:00:00Z',
          weight: 200,
          value: 800000,
          createdAt: '2024-08-08T10:00:00Z',
          updatedAt: '2024-08-08T14:00:00Z',
          assignedVehicle: 'VV-001',
          assignedDriver: 'D002'
        },
        { 
          id: 'DH003', 
          customerName: 'Nhà máy DEF',
          customerPhone: '0123456789',
          pickupAddress: 'Hà Nội', 
          deliveryAddress: 'Quảng Ninh', 
          status: 'DELIVERED', 
          priority: 'LOW',
          estimatedDeliveryTime: '2024-08-08T20:00:00Z',
          actualDeliveryTime: '2024-08-08T19:45:00Z',
          weight: 1000,
          value: 5000000,
          createdAt: '2024-08-08T07:00:00Z',
          updatedAt: '2024-08-08T19:45:00Z',
          assignedVehicle: 'VT-003'
        },
        { 
          id: 'DH004', 
          customerName: 'Siêu thị GHI',
          customerPhone: '0456789123',
          pickupAddress: 'Hà Nội', 
          deliveryAddress: 'Bắc Ninh', 
          status: 'DELIVERED', 
          priority: 'URGENT',
          estimatedDeliveryTime: '2024-08-08T14:00:00Z',
          actualDeliveryTime: '2024-08-08T16:30:00Z',
          weight: 300,
          value: 1500000,
          createdAt: '2024-08-08T09:00:00Z',
          updatedAt: '2024-08-08T16:30:00Z',
          assignedVehicle: 'VT-002'
        },
      ]);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    const fetchData = async () => {
      try {
        setLoading(true);
        const [metricsData, ordersData] = await Promise.all([
          operationsAPI.getPerformanceMetrics(timeRange),
          operationsAPI.getOrders({ limit: 10 })
        ]);
        setMetrics(metricsData);
        setRecentOrders(ordersData);
        setError('');
      } catch {
        setError('Không thể tải dữ liệu hiệu suất. Sử dụng dữ liệu mẫu.');
        // Fallback data
        setMetrics({
          deliverySuccessRate: 94.5,
          avgDeliveryTime: 28,
          costPerKm: 12500,
          customerSatisfaction: 4.6,
          onTimeDeliveryRate: 87.3,
          fuelEfficiency: 8.5,
          target: {
            deliverySuccessRate: 95,
            avgDeliveryTime: 30,
            costPerKm: 13000,
            customerSatisfaction: 4.5,
          }
        });
        setRecentOrders([
          { 
            id: 'DH001', 
            customerName: 'Công ty ABC',
            customerPhone: '0912345678',
            pickupAddress: 'Hà Nội', 
            deliveryAddress: 'Hải Phòng', 
            status: 'DELIVERED', 
            priority: 'HIGH',
            estimatedDeliveryTime: '2024-08-08T16:00:00Z',
            actualDeliveryTime: '2024-08-08T15:30:00Z',
            weight: 500,
            value: 2000000,
            createdAt: '2024-08-08T08:00:00Z',
            updatedAt: '2024-08-08T15:30:00Z',
            assignedVehicle: 'VT-001',
            assignedDriver: 'D001'
          },
          { 
            id: 'DH002', 
            customerName: 'Cửa hàng XYZ',
            customerPhone: '0987654321',
            pickupAddress: 'Hà Nội', 
            deliveryAddress: 'Hưng Yên', 
            status: 'IN_TRANSIT', 
            priority: 'MEDIUM',
            estimatedDeliveryTime: '2024-08-08T18:00:00Z',
            weight: 200,
            value: 800000,
            createdAt: '2024-08-08T10:00:00Z',
            updatedAt: '2024-08-08T14:00:00Z',
            assignedVehicle: 'VV-001',
            assignedDriver: 'D002'
          },
          { 
            id: 'DH003', 
            customerName: 'Nhà máy DEF',
            customerPhone: '0123456789',
            pickupAddress: 'Hà Nội', 
            deliveryAddress: 'Quảng Ninh', 
            status: 'DELIVERED', 
            priority: 'LOW',
            estimatedDeliveryTime: '2024-08-08T20:00:00Z',
            actualDeliveryTime: '2024-08-08T19:45:00Z',
            weight: 1000,
            value: 5000000,
            createdAt: '2024-08-08T07:00:00Z',
            updatedAt: '2024-08-08T19:45:00Z',
            assignedVehicle: 'VT-003'
          },
          { 
            id: 'DH004', 
            customerName: 'Siêu thị GHI',
            customerPhone: '0456789123',
            pickupAddress: 'Hà Nội', 
            deliveryAddress: 'Bắc Ninh', 
            status: 'DELIVERED', 
            priority: 'URGENT',
            estimatedDeliveryTime: '2024-08-08T14:00:00Z',
            actualDeliveryTime: '2024-08-08T16:30:00Z',
            weight: 300,
            value: 1500000,
            createdAt: '2024-08-08T09:00:00Z',
            updatedAt: '2024-08-08T16:30:00Z',
            assignedVehicle: 'VT-002'
          },
        ]);
      } finally {
        setLoading(false);
      }
    };
    fetchData();
  }, [timeRange]);

  const getStatusColor = (status: Order['status']) => {
    switch (status) {
      case 'DELIVERED': return 'text-green-600';
      case 'IN_TRANSIT': return 'text-blue-600';
      case 'PICKED_UP': return 'text-yellow-600';
      case 'ASSIGNED': return 'text-purple-600';
      case 'PENDING': return 'text-orange-600';
      case 'CANCELLED': return 'text-red-600';
      default: return 'text-gray-800';
    }
  };

  const getStatusText = (status: Order['status']) => {
    switch (status) {
      case 'DELIVERED': return 'Đã giao';
      case 'IN_TRANSIT': return 'Đang giao';
      case 'PICKED_UP': return 'Đã lấy hàng';
      case 'ASSIGNED': return 'Đã phân công';
      case 'PENDING': return 'Chờ xử lý';
      case 'CANCELLED': return 'Đã hủy';
      default: return status;
    }
  };

  const getPriorityColor = (priority: Order['priority']) => {
    switch (priority) {
      case 'URGENT': return 'text-red-400 bg-red-500/20';
      case 'HIGH': return 'text-orange-400 bg-orange-500/20';
      case 'MEDIUM': return 'text-yellow-400 bg-yellow-500/20';
      case 'LOW': return 'text-green-400 bg-green-500/20';
      default: return 'text-white bg-white/20';
    }
  };

  const calculateEfficiency = (order: Order) => {
    if (!order.actualDeliveryTime || !order.estimatedDeliveryTime) return 100;
    
    const estimated = new Date(order.estimatedDeliveryTime).getTime();
    const actual = new Date(order.actualDeliveryTime).getTime();
    const diff = (actual - estimated) / (1000 * 60); // minutes
    
    if (diff <= 0) return 100; // Early or on time
    if (diff <= 30) return 95; // Within 30 minutes
    if (diff <= 60) return 85; // Within 1 hour
    return Math.max(50, 100 - Math.floor(diff / 30) * 10); // Deduct 10% per 30min
  };

  const getEfficiencyColor = (efficiency: number) => {
    if (efficiency >= 90) return 'text-green-400';
    if (efficiency >= 75) return 'text-yellow-400';
    return 'text-red-400';
  };

  const formatDuration = (start: string, end?: string) => {
    if (!end) return '--';
    const startTime = new Date(start).getTime();
    const endTime = new Date(end).getTime();
    const diff = Math.abs(endTime - startTime) / (1000 * 60 * 60); // hours
    return `${diff.toFixed(1)}h`;
  };

  if (loading) {
    return (
      <GlassCard className="flex items-center justify-center h-64">
        <div className="text-gray-800 text-lg">Đang tải dữ liệu hiệu suất...</div>
      </GlassCard>
    );
  }

  const performanceData = metrics ? [
    { 
      metric: 'Tỷ lệ giao hàng thành công', 
      current: metrics.deliverySuccessRate, 
      target: metrics.target.deliverySuccessRate, 
      trend: Number((metrics.deliverySuccessRate - metrics.target.deliverySuccessRate).toFixed(1))
    },
    { 
      metric: 'Thời gian giao hàng trung bình', 
      current: metrics.avgDeliveryTime, 
      target: metrics.target.avgDeliveryTime, 
      trend: Number((metrics.target.avgDeliveryTime - metrics.avgDeliveryTime).toFixed(1))
    },
    { 
      metric: 'Chi phí vận chuyển/km', 
      current: metrics.costPerKm, 
      target: metrics.target.costPerKm, 
      trend: Number((((metrics.target.costPerKm - metrics.costPerKm) / metrics.target.costPerKm) * 100).toFixed(1))
    },
    { 
      metric: 'Mức độ hài lòng KH', 
      current: metrics.customerSatisfaction, 
      target: metrics.target.customerSatisfaction, 
      trend: Number((((metrics.customerSatisfaction - metrics.target.customerSatisfaction) / metrics.target.customerSatisfaction) * 100).toFixed(1))
    },
  ] : [];

  return (
    <GlassCard className="space-y-6">
      {error && (
        <div className="bg-yellow-500/30 border border-yellow-400/50 text-yellow-800 p-4 rounded-lg">
          ⚠️ {error}
        </div>
      )}

      <div className="flex items-center justify-between">
        <h2 className="text-xl font-semibold text-gray-800">Phân tích hiệu suất</h2>
        <div className="flex gap-2">
          {[
            { key: 'delivery', label: 'Giao hàng' },
            { key: 'cost', label: 'Chi phí' },
            { key: 'time', label: 'Thời gian' },
            { key: 'quality', label: 'Chất lượng' }
          ].map((metric) => (
            <GlassButton
              key={metric.key}
              size="sm"
              variant={selectedMetric === metric.key ? 'primary' : 'secondary'}
              onClick={() => setSelectedMetric(metric.key)}
            >
              {metric.label}
            </GlassButton>
          ))}
          {['24h', '7d', '30d'].map((range) => (
            <GlassButton
              key={range}
              size="sm"
              variant={timeRange === range ? 'primary' : 'secondary'}
              onClick={() => setTimeRange(range)}
            >
              {range}
            </GlassButton>
          ))}
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        {performanceData.map((data, index) => (
          <StatCard
            key={index}
            title={data.metric}
            value={
              data.metric.includes('Chi phí') 
                ? `${Math.round(data.current).toLocaleString()}đ` 
                : data.metric.includes('thời gian')
                ? `${Math.round(data.current)}min`
                : data.metric.includes('lòng')
                ? `${data.current.toFixed(1)}/5`
                : `${data.current.toFixed(1)}%`
            }
            subtitle={
              data.metric.includes('Chi phí') 
                ? `Mục tiêu: ${Math.round(data.target).toLocaleString()}đ`
                : data.metric.includes('thời gian')
                ? `Mục tiêu: ${Math.round(data.target)}min`
                : data.metric.includes('lòng')
                ? `Mục tiêu: ${data.target.toFixed(1)}/5`
                : `Mục tiêu: ${data.target.toFixed(1)}%`
            }
            trend={{ 
              value: Math.abs(data.trend), 
              isPositive: data.trend > 0 
            }}
            icon={index === 0 ? '📦' : index === 1 ? '⏱️' : index === 2 ? '💰' : '⭐'}
          />
        ))}
      </div>

      <div className="space-y-4">
        <div className="flex items-center justify-between">
          <h3 className="text-lg font-medium">Đơn hàng gần đây</h3>
          <GlassButton size="sm" variant="secondary" onClick={fetchPerformanceData}>
            🔄 Làm mới
          </GlassButton>
        </div>
        <DataTable headers={['Mã đơn', 'Khách hàng', 'Tuyến đường', 'Thời gian', 'Hiệu suất', 'Ưu tiên', 'Trạng thái', 'Hành động']}>
          {recentOrders.map((order) => (
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
    </GlassCard>
  );
}
