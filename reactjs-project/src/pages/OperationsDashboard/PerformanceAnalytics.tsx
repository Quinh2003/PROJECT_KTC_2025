import { useState, useEffect } from 'react';
import GlassCard from '../../components/GlassCard';
import PerformanceStatCards from './PerformanceStatCards';
import RecentOrdersTable from './RecentOrdersTable';
import GlassButton from '../../components/GlassButton';
import { operationsAPI} from '../../services/operationsAPI';
import type { Order } from '../../types/dashboard';

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

  <PerformanceStatCards performanceData={performanceData} />

  <RecentOrdersTable orders={recentOrders} onRefresh={fetchPerformanceData} loading={loading} />
    </GlassCard>
  );
}
