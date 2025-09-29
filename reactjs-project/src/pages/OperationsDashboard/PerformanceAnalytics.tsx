import { useState, useEffect, useCallback } from 'react';
import { useTranslation } from 'react-i18next';
import GlassCard from '../../components/GlassCard';
import PerformanceStatCards from './PerformanceStatCards';
import RecentOrdersTable from './RecentOrdersTable';
import GlassButton from '../../components/GlassButton';
import { operationsAPI} from '../../services/operationsAPI';
import type { Order } from '../../types/dashboard';

const ITEMS_PER_PAGE = 10;

interface PerformanceMetrics {
  deliverySuccessRate: number;
  avgDeliveryTime: number;
  costPerKm: number;
  totalDistanceKm: number;
  onTimeDeliveryRate: number;
  fuelEfficiency: number;
  target: {
    deliverySuccessRate: number;
    avgDeliveryTime: number;
    costPerKm: number;
  };
}

export default function PerformanceAnalytics() {
  const { t } = useTranslation();
  
  // Function to refresh performance and orders data
  const handleRefresh = async () => {
    setLoading(true);
    setOrdersLoading(true);
    await Promise.all([
      fetchMetricsData(),
      fetchOrdersData(0, convertStatusForAPI(selectedStatus))
    ]);
    setLoading(false);
    setOrdersLoading(false);
    setCurrentPage(0);
  };
  const [metrics, setMetrics] = useState<PerformanceMetrics | null>(null);
  const [recentOrders, setRecentOrders] = useState<Order[]>([]);
  const [loading, setLoading] = useState(true);
  const [ordersLoading, setOrdersLoading] = useState(false);
  const [error, setError] = useState('');

  // Status filter (matching backend status mapping)
  const [selectedStatus, setSelectedStatus] = useState<string>('All');
  const statusOptions = [
    'All',
    'Pending',        // Pending (ID: 1)
    'Processing',     // Processing (ID: 4)
    'Shipping',        // Shipped (ID: 5)
    'Completed',      // Completed (ID: 2)
    'Cancelled',      // Cancelled (ID: 3)
  ];

  const getStatusLabel = (status: string) => {
    const statusMap: Record<string, string> = {
      'All': t('common.all', 'All'),
      'Pending': t('common.pending', 'Pending'),
      'Processing': t('dashboard.operations.status.processing', 'Processing'),
      'Shipping': t('dashboard.operations.status.shipping', 'Shipping'),
      'Completed': t('common.completed', 'Completed'),
      'Cancelled': t('common.cancelled', 'Cancelled')
    };
    return statusMap[status] || status;
  };

  // Server-side pagination states
  const [currentPage, setCurrentPage] = useState(0); // 0-based for API
  const [totalPages, setTotalPages] = useState(0);
  const [totalElements, setTotalElements] = useState(0);

  const fetchMetricsData = useCallback(async () => {
    try {
      setLoading(true);
      
      // Call real API endpoint for performance metrics
      const response = await fetch('http://localhost:8080/api/operations/performance-metrics', {
        headers: {
          'Authorization': `Bearer ${localStorage.getItem('token')}`,
          'Content-Type': 'application/json'
        }
      });
      
      if (!response.ok) {
        throw new Error(`Failed to fetch metrics: ${response.status}`);
      }
      
      const data = await response.json();
      setMetrics(data);
      setError('');
      
      console.log('📊 Performance metrics from API:', data);
      
    } catch (error) {
      console.error('Failed to fetch metrics data:', error);
      
      // Fallback to sample data if API fails
      setMetrics({
        deliverySuccessRate: 94.5,
        avgDeliveryTime: 45, // 45 minutes
        costPerKm: 12500,
        totalDistanceKm: 2500, // 2500 km total transported
        onTimeDeliveryRate: 87.3,
        fuelEfficiency: 8.5,
        target: {
          deliverySuccessRate: 95,
          avgDeliveryTime: 60, // 60 minutes target
          costPerKm: 13000,
        }
      });
      setError('Using sample data due to API connection error.');
    } finally {
      setLoading(false);
    }
  }, []);

  // Function to fetch orders data separately
  const fetchOrdersData = useCallback(async (page: number, status?: string) => {
    try {
      setOrdersLoading(true);
      const ordersResponse = await operationsAPI.getOrdersForOperations(page, ITEMS_PER_PAGE, status);
      
      setRecentOrders(ordersResponse.content);
      setTotalPages(ordersResponse.totalPages);
      setTotalElements(ordersResponse.totalElements);
    } catch (err) {
      setError('Error loading order list');
      console.error('Error fetching orders:', err);
    } finally {
      setOrdersLoading(false);
    }
  }, []);

  // Function to handle page change
  const handlePageChange = useCallback((newPage: number) => {
    setCurrentPage(newPage);
    fetchOrdersData(newPage, convertStatusForAPI(selectedStatus));
  }, [selectedStatus, fetchOrdersData]);

  // Function to convert English status to Vietnamese for API
  const convertStatusForAPI = (status: string) => {
    const statusMap: Record<string, string> = {
      'All': 'Tất cả',
      'Pending': 'Chờ xử lý',
      'Processing': 'Đang xử lý',
      'Shipped': 'Đang giao',
      'Completed': 'Hoàn thành',
      'Cancelled': 'Đã hủy'
    };
    return statusMap[status] || status;
  };

  // Function to handle status filter change
  const handleStatusChange = useCallback((status: string) => {
    setSelectedStatus(status);
    setCurrentPage(0); // Reset to first page when filter changes
    fetchOrdersData(0, convertStatusForAPI(status)); // Fetch with new filter
  }, [fetchOrdersData]);

  useEffect(() => {
    // Fetch metrics data on initial load and when timeRange changes
    fetchMetricsData();
  }, [fetchMetricsData]);

  useEffect(() => {
    // Fetch orders data when selectedStatus changes
    fetchOrdersData(0, convertStatusForAPI(selectedStatus));
    setCurrentPage(0); // Reset page when status changes
  }, [selectedStatus, fetchOrdersData]);

  if (loading) {
    return (
      <GlassCard className="flex items-center justify-center h-64">
        <div className="text-gray-800 text-lg">{t('dashboard.operations.performance.loadingData', 'Loading performance data...')}</div>
      </GlassCard>
    );
  }

  const performanceData = metrics ? [
    { 
      metric: t('dashboard.operations.performance.metrics.deliverySuccessRate', 'Delivery Success Rate'), 
      type: 'percentage' as const,
      current: metrics.deliverySuccessRate, 
      target: metrics.target.deliverySuccessRate, 
      trend: Number((metrics.deliverySuccessRate - metrics.target.deliverySuccessRate).toFixed(1))
    },
    { 
      metric: t('dashboard.operations.performance.metrics.avgDeliveryTime', 'Average Delivery Time'), 
      type: 'time' as const,
      current: metrics.avgDeliveryTime, 
      target: metrics.target.avgDeliveryTime, 
      trend: Number((metrics.target.avgDeliveryTime - metrics.avgDeliveryTime).toFixed(1))
    },
    { 
      metric: t('dashboard.operations.performance.metrics.transportationCost', 'Transportation Cost/km'), 
      type: 'cost' as const,
      current: metrics.costPerKm, 
      target: metrics.target.costPerKm, 
      trend: Number((((metrics.costPerKm - metrics.target.costPerKm) / metrics.target.costPerKm) * 100).toFixed(1))
    },
    { 
      metric: t('dashboard.operations.performance.metrics.totalKmTransported', 'Total km Transported'), 
      type: 'distance' as const,
      current: metrics.totalDistanceKm, 
      target: 0, // No target for total distance as it's cumulative
      trend: 0 // No trend for total distance as it's cumulative
    },
  ] : [];

  return (
  <GlassCard className="space-y-6">
      {error && (
        <div className="bg-yellow-500/30 border border-yellow-400/50 text-yellow-800 p-4 rounded-lg">
          ⚠️ {error}
        </div>
      )}


      <div className="flex flex-col sm:flex-row sm:items-center justify-between gap-4">
        <h2 className="text-xl font-semibold text-gray-800">{t('dashboard.operations.tabs.performance')}</h2>
        <div className="flex flex-col sm:flex-row items-start sm:items-center gap-3 sm:gap-4">
          <div className="flex flex-wrap gap-2">
            {statusOptions.map((status) => (
              <GlassButton
                key={status}
                size="sm"
                variant={selectedStatus === status ? 'primary' : 'secondary'}
                onClick={() => handleStatusChange(status)}
              >
{getStatusLabel(status)}
              </GlassButton>
            ))}
          </div>
          <GlassButton onClick={handleRefresh} size="sm" variant="primary" className="whitespace-nowrap self-start sm:self-auto" disabled={loading || ordersLoading}>
            <span className="flex items-center gap-2">
              <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="w-5 h-5"><path d="M4.93 19.07a10 10 0 1 0 0-14.14M4 4v5h5"/></svg>
{t('common.refresh')}
            </span>
          </GlassButton>
        </div>
      </div>

      <PerformanceStatCards performanceData={performanceData} />

  {/* Bảng đơn hàng - không cần filter nữa vì đã filter ở backend */}
  <RecentOrdersTable
    orders={recentOrders}
  />
  
  {/* Pagination Controls - Glass Morphism Style */}
  {totalPages > 1 && (
    <div className="flex items-center justify-between mt-6 pt-4 border-t border-gray-200/30">
      <div className="text-sm text-gray-600 font-medium">
        {t('operations.pagination.showing', 'Showing {{start}}-{{end}} of {{total}} orders', {
          start: currentPage * ITEMS_PER_PAGE + 1,
          end: Math.min((currentPage + 1) * ITEMS_PER_PAGE, totalElements),
          total: totalElements
        })}
      </div>
      <div className="flex items-center gap-1">
        <GlassButton
          size="sm"
          variant="secondary"
          onClick={() => handlePageChange(Math.max(currentPage - 1, 0))}
          disabled={currentPage === 0}
          className={`px-3 ${currentPage === 0 ? 'opacity-50 cursor-not-allowed' : 'hover:bg-white/30'}`}
        >
          ← {t('operations.pagination.previous', 'Previous')}
        </GlassButton>
        
        <div className="flex items-center gap-1 mx-2">
          {Array.from({ length: Math.min(totalPages, 5) }, (_, i) => {
            let pageNumber;
            if (totalPages <= 5) {
              pageNumber = i;
            } else if (currentPage <= 2) {
              pageNumber = i;
            } else if (currentPage >= totalPages - 3) {
              pageNumber = totalPages - 5 + i;
            } else {
              pageNumber = currentPage - 2 + i;
            }
            
            return (
              <GlassButton
                key={pageNumber}
                size="sm"
                variant={currentPage === pageNumber ? 'primary' : 'secondary'}
                onClick={() => handlePageChange(pageNumber)}
                className={`min-w-[36px] h-9 ${
                  currentPage === pageNumber 
                    ? 'bg-blue-500/80 text-white font-semibold ring-2 ring-blue-400/50' 
                    : 'hover:bg-white/20'
                }`}
              >
                {pageNumber + 1}
              </GlassButton>
            );
          })}
        </div>
        
        <GlassButton
          size="sm"
          variant="secondary"
          onClick={() => handlePageChange(Math.min(currentPage + 1, totalPages - 1))}
          disabled={currentPage === totalPages - 1}
          className={`px-3 ${currentPage === totalPages - 1 ? 'opacity-50 cursor-not-allowed' : 'hover:bg-white/30'}`}
        >
          {t('operations.pagination.next', 'Next')} →
        </GlassButton>
      </div>
    </div>
  )}
    </GlassCard>
  );
}
