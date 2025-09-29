import { useState, useEffect } from 'react';
import GlassCard from '../../components/GlassCard';
import StatCard from '../../components/StatCard';
import DataTable, { TableRow, TableCell } from '../../components/DataTable';
import GlassButton from '../../components/GlassButton';
import { operationsAPI } from '../../services/operationsAPI';
import type { Vehicle } from '../../types/dashboard';
import { fetchVehicleStats } from '../../services/VehicleListAPI';
import { OperationsMetricsService } from '../../services/operationsMetricsService';
import { Truck } from 'lucide-react';
import { FaRegPlayCircle } from 'react-icons/fa';
import { LiaClipboardListSolid } from 'react-icons/lia';
import { HiOutlineWrenchScrewdriver } from 'react-icons/hi2';

export default function ResourceMonitoring() {
  const [timeFilter, setTimeFilter] = useState('24h');
  const [vehicles, setVehicles] = useState<Vehicle[]>([]);
  const [vehicleMetrics, setVehicleMetrics] = useState({
    active: 0,
    total: 0,
    percentage: 0,
    ratio: '0/0'
  });
  const [maintenanceRequestsCount, setMaintenanceRequestsCount] = useState(0);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');
  
  // Pagination states
  const [currentPage, setCurrentPage] = useState(1);
  const [itemsPerPage] = useState(5); // Show 5 vehicles per page

  useEffect(() => {
    fetchVehicles(timeFilter);
    fetchMaintenanceRequestsCount();
  }, [timeFilter]);

  // Reset to first page when vehicles data changes
  useEffect(() => {
    setCurrentPage(1);
  }, [vehicles.length]);

  // Thêm function để fetch maintenance requests count
  const fetchMaintenanceRequestsCount = async () => {
    try {
      const result = await operationsAPI.getMaintenanceRequestsCount();
      setMaintenanceRequestsCount(result.count);
      console.log('📊 Maintenance requests count:', result.count);
    } catch (error) {
      console.warn('Unable to get maintenance requests count:', error);
      setMaintenanceRequestsCount(0);
    }
  };

  // Thêm tham số filter thời gian
  const fetchVehicles = async (filter: string = '24h') => {
    try {
      setLoading(true);
      
      // Luôn lấy tổng số xe từ database trước
      let totalVehiclesFromDB = 0;
      try {
        // Nếu backend hỗ trợ filter thời gian, truyền filter vào đây
        const { totalRecords } = await fetchVehicleStats(/* filter */);
        totalVehiclesFromDB = totalRecords;
        console.log('📊 Total vehicles from database:', totalVehiclesFromDB, 'with filter:', filter);
      } catch (error) {
        console.warn('Unable to get total vehicles from database:', error);
      }
      
      // Lấy vehicle metrics từ OperationsMetricsService (giống như trang overview)
      try {
        // Nếu backend hỗ trợ filter thời gian, truyền filter vào đây
        const metrics = await OperationsMetricsService.getActiveVehiclesRatio(/* filter */);
        console.log('📊 Vehicle metrics from OperationsMetricsService:', metrics, 'with filter:', filter);
        // Sử dụng total từ database nếu có, nếu không thì dùng từ metrics
        const correctedMetrics = {
          ...metrics,
          total: totalVehiclesFromDB > 0 ? totalVehiclesFromDB : metrics.total,
          percentage: totalVehiclesFromDB > 0 && metrics.active > 0 
            ? Math.round((metrics.active / totalVehiclesFromDB) * 100) 
            : metrics.percentage,
          ratio: totalVehiclesFromDB > 0 
            ? `${metrics.active}/${totalVehiclesFromDB}` 
            : metrics.ratio
        };
        setVehicleMetrics(correctedMetrics);
        console.log('📊 Corrected vehicle metrics:', correctedMetrics);
      } catch (error) {
        console.warn('Unable to get vehicle metrics from OperationsMetricsService:', error);
        // Fallback: sử dụng tổng số từ database
        setVehicleMetrics({
          active: 0,
          total: totalVehiclesFromDB,
          percentage: 0,
          ratio: `0/${totalVehiclesFromDB}`
        });
      }
      
      // Lấy danh sách xe để hiển thị trong table
      try {
        console.log('📋 Fetching vehicles from operations API...');
        const data = await operationsAPI.getVehicles();
        console.log('📋 Received vehicles data:', data);
        
        // Transform data để đảm bảo compatibility với component  
        const transformedVehicles: Vehicle[] = data.map((vehicle) => ({
          id: String(vehicle.id || ''),
          name: String(vehicle.name || ''),
          type: vehicle.type as 'TRUCK' | 'VAN' | 'MOTORCYCLE',
          status: vehicle.status as 'AVAILABLE' | 'IN_USE' | 'MAINTENANCE' | 'MAINTENANCE_PENDING',
          statusDisplay: String(vehicle.statusDisplay || vehicle.status || ''),
          statusCode: vehicle.statusCode || '',
          statusDescription: vehicle.statusDescription || '',
          created_at: vehicle.created_at || '',
          fuel: Number(vehicle.fuel) || 0,
          location: vehicle.location || { lat: 0, lng: 0, address: 'Not specified' },
          mileage: Number(vehicle.mileage) || 0,
          lastMaintenance: String(vehicle.lastMaintenance || ''),
          nextMaintenance: String(vehicle.nextMaintenance || ''),
          driver: vehicle.driver || undefined
        }));
        
        setVehicles(transformedVehicles);
        console.log('📋 Transformed vehicles:', transformedVehicles);
      } catch (error) {
        console.error('❌ Failed to fetch vehicles from API:', error);
        setError('Unable to load vehicle data from server');
        setVehicles([]); // Clear vehicles instead of using mock data
      }
      
      setError('');
    } catch (globalError) {
      setError('Unable to load vehicle data. Please try again later.');
      console.error('Global error in fetchVehicles:', globalError);
      
      // Clear all data on global error
      setVehicleMetrics({
        active: 0,  
        total: 0,
        percentage: 0,
        ratio: '0/0'
      });
      setVehicles([]);
    } finally {
      setLoading(false);
    }
  };

  const getStatusColor = (status: Vehicle['status'] | string) => {
    // Handle English status codes
    switch (status) {
      case 'AVAILABLE': return 'text-green-600';
      case 'IN_USE': return 'text-blue-600';
      case 'MAINTENANCE': return 'text-yellow-600';
      case 'MAINTENANCE_PENDING': return 'text-orange-600';
    }
    
    // Handle Vietnamese status display names from backend
    switch (status) {
      case 'Sẵn sàng': return 'text-green-600';
      case 'Đang sử dụng': return 'text-blue-600';
      case 'Bảo trì': return 'text-yellow-600';
      case 'Đang bảo trì': return 'text-yellow-600';
      case 'Chờ bảo trì': return 'text-orange-600';
      default: return 'text-gray-800';
    }
  };

  const getStatusText = (status: Vehicle['status'] | string) => {
    // Handle English status codes
    switch (status) {
      case 'AVAILABLE': return 'Available';
      case 'IN_USE': return 'In Use';
      case 'MAINTENANCE': return 'Maintenance';
      case 'MAINTENANCE_PENDING': return 'Maintenance Pending';
    }
    
    // Handle Vietnamese status display names from backend
    switch (status) {
      case 'Sẵn sàng': return 'Available';
      case 'Đang sử dụng': return 'In Use';
      case 'Bảo trì': return 'Maintenance';
      case 'Đang bảo trì': return 'Maintenance';
      case 'Chờ bảo trì': return 'Maintenance Pending';
      default: return status;
    }
  };


  const getTypeText = (type: Vehicle['type']) => {
    switch (type) {
      case 'TRUCK': return 'Truck';
      case 'VAN': return 'Van';
      case 'MOTORCYCLE': return 'Motorcycle';
      default: return type;
    }
  };

  // Calculate stats from vehicleMetrics (sử dụng dữ liệu từ OperationsMetricsService giống như trang overview)
  const totalVehicles = vehicleMetrics.total;
  const activeVehicles = vehicleMetrics.active;
  const maintenanceVehicles = vehicles.filter(v => v.status === 'MAINTENANCE').length;

  // Pagination calculations
  const totalPages = Math.ceil(vehicles.length / itemsPerPage);
  const startIndex = (currentPage - 1) * itemsPerPage;
  const endIndex = startIndex + itemsPerPage;
  
  // Get paginated vehicles
  const getPaginatedVehicles = () => {
    return vehicles.slice(startIndex, endIndex);
  };

  if (loading) {
    return (
      <GlassCard className="flex items-center justify-center h-64">
        <div className="text-gray-800 text-lg">Loading data...</div>
      </GlassCard>
    );
  }

  return (
    <GlassCard className="space-y-6">
      {error && (
        <div className="bg-yellow-500/30 border border-yellow-400/50 text-yellow-800 p-4 rounded-lg">
          ⚠️ {error}
        </div>
      )}
      
      <div className="flex items-center justify-between">
        <h2 className="text-xl font-semibold text-gray-800">Resource Monitoring</h2>
        <div className="flex gap-2">
          {['24h', '7d', '1m'].map((period) => (
            <GlassButton
              key={period}
              size="sm"
              variant={timeFilter === period ? 'primary' : 'secondary'}
              onClick={() => setTimeFilter(period)}
            >
              {period === '1m' ? '1 month' : period}
            </GlassButton>
          ))}
          <GlassButton size="sm" variant="secondary" onClick={() => {
            fetchVehicles(timeFilter);
            fetchMaintenanceRequestsCount();
          }}>
            🔄 Refresh
          </GlassButton>
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        <StatCard
          title="Total Vehicles"
          value={totalVehicles.toString()}
          icon={<Truck size={24} color="#f59e0b" />}
          trend={{ value: 8.2, isPositive: true }}
        />
        <StatCard
          title="Active"
          value={activeVehicles.toString()}
          icon={<FaRegPlayCircle size={24} color="#10b981" />}
          subtitle={`${vehicleMetrics.percentage}% of total`}
        />
        <StatCard
          title="In Maintenance"
          value={maintenanceVehicles.toString()}
          icon={<HiOutlineWrenchScrewdriver size={24} color="#6B7280" />}
          subtitle={`${totalVehicles > 0 ? Math.round((maintenanceVehicles / totalVehicles) * 100) : 0}% of total`}
        />
        <StatCard
          title="Maintenance Requests"
          value={maintenanceRequestsCount.toString()}
          icon={<LiaClipboardListSolid size={24} color="#3B82F6" />}
        />
      </div>

      <div className="space-y-4">
        <h3 className="text-lg font-medium text-gray-800">Resource Details</h3>
        <DataTable headers={['Vehicle Name', 'Type', 'Driver', 'Status', 'Created Date']}>
          {getPaginatedVehicles().map((vehicle) => (
            <TableRow key={vehicle.id}>
              <TableCell>
                <div className="font-medium">{vehicle.name}</div>
                <div className="text-gray-600 text-xs">ID: {vehicle.id}</div>
              </TableCell>
              <TableCell>{getTypeText(vehicle.type)}</TableCell>
              <TableCell>
                {vehicle.driver ? (
                  <div>
                    <div className="font-medium">{vehicle.driver.name}</div>
                    <div className="text-gray-600 text-xs">{vehicle.driver.phone}</div>
                  </div>
                ) : (
                  <span className="text-gray-600">Not assigned</span>
                )}
              </TableCell>
              <TableCell>
                <span className={`font-medium ${getStatusColor(vehicle.statusDisplay || vehicle.status)}`}>
                  {getStatusText(vehicle.statusDisplay || vehicle.status)}
                </span>
              </TableCell>
              <TableCell>
                <span className="text-xs text-gray-700">{vehicle.created_at}</span>
              </TableCell>
            </TableRow>
          ))}
        </DataTable>
        
        {/* Pagination Controls */}
        {totalPages > 1 && (
          <div className="flex items-center justify-between mt-6 pt-4 border-t border-gray-200/30">
            <div className="text-sm text-gray-600 font-medium">
              Showing {startIndex + 1}-{Math.min(endIndex, vehicles.length)} of {vehicles.length} vehicles
            </div>
            <div className="flex items-center gap-1">
              <GlassButton
                size="sm"
                variant="secondary"
                onClick={() => setCurrentPage(prev => Math.max(prev - 1, 1))}
                disabled={currentPage === 1}
                className={`px-3 ${currentPage === 1 ? 'opacity-50 cursor-not-allowed' : 'hover:bg-white/30'}`}
              >
                ← Previous
              </GlassButton>
              
              <div className="flex items-center gap-1 mx-2">
                {Array.from({ length: Math.min(totalPages, 5) }, (_, i) => {
                  let page;
                  if (totalPages <= 5) {
                    page = i + 1;
                  } else if (currentPage <= 3) {
                    page = i + 1;
                  } else if (currentPage >= totalPages - 2) {
                    page = totalPages - 4 + i;
                  } else {
                    page = currentPage - 2 + i;
                  }
                  
                  return (
                    <GlassButton
                      key={page}
                      size="sm"
                      variant={currentPage === page ? 'primary' : 'secondary'}
                      onClick={() => setCurrentPage(page)}
                      className={`min-w-[36px] h-9 ${
                        currentPage === page 
                          ? 'bg-blue-500/80 text-white font-semibold ring-2 ring-blue-400/50' 
                          : 'hover:bg-white/20'
                      }`}
                    >
                      {page}
                    </GlassButton>
                  );
                })}
              </div>
              
              <GlassButton
                size="sm"
                variant="secondary"
                onClick={() => setCurrentPage(prev => Math.min(prev + 1, totalPages))}
                disabled={currentPage === totalPages}
                className={`px-3 ${currentPage === totalPages ? 'opacity-50 cursor-not-allowed' : 'hover:bg-white/30'}`}
              >
                Next →
              </GlassButton>
            </div>
          </div>
        )}
      </div>
    </GlassCard>
  );
}
