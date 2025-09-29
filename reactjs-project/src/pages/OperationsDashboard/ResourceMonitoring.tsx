import React, { useState, useEffect } from 'react';
import { useTranslation } from 'react-i18next';
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
  const { t } = useTranslation();
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

  // Test translation keys on component mount
  useEffect(() => {
    console.log('🧪 Testing translation keys on ResourceMonitoring mount:');
    console.log('fleet.status.available:', t('fleet.status.available'));
    console.log('fleet.status.inUse:', t('fleet.status.inUse'));
    console.log('fleet.status.underMaintenance:', t('fleet.status.underMaintenance'));
    console.log('fleet.notAssigned:', t('fleet.notAssigned'));
  }, [t]);

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
          // FORCE CLEAN statusDisplay - don't use translation keys from backend
          statusDisplay: (() => {
            const backendStatus = vehicle.statusDisplay || vehicle.status || '';
            console.log('🔧 DEBUG - Backend status for vehicle', vehicle.id, ':', backendStatus);
            
            // If backend returns translation key, ignore it and use clean status
            if (String(backendStatus).includes('fleet.status.')) {
              console.log('🔧 CLEANING translation key from backend:', backendStatus);
              return vehicle.status || 'UNKNOWN';
            }
            return String(backendStatus);
          })(),
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
        setError(t('errors.loadingData', 'Unable to load data'));
        setVehicles([]); // Clear vehicles instead of using mock data
      }
      
      setError('');
    } catch (globalError) {
      setError(t('errors.loadingData', 'Unable to load data. Please try again later.'));
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
    console.log('🔍 ResourceMonitoring - getStatusText input:', status, 'type:', typeof status);
    
    if (!status) {
      console.log('❌ ResourceMonitoring - Empty status, returning unknown');
      return t('common.unknown');
    }
    
    // FORCE OVERRIDE for Vietnamese text from backend
    if (status === 'Đang sử dụng') {
      console.log('💥 FORCE OVERRIDE: Đang sử dụng → In Use');
      return 'In Use';
    }
    if (status === 'Sẵn sàng') {
      console.log('💥 FORCE OVERRIDE: Sẵn sàng → Available');
      return 'Available';
    }
    if (status === 'Bảo trì') {
      console.log('💥 FORCE OVERRIDE: Bảo trì → Under Maintenance');
      return 'Under Maintenance';
    }
    
    // Handle cases where backend returns translation keys directly 
    if (typeof status === 'string' && status.includes('fleet.status.')) {
      console.log('🎯 ResourceMonitoring - Found fleet.status translation key:', status);
      
      // Map translation keys to actual translations
      switch (status) {
        case 'fleet.status.available':
          return t('fleet.status.available', 'Available');
        case 'fleet.status.inUse':
          return t('fleet.status.inUse', 'In Use');
        case 'fleet.status.underMaintenance':
          return t('fleet.status.underMaintenance', 'Under Maintenance');
        case 'fleet.status.needMaintenance':
          return t('fleet.status.needMaintenance', 'Need Maintenance');
        default:
          console.log('🎯 Unknown fleet.status key:', status);
          return t('common.unknown', 'Unknown');
      }
    }
    
    // Convert to uppercase for consistency
    const normalizedStatus = typeof status === 'string' ? status.toUpperCase() : status;
    console.log('🔥 SWITCH DEBUG - Original:', status, '→ Normalized:', normalizedStatus);
    
    // Handle English status codes
    switch (normalizedStatus) {
      case 'AVAILABLE': 
      case 'SẴN SÀNG':  // Backend returns this exact format
        return t('fleet.status.available');
      case 'IN_USE': 
      case 'INUSE':
      case 'IN USE':
      case 'ĐANG SỬ DỤNG':  // Backend returns this exact format
        return t('fleet.status.inUse');
      case 'MAINTENANCE': 
      case 'UNDER_MAINTENANCE':
      case 'UNDERMAINTENANCE':
      case 'BẢO TRÌ':  // Backend returns this exact format
      case 'ĐANG BẢO TRÌ': 
        return t('fleet.status.underMaintenance');
      case 'MAINTENANCE_PENDING': 
      case 'MAINTENANCEPENDING':
      case 'NEED_MAINTENANCE':
      case 'CHỜ BẢO TRÌ': 
        return t('fleet.status.needMaintenance');
      default: 
        console.log('ResourceMonitoring - No match for status:', status, 'normalized:', normalizedStatus);
        
        // Last resort: if it looks like a translation key, try to translate it
        if (typeof status === 'string' && (status.includes('status.') || status.includes('fleet.'))) {
          console.log('ResourceMonitoring - Attempting to translate as key:', status);
          try {
            const translated = t(status);
            if (translated !== status) {
              return translated;
            }
          } catch (e) {
            console.log('ResourceMonitoring - Translation failed:', e);
          }
        }
        
        return String(status);
    }
  };


  const getTypeText = (type: Vehicle['type']) => {
    console.log('ResourceMonitoring - getTypeText input:', type);
    
    // Handle cases where backend returns translation keys
    if (typeof type === 'string' && type.startsWith('fleet.vehicleTypes.')) {
      console.log('ResourceMonitoring - Found translation key in type, using t() directly');
      const translatedText = t(type);
      // If translation returns the key itself (translation not found), fall back to mapping
      if (translatedText === type) {
        console.log('ResourceMonitoring - Translation not found for key:', type, 'using fallback mapping');
        // Extract the vehicle type from the key (e.g., 'fleet.vehicleTypes.truck' -> 'truck')
        const vehicleType = type.split('.').pop()?.toUpperCase();
        return getTypeText(vehicleType as Vehicle['type']);
      }
      return translatedText;
    }
    
    switch (type) {
      case 'TRUCK': return t('fleet.vehicleTypes.truck', 'Truck');
      case 'VAN': return t('fleet.vehicleTypes.van', 'Van');
      case 'MOTORCYCLE': return t('fleet.vehicleTypes.motorcycle', 'Motorcycle');
      case 'CAR': return t('fleet.vehicleTypes.car', 'Car');
      default: 
        console.log('ResourceMonitoring - No match for type:', type, 'returning as-is');
        return type || 'Unknown Type';
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
        <div className="text-gray-800 text-lg">{t('common.loading')}...</div>
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
        <h2 className="text-xl font-semibold text-gray-800">{t('dashboard.operations.tabs.monitoring')}</h2>
        <div className="flex gap-2">
          {['24h', '7d', '1m'].map((period) => {
            const periodLabels = {
              '24h': t('common.timeFilters.24h', '24h'),
              '7d': t('common.timeFilters.7d', '7d'), 
              '1m': t('common.timeFilters.1month', '1 month')
            };
            return (
              <GlassButton
                key={period}
                size="sm"
                variant={timeFilter === period ? 'primary' : 'secondary'}
                onClick={() => setTimeFilter(period)}
              >
                {periodLabels[period as keyof typeof periodLabels]}
              </GlassButton>
            );
          })}
          <GlassButton size="sm" variant="secondary" onClick={() => {
            fetchVehicles(timeFilter);
            fetchMaintenanceRequestsCount();
          }}>
🔄 {t('common.refresh')}
          </GlassButton>
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        <StatCard
title={t('dashboard.operations.monitoring.totalVehicles', 'Total Vehicles')}
          value={totalVehicles.toString()}
          icon={<Truck size={24} color="#f59e0b" />}
          trend={{ value: 8.2, isPositive: true }}
        />
        <StatCard
          title={t('common.active', 'Active')}
          value={activeVehicles.toString()}
          icon={<FaRegPlayCircle size={24} color="#10b981" />}
          subtitle={`${vehicleMetrics.percentage}% ${t('common.ofTotal', 'of total')}`}
        />
        <StatCard
          title={t('fleet.status.underMaintenance', 'Under Maintenance')}
          value={maintenanceVehicles.toString()}
          icon={<HiOutlineWrenchScrewdriver size={24} color="#6B7280" />}
          subtitle={`${totalVehicles > 0 ? Math.round((maintenanceVehicles / totalVehicles) * 100) : 0}% ${t('common.ofTotal', 'of total')}`}
        />
        <StatCard
title={t('dashboard.operations.monitoring.maintenanceRequests', 'Maintenance Requests')}
          value={maintenanceRequestsCount.toString()}
          icon={<LiaClipboardListSolid size={24} color="#3B82F6" />}
        />
      </div>

      <div className="space-y-4">
        <h3 className="text-lg font-medium text-gray-800">{t('dashboard.operations.monitoring.resourceDetails', 'Resource Details')}</h3>
        <DataTable headers={[t('dashboard.operations.monitoring.headers.vehicleName', 'Vehicle Name'), t('dashboard.operations.monitoring.headers.type', 'Type'), t('dashboard.operations.monitoring.headers.driver', 'Driver'), t('dashboard.operations.monitoring.headers.status', 'Status'), t('dashboard.operations.monitoring.headers.createdDate', 'Created Date')]}>
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
                  <span className="text-gray-600">
                    {t('fleet.notAssigned', 'Not Assigned')}
                  </span>
                )}
              </TableCell>
              <TableCell>
                <span className={`font-medium ${getStatusColor(vehicle.statusDisplay || vehicle.status)}`}>
                  {(() => {
                    const status = vehicle.statusDisplay || vehicle.status;
                    console.log('🚨 DEBUG - Table rendering raw status:', status, typeof status);
                    console.log('🚨 DEBUG - Vehicle object:', vehicle);
                    const result = getStatusText(status);
                    console.log('🚨 DEBUG - Final translated status:', result);
                    return result;
                  })()}
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
{t('operations.monitoring.pagination.showing', 'Showing {{start}}-{{end}} of {{total}} vehicles', {
                start: startIndex + 1,
                end: Math.min(endIndex, vehicles.length),
                total: vehicles.length
              })}
            </div>
            <div className="flex items-center gap-1">
              <GlassButton
                size="sm"
                variant="secondary"
                onClick={() => setCurrentPage(prev => Math.max(prev - 1, 1))}
                disabled={currentPage === 1}
                className={`px-3 ${currentPage === 1 ? 'opacity-50 cursor-not-allowed' : 'hover:bg-white/30'}`}
              >
← {t('common.previous', 'Previous')}
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
{t('common.next', 'Next')} →
              </GlassButton>
            </div>
          </div>
        )}
      </div>
    </GlassCard>
  );
}
