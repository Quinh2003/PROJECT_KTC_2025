import { useState, useEffect } from 'react';
import GlassCard from '../../components/GlassCard';
import StatCard from '../../components/StatCard';
import DataTable, { TableRow, TableCell } from '../../components/DataTable';
import GlassButton from '../../components/GlassButton';
import { operationsAPI, type Vehicle } from '../../services/operationsAPI';

export default function ResourceMonitoring() {
  const [timeFilter, setTimeFilter] = useState('24h');
  const [vehicles, setVehicles] = useState<Vehicle[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');

  useEffect(() => {
    fetchVehicles();
  }, []);

  const fetchVehicles = async () => {
    try {
      setLoading(true);
      const data = await operationsAPI.getVehicles();
      setVehicles(data);
      setError('');
    } catch {
      setError('Không thể tải dữ liệu xe. Sử dụng dữ liệu mẫu.');
      // Fallback to mock data if API fails
      setVehicles([
        { 
          id: '1', 
          name: 'Xe tải VT-001', 
          type: 'TRUCK', 
          status: 'ACTIVE', 
          fuel: 85, 
          location: { lat: 21.0285, lng: 105.8542, address: 'Khu vực A - Hà Nội' },
          mileage: 45000,
          lastMaintenance: '2024-07-15',
          nextMaintenance: '2024-10-15',
          driver: { id: 'D001', name: 'Nguyễn Văn A', phone: '0912345678' }
        },
        { 
          id: '2', 
          name: 'Xe tải VT-002', 
          type: 'TRUCK', 
          status: 'MAINTENANCE', 
          fuel: 42, 
          location: { lat: 21.0245, lng: 105.8412, address: 'Garage - Hà Nội' },
          mileage: 52000,
          lastMaintenance: '2024-08-01',
          nextMaintenance: '2024-11-01',
        },
        { 
          id: '3', 
          name: 'Xe van VV-001', 
          type: 'VAN', 
          status: 'ACTIVE', 
          fuel: 73, 
          location: { lat: 21.0195, lng: 105.8385, address: 'Khu vực B - Hà Nội' },
          mileage: 32000,
          lastMaintenance: '2024-06-20',
          nextMaintenance: '2024-09-20',
          driver: { id: 'D002', name: 'Trần Thị B', phone: '0987654321' }
        },
        { 
          id: '4', 
          name: 'Xe tải VT-003', 
          type: 'TRUCK', 
          status: 'IDLE', 
          fuel: 92, 
          location: { lat: 21.0305, lng: 105.8485, address: 'Khu vực C - Hà Nội' },
          mileage: 38000,
          lastMaintenance: '2024-07-05',
          nextMaintenance: '2024-10-05',
        },
      ]);
    } finally {
      setLoading(false);
    }
  };

  const handleStatusChange = async (vehicleId: string, newStatus: Vehicle['status']) => {
    try {
      await operationsAPI.updateVehicleStatus(vehicleId, newStatus);
      await fetchVehicles(); // Refresh data
    } catch {
      setError('Không thể cập nhật trạng thái xe');
    }
  };

  const getStatusColor = (status: Vehicle['status']) => {
    switch (status) {
      case 'ACTIVE': return 'text-green-600';
      case 'MAINTENANCE': return 'text-yellow-600';
      case 'IDLE': return 'text-blue-600';
      case 'OUT_OF_SERVICE': return 'text-red-600';
      default: return 'text-gray-800';
    }
  };

  const getStatusText = (status: Vehicle['status']) => {
    switch (status) {
      case 'ACTIVE': return 'Hoạt động';
      case 'MAINTENANCE': return 'Bảo trì';
      case 'IDLE': return 'Nghỉ';
      case 'OUT_OF_SERVICE': return 'Hỏng hóc';
      default: return status;
    }
  };

  const getFuelColor = (fuel: number) => {
    if (fuel > 70) return 'text-green-400';
    if (fuel > 30) return 'text-yellow-400';
    return 'text-red-400';
  };

  const getTypeText = (type: Vehicle['type']) => {
    switch (type) {
      case 'TRUCK': return 'Xe tải';
      case 'VAN': return 'Xe van';
      case 'MOTORCYCLE': return 'Xe máy';
      default: return type;
    }
  };

  // Calculate stats from vehicles data
  const totalVehicles = vehicles.length;
  const activeVehicles = vehicles.filter(v => v.status === 'ACTIVE').length;
  const maintenanceVehicles = vehicles.filter(v => v.status === 'MAINTENANCE').length;
  const avgFuel = vehicles.length > 0 ? Math.round(vehicles.reduce((sum, v) => sum + v.fuel, 0) / vehicles.length) : 0;

  if (loading) {
    return (
      <GlassCard className="flex items-center justify-center h-64">
        <div className="text-gray-800 text-lg">Đang tải dữ liệu...</div>
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
        <h2 className="text-xl font-semibold text-gray-800">Giám sát tài nguyên</h2>
        <div className="flex gap-2">
          {['1h', '6h', '24h', '7d'].map((period) => (
            <GlassButton
              key={period}
              size="sm"
              variant={timeFilter === period ? 'primary' : 'secondary'}
              onClick={() => setTimeFilter(period)}
            >
              {period}
            </GlassButton>
          ))}
          <GlassButton size="sm" variant="secondary" onClick={fetchVehicles}>
            🔄 Làm mới
          </GlassButton>
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
        <StatCard
          title="Tổng xe"
          value={totalVehicles.toString()}
          icon="🚛"
          trend={{ value: 8.2, isPositive: true }}
        />
        <StatCard
          title="Đang hoạt động"
          value={activeVehicles.toString()}
          icon="✅"
          subtitle={`${Math.round((activeVehicles / totalVehicles) * 100)}% tổng số`}
        />
        <StatCard
          title="Đang bảo trì"
          value={maintenanceVehicles.toString()}
          icon="🔧"
          subtitle={`${Math.round((maintenanceVehicles / totalVehicles) * 100)}% tổng số`}
        />
        <StatCard
          title="Mức nhiên liệu TB"
          value={`${avgFuel}%`}
          icon="⛽"
          trend={{ value: 3.1, isPositive: true }}
        />
      </div>

      <div className="space-y-4">
        <h3 className="text-lg font-medium text-gray-800">Chi tiết tài nguyên</h3>
        <DataTable headers={['Tên xe', 'Loại', 'Tài xế', 'Trạng thái', 'Nhiên liệu', 'Vị trí', 'Hành động']}>
          {vehicles.map((vehicle) => (
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
                  <span className="text-gray-600">Chưa phân công</span>
                )}
              </TableCell>
              <TableCell>
                <span className={`font-medium ${getStatusColor(vehicle.status)}`}>
                  {getStatusText(vehicle.status)}
                </span>
              </TableCell>
              <TableCell>
                <span className={`font-medium ${getFuelColor(vehicle.fuel)}`}>
                  {vehicle.fuel}%
                </span>
              </TableCell>
              <TableCell>
                <div className="text-sm">
                  {vehicle.location.address}
                </div>
              </TableCell>
              <TableCell>
                <div className="flex gap-2">
                  <GlassButton size="sm" variant="ocean">
                    Chi tiết
                  </GlassButton>
                  <GlassButton size="sm" variant="green">
                    Theo dõi
                  </GlassButton>
                  {vehicle.status === 'ACTIVE' && (
                    <GlassButton 
                      size="sm" 
                      variant="danger"
                      onClick={() => handleStatusChange(vehicle.id, 'MAINTENANCE')}
                    >
                      Bảo trì
                    </GlassButton>
                  )}
                </div>
              </TableCell>
            </TableRow>
          ))}
        </DataTable>
      </div>
    </GlassCard>
  );
}
