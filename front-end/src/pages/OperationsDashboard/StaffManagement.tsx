import { useState, useEffect } from 'react';
import GlassCard from '../../components/GlassCard';
import StatCard from '../../components/StatCard';
import DataTable, { TableRow, TableCell } from '../../components/DataTable';
import GlassButton from '../../components/GlassButton';
import { operationsAPI, type Staff } from '../../services/operationsAPI';

export default function StaffManagement() {
  const [selectedDepartment, setSelectedDepartment] = useState('all');
  const [staff, setStaff] = useState<Staff[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState('');

  const fetchStaff = async () => {
    try {
      setLoading(true);
      const department = selectedDepartment === 'all' ? undefined : selectedDepartment;
      const data = await operationsAPI.getStaff(department);
      setStaff(data);
      setError('');
    } catch {
      setError('Không thể tải dữ liệu nhân viên. Sử dụng dữ liệu mẫu.');
      // Fallback data
      setStaff([
        { 
          id: '1', 
          name: 'Nguyễn Văn A', 
          email: 'nguyenvana@company.com',
          phone: '0912345678',
          role: 'DRIVER',
          status: 'ACTIVE',
          department: 'Vận chuyển', 
          shiftStart: '06:00', 
          shiftEnd: '14:00',
          performanceScore: 92,
          totalDeliveries: 245,
          onTimeDeliveries: 225
        },
        { 
          id: '2', 
          name: 'Trần Thị B', 
          email: 'tranthib@company.com',
          phone: '0987654321',
          role: 'WAREHOUSE_STAFF',
          status: 'ON_LEAVE',
          department: 'Kho', 
          shiftStart: '08:00', 
          shiftEnd: '16:00',
          performanceScore: 88,
          totalDeliveries: 0,
          onTimeDeliveries: 0
        },
        { 
          id: '3', 
          name: 'Lê Văn C', 
          email: 'levanc@company.com',
          phone: '0123456789',
          role: 'DRIVER',
          status: 'ACTIVE',
          department: 'Vận chuyển', 
          shiftStart: '08:00', 
          shiftEnd: '16:00',
          performanceScore: 95,
          totalDeliveries: 312,
          onTimeDeliveries: 298
        },
        { 
          id: '4', 
          name: 'Phạm Thị D', 
          email: 'phamthid@company.com',
          phone: '0456789123',
          role: 'WAREHOUSE_STAFF',
          status: 'ACTIVE',
          department: 'Kho', 
          shiftStart: '07:00', 
          shiftEnd: '15:00',
          performanceScore: 82,
          totalDeliveries: 0,
          onTimeDeliveries: 0
        },
        { 
          id: '5', 
          name: 'Hoàng Văn E', 
          email: 'hoangvane@company.com',
          phone: '0789123456',
          role: 'DISPATCHER',
          status: 'ACTIVE',
          department: 'Điều phối', 
          shiftStart: '08:00', 
          shiftEnd: '17:00',
          performanceScore: 90,
          totalDeliveries: 0,
          onTimeDeliveries: 0
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
        const department = selectedDepartment === 'all' ? undefined : selectedDepartment;
        const data = await operationsAPI.getStaff(department);
        setStaff(data);
        setError('');
      } catch {
        setError('Không thể tải dữ liệu nhân viên. Sử dụng dữ liệu mẫu.');
        // Fallback data
        setStaff([
          { 
            id: '1', 
            name: 'Nguyễn Văn A', 
            email: 'nguyenvana@company.com',
            phone: '0912345678',
            role: 'DRIVER',
            status: 'ACTIVE',
            department: 'Vận chuyển', 
            shiftStart: '06:00', 
            shiftEnd: '14:00',
            performanceScore: 92,
            totalDeliveries: 245,
            onTimeDeliveries: 225
          },
          { 
            id: '2', 
            name: 'Trần Thị B', 
            email: 'tranthib@company.com',
            phone: '0987654321',
            role: 'WAREHOUSE_STAFF',
            status: 'ON_LEAVE',
            department: 'Kho', 
            shiftStart: '08:00', 
            shiftEnd: '16:00',
            performanceScore: 88,
            totalDeliveries: 0,
            onTimeDeliveries: 0
          },
          { 
            id: '3', 
            name: 'Lê Văn C', 
            email: 'levanc@company.com',
            phone: '0123456789',
            role: 'DRIVER',
            status: 'ACTIVE',
            department: 'Vận chuyển', 
            shiftStart: '08:00', 
            shiftEnd: '16:00',
            performanceScore: 95,
            totalDeliveries: 312,
            onTimeDeliveries: 298
          },
          { 
            id: '4', 
            name: 'Phạm Thị D', 
            email: 'phamthid@company.com',
            phone: '0456789123',
            role: 'WAREHOUSE_STAFF',
            status: 'ACTIVE',
            department: 'Kho', 
            shiftStart: '07:00', 
            shiftEnd: '15:00',
            performanceScore: 82,
            totalDeliveries: 0,
            onTimeDeliveries: 0
          },
          { 
            id: '5', 
            name: 'Hoàng Văn E', 
            email: 'hoangvane@company.com',
            phone: '0789123456',
            role: 'DISPATCHER',
            status: 'ACTIVE',
            department: 'Điều phối', 
            shiftStart: '08:00', 
            shiftEnd: '17:00',
            performanceScore: 90,
            totalDeliveries: 0,
            onTimeDeliveries: 0
          },
        ]);
      } finally {
        setLoading(false);
      }
    };
    fetchData();
  }, [selectedDepartment]);

  const handleStatusUpdate = async (staffId: string, newStatus: Staff['status']) => {
    try {
      await operationsAPI.updateStaffStatus(staffId, newStatus);
      await fetchStaff(); // Refresh data
    } catch {
      setError('Không thể cập nhật trạng thái nhân viên');
    }
  };

  const departments = [
    { key: 'all', label: 'Tất cả' },
    { key: 'Vận chuyển', label: 'Vận chuyển' },
    { key: 'Kho', label: 'Kho' },
    { key: 'Điều phối', label: 'Điều phối' },
    { key: 'Bảo trì', label: 'Bảo trì' },
  ];

  const getStatusColor = (status: Staff['status']) => {
    switch (status) {
      case 'ACTIVE': return 'text-green-400';
      case 'ON_LEAVE': return 'text-yellow-400';
      case 'SICK_LEAVE': return 'text-orange-400';
      case 'TERMINATED': return 'text-red-400';
      default: return 'text-white';
    }
  };

  const getStatusText = (status: Staff['status']) => {
    switch (status) {
      case 'ACTIVE': return 'Đang làm việc';
      case 'ON_LEAVE': return 'Nghỉ phép';
      case 'SICK_LEAVE': return 'Nghỉ ốm';
      case 'TERMINATED': return 'Nghỉ việc';
      default: return status;
    }
  };

  const getRoleText = (role: Staff['role']) => {
    switch (role) {
      case 'DRIVER': return 'Tài xế';
      case 'DISPATCHER': return 'Điều phối viên';
      case 'WAREHOUSE_STAFF': return 'Nhân viên kho';
      case 'MAINTENANCE': return 'Nhân viên bảo trì';
      default: return role;
    }
  };

  const getPerformanceColor = (performance: number) => {
    if (performance >= 90) return 'text-green-400';
    if (performance >= 75) return 'text-yellow-400';
    return 'text-red-400';
  };

  const filteredStaff = selectedDepartment === 'all' 
    ? staff 
    : staff.filter(s => s.department === selectedDepartment);

  // Calculate stats
  const totalStaff = staff.length;
  const activeStaff = staff.filter(s => s.status === 'ACTIVE').length;
  const onLeaveStaff = staff.filter(s => s.status === 'ON_LEAVE' || s.status === 'SICK_LEAVE').length;
  const avgPerformance = staff.length > 0 ? Math.round(staff.reduce((sum, s) => sum + s.performanceScore, 0) / staff.length) : 0;

  if (loading) {
    return (
      <GlassCard className="flex items-center justify-center h-64">
        <div className="text-gray-800 text-lg">Đang tải dữ liệu nhân viên...</div>
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
        <h2 className="text-xl font-semibold text-gray-800">Quản lý nhân viên</h2>
        <div className="flex gap-2">
          {departments.map((dept) => (
            <GlassButton
              key={dept.key}
              size="sm"
              variant={selectedDepartment === dept.key ? 'primary' : 'secondary'}
              onClick={() => setSelectedDepartment(dept.key)}
            >
              {dept.label}
            </GlassButton>
          ))}
          <GlassButton size="sm" variant="secondary" onClick={fetchStaff}>
            🔄 Làm mới
          </GlassButton>
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <StatCard
          title="Tổng nhân viên"
          value={totalStaff.toString()}
          icon="👥"
          trend={{ value: 5.2, isPositive: true }}
        />
        <StatCard
          title="Đang làm việc"
          value={activeStaff.toString()}
          icon="✅"
          subtitle={`${Math.round((activeStaff / totalStaff) * 100)}% tổng số`}
        />
        <StatCard
          title="Nghỉ phép"
          value={onLeaveStaff.toString()}
          icon="🏖️"
          subtitle={`${Math.round((onLeaveStaff / totalStaff) * 100)}% tổng số`}
        />
        <StatCard
          title="Hiệu suất TB"
          value={`${avgPerformance}%`}
          icon="📊"
          trend={{ value: 2.3, isPositive: true }}
        />
      </div>

      <div className="space-y-4">
        <div className="flex items-center justify-between">
          <h3 className="text-lg font-medium">
            Danh sách nhân viên 
            {selectedDepartment !== 'all' && (
              <span className="text-gray-600 text-base ml-2">
                - {departments.find(d => d.key === selectedDepartment)?.label}
              </span>
            )}
          </h3>
          <GlassButton variant="primary" size="sm">
            + Thêm nhân viên
          </GlassButton>
        </div>
        
        <DataTable headers={['Tên', 'Chức vụ', 'Phòng ban', 'Trạng thái', 'Hiệu suất', 'Ca làm việc', 'Liên hệ', 'Hành động']}>
          {filteredStaff.map((person) => (
            <TableRow key={person.id}>
              <TableCell>
                <div className="font-medium">{person.name}</div>
                <div className="text-gray-600 text-xs">ID: {person.id}</div>
              </TableCell>
              <TableCell>{getRoleText(person.role)}</TableCell>
              <TableCell>{person.department}</TableCell>
              <TableCell>
                <span className={`font-medium ${getStatusColor(person.status)}`}>
                  {getStatusText(person.status)}
                </span>
              </TableCell>
              <TableCell>
                <span className={`font-medium ${getPerformanceColor(person.performanceScore)}`}>
                  {person.performanceScore}%
                </span>
                {person.role === 'DRIVER' && (
                  <div className="text-gray-600 text-xs">
                    {person.onTimeDeliveries}/{person.totalDeliveries} đúng hạn
                  </div>
                )}
              </TableCell>
              <TableCell>
                <div className="text-sm">
                  {person.shiftStart} - {person.shiftEnd}
                </div>
              </TableCell>
              <TableCell>
                <div className="text-sm">
                  <div>{person.phone}</div>
                  <div className="text-gray-600 text-xs">{person.email}</div>
                </div>
              </TableCell>
              <TableCell>
                <div className="flex gap-2">
                  <GlassButton size="sm" variant="ocean">
                    Hồ sơ
                  </GlassButton>
                  <GlassButton size="sm" variant="green">
                    Chỉnh sửa
                  </GlassButton>
                  {person.status === 'ACTIVE' && (
                    <GlassButton 
                      size="sm" 
                      variant="danger"
                      onClick={() => handleStatusUpdate(person.id, 'ON_LEAVE')}
                    >
                      Nghỉ phép
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
