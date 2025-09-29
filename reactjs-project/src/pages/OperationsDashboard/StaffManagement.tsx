import { useState, useEffect } from 'react';
import GlassCard from '../../components/GlassCard';
import StatCard from '../../components/StatCard';
import DataTable, { TableRow, TableCell } from '../../components/DataTable';
import GlassButton from '../../components/GlassButton';
import { operationsAPI, type Staff } from '../../services/operationsAPI';
import { FaChartLine, FaUmbrellaBeach, FaUsers } from 'react-icons/fa6';
import { MdWorkHistory } from 'react-icons/md';

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
      setError('Unable to load staff data. Using sample data.');
      // Fallback data
      setStaff([
        { 
          id: '1', 
          name: 'Nguyễn Văn A', 
          email: 'nguyenvana@company.com',
          phone: '0912345678',
          role: 'DRIVER',
          status: 'ACTIVE',
          department: 'Transportation', 
          shiftStart: '06:00', 
          shiftEnd: '14:00',
          performanceScore: 92,
          totalDeliveries: 245,
          onTimeDeliveries: 225
        },
        { 
          id: '3', 
          name: 'Lê Văn C', 
          email: 'levanc@company.com',
          phone: '0123456789',
          role: 'DRIVER',
          status: 'ACTIVE',
          department: 'Transportation', 
          shiftStart: '08:00', 
          shiftEnd: '16:00',
          performanceScore: 95,
          totalDeliveries: 312,
          onTimeDeliveries: 298
        },
        { 
          id: '5', 
          name: 'Hoàng Văn E', 
          email: 'hoangvane@company.com',
          phone: '0789123456',
          role: 'DISPATCHER',
          status: 'ACTIVE',
          department: 'Dispatch', 
          shiftStart: '08:00', 
          shiftEnd: '17:00',
          performanceScore: 90,
          totalDeliveries: 0,
          onTimeDeliveries: 0
        },
        { 
          id: '6', 
          name: 'Đặng Văn F', 
          email: 'dangvanf@company.com',
          phone: '0456123789',
          role: 'FLEET',
          status: 'ACTIVE',
          department: 'Maintenance', 
          shiftStart: '07:00', 
          shiftEnd: '15:00',
          performanceScore: 88,
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
        setError('Unable to load staff data. Using sample data.');
        // Fallback data
        setStaff([
          { 
            id: '1', 
            name: 'Nguyễn Văn A', 
            email: 'nguyenvana@company.com',
            phone: '0912345678',
            role: 'DRIVER',
            status: 'ACTIVE',
            department: 'Transportation', 
            shiftStart: '06:00', 
            shiftEnd: '14:00',
            performanceScore: 92,
            totalDeliveries: 245,
            onTimeDeliveries: 225
          },
          { 
            id: '3', 
            name: 'Lê Văn C', 
            email: 'levanc@company.com',
            phone: '0123456789',
            role: 'DRIVER',
            status: 'ACTIVE',
            department: 'Transportation', 
            shiftStart: '08:00', 
            shiftEnd: '16:00',
            performanceScore: 95,
            totalDeliveries: 312,
            onTimeDeliveries: 298
          },
          { 
            id: '5', 
            name: 'Hoàng Văn E', 
            email: 'hoangvane@company.com',
            phone: '0789123456',
            role: 'DISPATCHER',
            status: 'ACTIVE',
            department: 'Dispatch', 
            shiftStart: '08:00', 
            shiftEnd: '17:00',
            performanceScore: 90,
            totalDeliveries: 0,
            onTimeDeliveries: 0
          },
          { 
            id: '6', 
            name: 'Đặng Văn F', 
            email: 'dangvanf@company.com',
            phone: '0456123789',
            role: 'FLEET',
            status: 'ACTIVE',
            department: 'Maintenance', 
            shiftStart: '07:00', 
            shiftEnd: '15:00',
            performanceScore: 88,
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

  const departments = [
    { key: 'all', label: 'All' },
    { key: 'Vận chuyển', label: 'Transportation' },
    { key: 'Điều phối', label: 'Dispatch' },
    { key: 'Bảo trì', label: 'Maintenance' },
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
      case 'ACTIVE': return 'Working';
      case 'ON_LEAVE': return 'On Leave';
      case 'SICK_LEAVE': return 'Sick Leave';
      case 'TERMINATED': return 'Terminated';
      default: return status;
    }
  };

  const getRoleText = (role: Staff['role']) => {
    switch (role) {
      case 'DRIVER': return 'Driver';
      case 'DISPATCHER': return 'Dispatcher';
      case 'FLEET': return 'Fleet Manager';
      default: return role;
    }
  };

  const getDepartmentText = (department: string) => {
    switch (department) {
      case 'Vận chuyển': return 'Transportation';
      case 'Điều phối': return 'Dispatch';
      case 'Bảo trì': return 'Maintenance';
      default: return department;
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
        <div className="text-gray-800 text-lg">Loading staff data...</div>
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
        <h2 className="text-xl font-semibold text-gray-800">Staff Management</h2>
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
            🔄 Refresh
          </GlassButton>
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        <StatCard
          title="Total Staff"
          value={totalStaff.toString()}
          icon={<FaUsers size={24} color="#4B5563" />}
        />
        <StatCard
          title="Working"
          value={activeStaff.toString()}
          icon={<MdWorkHistory size={24} color="#10b981" />}
        />
        <StatCard
          title="On Leave"
          value={onLeaveStaff.toString()}
          icon={<FaUmbrellaBeach size={24} color="#f59e0b" />}
        />
        <StatCard
          title="Avg Performance"
          value={`${avgPerformance}%`}
          icon={<FaChartLine size={24} color="#4f46e5" />}
          trend={{ value: 2.3, isPositive: true }}
        />
      </div>

      <div className="space-y-4">
        <div className="flex items-center justify-between">
          <h3 className="text-lg font-medium">
            Staff List 
            {selectedDepartment !== 'all' && (
              <span className="text-gray-600 text-base ml-2">
                - {departments.find(d => d.key === selectedDepartment)?.label}
              </span>
            )}
          </h3>
        </div>
        
        <DataTable headers={['Name', 'Role', 'Department', 'Status', 'Contact']}>
          {filteredStaff.map((person) => (
            <TableRow key={person.id}>
              <TableCell>
                <div className="font-medium">{person.name}</div>
                <div className="text-gray-600 text-xs">ID: {person.id}</div>
              </TableCell>
              <TableCell>{getRoleText(person.role)}</TableCell>
              <TableCell>{getDepartmentText(person.department)}</TableCell>
              <TableCell>
                <span className={`font-medium ${getStatusColor(person.status)}`}>
                  {getStatusText(person.status)}
                </span>
              </TableCell>
              <TableCell>
                <div className="text-sm">
                  <div>{person.phone}</div>
                  <div className="text-gray-600 text-xs">{person.email}</div>
                </div>
              </TableCell>
            </TableRow>
          ))}
        </DataTable>
      </div>
    </GlassCard>
  );
}
