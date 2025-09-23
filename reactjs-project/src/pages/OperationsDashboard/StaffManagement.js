import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useState, useEffect } from 'react';
import GlassCard from '../../components/GlassCard';
import StatCard from '../../components/StatCard';
import DataTable, { TableRow, TableCell } from '../../components/DataTable';
import GlassButton from '../../components/GlassButton';
import { operationsAPI } from '../../services/operationsAPI';
export default function StaffManagement() {
    const [selectedDepartment, setSelectedDepartment] = useState('all');
    const [staff, setStaff] = useState([]);
    const [loading, setLoading] = useState(true);
    const [error, setError] = useState('');
    const fetchStaff = async () => {
        try {
            setLoading(true);
            const department = selectedDepartment === 'all' ? undefined : selectedDepartment;
            const data = await operationsAPI.getStaff(department);
            setStaff(data);
            setError('');
        }
        catch {
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
        }
        finally {
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
            }
            catch {
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
            }
            finally {
                setLoading(false);
            }
        };
        fetchData();
    }, [selectedDepartment]);
    const handleStatusUpdate = async (staffId, newStatus) => {
        try {
            await operationsAPI.updateStaffStatus(staffId, newStatus);
            await fetchStaff(); // Refresh data
        }
        catch {
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
    const getStatusColor = (status) => {
        switch (status) {
            case 'ACTIVE': return 'text-green-400';
            case 'ON_LEAVE': return 'text-yellow-400';
            case 'SICK_LEAVE': return 'text-orange-400';
            case 'TERMINATED': return 'text-red-400';
            default: return 'text-white';
        }
    };
    const getStatusText = (status) => {
        switch (status) {
            case 'ACTIVE': return 'Đang làm việc';
            case 'ON_LEAVE': return 'Nghỉ phép';
            case 'SICK_LEAVE': return 'Nghỉ ốm';
            case 'TERMINATED': return 'Nghỉ việc';
            default: return status;
        }
    };
    const getRoleText = (role) => {
        switch (role) {
            case 'DRIVER': return 'Tài xế';
            case 'DISPATCHER': return 'Điều phối viên';
            case 'WAREHOUSE_STAFF': return 'Nhân viên kho';
            case 'MAINTENANCE': return 'Nhân viên bảo trì';
            default: return role;
        }
    };
    const getPerformanceColor = (performance) => {
        if (performance >= 90)
            return 'text-green-400';
        if (performance >= 75)
            return 'text-yellow-400';
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
        return (_jsx(GlassCard, { className: "flex items-center justify-center h-64", children: _jsx("div", { className: "text-gray-800 text-lg", children: "\u0110ang t\u1EA3i d\u1EEF li\u1EC7u nh\u00E2n vi\u00EAn..." }) }));
    }
    return (_jsxs(GlassCard, { className: "space-y-6", children: [error && (_jsxs("div", { className: "bg-yellow-500/30 border border-yellow-400/50 text-yellow-800 p-4 rounded-lg", children: ["\u26A0\uFE0F ", error] })), _jsxs("div", { className: "flex items-center justify-between", children: [_jsx("h2", { className: "text-xl font-semibold text-gray-800", children: "Qu\u1EA3n l\u00FD nh\u00E2n vi\u00EAn" }), _jsxs("div", { className: "flex gap-2", children: [departments.map((dept) => (_jsx(GlassButton, { size: "sm", variant: selectedDepartment === dept.key ? 'primary' : 'secondary', onClick: () => setSelectedDepartment(dept.key), children: dept.label }, dept.key))), _jsx(GlassButton, { size: "sm", variant: "secondary", onClick: fetchStaff, children: "\uD83D\uDD04 L\u00E0m m\u1EDBi" })] })] }), _jsxs("div", { className: "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4", children: [_jsx(StatCard, { title: "T\u1ED5ng nh\u00E2n vi\u00EAn", value: totalStaff.toString(), icon: "\uD83D\uDC65", trend: { value: 5.2, isPositive: true } }), _jsx(StatCard, { title: "\u0110ang l\u00E0m vi\u1EC7c", value: activeStaff.toString(), icon: "\u2705", subtitle: `${Math.round((activeStaff / totalStaff) * 100)}% tổng số` }), _jsx(StatCard, { title: "Ngh\u1EC9 ph\u00E9p", value: onLeaveStaff.toString(), icon: "\uD83C\uDFD6\uFE0F", subtitle: `${Math.round((onLeaveStaff / totalStaff) * 100)}% tổng số` }), _jsx(StatCard, { title: "Hi\u1EC7u su\u1EA5t TB", value: `${avgPerformance}%`, icon: "\uD83D\uDCCA", trend: { value: 2.3, isPositive: true } })] }), _jsxs("div", { className: "space-y-4", children: [_jsxs("div", { className: "flex items-center justify-between", children: [_jsxs("h3", { className: "text-lg font-medium", children: ["Danh s\u00E1ch nh\u00E2n vi\u00EAn", selectedDepartment !== 'all' && (_jsxs("span", { className: "text-gray-600 text-base ml-2", children: ["- ", departments.find(d => d.key === selectedDepartment)?.label] }))] }), _jsx(GlassButton, { variant: "primary", size: "sm", children: "+ Th\u00EAm nh\u00E2n vi\u00EAn" })] }), _jsx(DataTable, { headers: ['Tên', 'Chức vụ', 'Phòng ban', 'Trạng thái', 'Hiệu suất', 'Ca làm việc', 'Liên hệ', 'Hành động'], children: filteredStaff.map((person) => (_jsxs(TableRow, { children: [_jsxs(TableCell, { children: [_jsx("div", { className: "font-medium", children: person.name }), _jsxs("div", { className: "text-gray-600 text-xs", children: ["ID: ", person.id] })] }), _jsx(TableCell, { children: getRoleText(person.role) }), _jsx(TableCell, { children: person.department }), _jsx(TableCell, { children: _jsx("span", { className: `font-medium ${getStatusColor(person.status)}`, children: getStatusText(person.status) }) }), _jsxs(TableCell, { children: [_jsxs("span", { className: `font-medium ${getPerformanceColor(person.performanceScore)}`, children: [person.performanceScore, "%"] }), person.role === 'DRIVER' && (_jsxs("div", { className: "text-gray-600 text-xs", children: [person.onTimeDeliveries, "/", person.totalDeliveries, " \u0111\u00FAng h\u1EA1n"] }))] }), _jsx(TableCell, { children: _jsxs("div", { className: "text-sm", children: [person.shiftStart, " - ", person.shiftEnd] }) }), _jsx(TableCell, { children: _jsxs("div", { className: "text-sm", children: [_jsx("div", { children: person.phone }), _jsx("div", { className: "text-gray-600 text-xs", children: person.email })] }) }), _jsx(TableCell, { children: _jsxs("div", { className: "flex gap-2", children: [_jsx(GlassButton, { size: "sm", variant: "ocean", children: "H\u1ED3 s\u01A1" }), _jsx(GlassButton, { size: "sm", variant: "green", children: "Ch\u1EC9nh s\u1EEDa" }), person.status === 'ACTIVE' && (_jsx(GlassButton, { size: "sm", variant: "danger", onClick: () => handleStatusUpdate(person.id, 'ON_LEAVE'), children: "Ngh\u1EC9 ph\u00E9p" }))] }) })] }, person.id))) })] })] }));
}
