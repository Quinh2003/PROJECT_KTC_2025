import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useState, useEffect } from 'react';
import GlassCard from '../../components/GlassCard';
import StatCard from '../../components/StatCard';
import DataTable, { TableRow, TableCell } from '../../components/DataTable';
import GlassButton from '../../components/GlassButton';
import { operationsAPI } from '../../services/operationsAPI';
export default function ResourceMonitoring() {
    const [timeFilter, setTimeFilter] = useState('24h');
    const [vehicles, setVehicles] = useState([]);
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
        }
        catch {
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
        }
        finally {
            setLoading(false);
        }
    };
    const handleStatusChange = async (vehicleId, newStatus) => {
        try {
            await operationsAPI.updateVehicleStatus(vehicleId, newStatus);
            await fetchVehicles(); // Refresh data
        }
        catch {
            setError('Không thể cập nhật trạng thái xe');
        }
    };
    const getStatusColor = (status) => {
        switch (status) {
            case 'ACTIVE': return 'text-green-600';
            case 'MAINTENANCE': return 'text-yellow-600';
            case 'IDLE': return 'text-blue-600';
            case 'OUT_OF_SERVICE': return 'text-red-600';
            default: return 'text-gray-800';
        }
    };
    const getStatusText = (status) => {
        switch (status) {
            case 'ACTIVE': return 'Hoạt động';
            case 'MAINTENANCE': return 'Bảo trì';
            case 'IDLE': return 'Nghỉ';
            case 'OUT_OF_SERVICE': return 'Hỏng hóc';
            default: return status;
        }
    };
    const getFuelColor = (fuel) => {
        if (fuel > 70)
            return 'text-green-400';
        if (fuel > 30)
            return 'text-yellow-400';
        return 'text-red-400';
    };
    const getTypeText = (type) => {
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
        return (_jsx(GlassCard, { className: "flex items-center justify-center h-64", children: _jsx("div", { className: "text-gray-800 text-lg", children: "\u0110ang t\u1EA3i d\u1EEF li\u1EC7u..." }) }));
    }
    return (_jsxs(GlassCard, { className: "space-y-6", children: [error && (_jsxs("div", { className: "bg-yellow-500/30 border border-yellow-400/50 text-yellow-800 p-4 rounded-lg", children: ["\u26A0\uFE0F ", error] })), _jsxs("div", { className: "flex items-center justify-between", children: [_jsx("h2", { className: "text-xl font-semibold text-gray-800", children: "Gi\u00E1m s\u00E1t t\u00E0i nguy\u00EAn" }), _jsxs("div", { className: "flex gap-2", children: [['1h', '6h', '24h', '7d'].map((period) => (_jsx(GlassButton, { size: "sm", variant: timeFilter === period ? 'primary' : 'secondary', onClick: () => setTimeFilter(period), children: period }, period))), _jsx(GlassButton, { size: "sm", variant: "secondary", onClick: fetchVehicles, children: "\uD83D\uDD04 L\u00E0m m\u1EDBi" })] })] }), _jsxs("div", { className: "grid grid-cols-1 md:grid-cols-4 gap-4", children: [_jsx(StatCard, { title: "T\u1ED5ng xe", value: totalVehicles.toString(), icon: "\uD83D\uDE9B", trend: { value: 8.2, isPositive: true } }), _jsx(StatCard, { title: "\u0110ang ho\u1EA1t \u0111\u1ED9ng", value: activeVehicles.toString(), icon: "\u2705", subtitle: `${Math.round((activeVehicles / totalVehicles) * 100)}% tổng số` }), _jsx(StatCard, { title: "\u0110ang b\u1EA3o tr\u00EC", value: maintenanceVehicles.toString(), icon: "\uD83D\uDD27", subtitle: `${Math.round((maintenanceVehicles / totalVehicles) * 100)}% tổng số` }), _jsx(StatCard, { title: "M\u1EE9c nhi\u00EAn li\u1EC7u TB", value: `${avgFuel}%`, icon: "\u26FD", trend: { value: 3.1, isPositive: true } })] }), _jsxs("div", { className: "space-y-4", children: [_jsx("h3", { className: "text-lg font-medium text-gray-800", children: "Chi ti\u1EBFt t\u00E0i nguy\u00EAn" }), _jsx(DataTable, { headers: ['Tên xe', 'Loại', 'Tài xế', 'Trạng thái', 'Nhiên liệu', 'Vị trí', 'Hành động'], children: vehicles.map((vehicle) => (_jsxs(TableRow, { children: [_jsxs(TableCell, { children: [_jsx("div", { className: "font-medium", children: vehicle.name }), _jsxs("div", { className: "text-gray-600 text-xs", children: ["ID: ", vehicle.id] })] }), _jsx(TableCell, { children: getTypeText(vehicle.type) }), _jsx(TableCell, { children: vehicle.driver ? (_jsxs("div", { children: [_jsx("div", { className: "font-medium", children: vehicle.driver.name }), _jsx("div", { className: "text-gray-600 text-xs", children: vehicle.driver.phone })] })) : (_jsx("span", { className: "text-gray-600", children: "Ch\u01B0a ph\u00E2n c\u00F4ng" })) }), _jsx(TableCell, { children: _jsx("span", { className: `font-medium ${getStatusColor(vehicle.status)}`, children: getStatusText(vehicle.status) }) }), _jsx(TableCell, { children: _jsxs("span", { className: `font-medium ${getFuelColor(vehicle.fuel)}`, children: [vehicle.fuel, "%"] }) }), _jsx(TableCell, { children: _jsx("div", { className: "text-sm", children: vehicle.location.address }) }), _jsx(TableCell, { children: _jsxs("div", { className: "flex gap-2", children: [_jsx(GlassButton, { size: "sm", variant: "ocean", children: "Chi ti\u1EBFt" }), _jsx(GlassButton, { size: "sm", variant: "green", children: "Theo d\u00F5i" }), vehicle.status === 'ACTIVE' && (_jsx(GlassButton, { size: "sm", variant: "danger", onClick: () => handleStatusChange(vehicle.id, 'MAINTENANCE'), children: "B\u1EA3o tr\u00EC" }))] }) })] }, vehicle.id))) })] })] }));
}
