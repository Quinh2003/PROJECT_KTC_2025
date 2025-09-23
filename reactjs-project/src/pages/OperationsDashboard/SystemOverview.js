import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import GlassCard from '../../components/GlassCard';
import StatCard from '../../components/StatCard';
import GlassButton from '../../components/GlassButton';
export default function SystemOverview() {
    const systemStats = [
        { title: 'Uptime hệ thống', value: '99.8%', icon: '🟢', trend: { value: 0.2, isPositive: true } },
        { title: 'Tải CPU', value: '23%', icon: '💻', subtitle: 'Bình thường' },
        { title: 'Bộ nhớ sử dụng', value: '67%', icon: '🧠', subtitle: '8.2GB/12GB' },
        { title: 'Kết nối DB', value: '145ms', icon: '🗄️', trend: { value: 12, isPositive: false } },
    ];
    const alerts = [
        {
            id: 1,
            level: 'warning',
            message: 'Xe tải VT-003 cần bảo trì định kỳ',
            time: '10 phút trước',
            source: 'Hệ thống bảo trì'
        },
        {
            id: 2,
            level: 'info',
            message: 'Cập nhật phần mềm v2.1.3 có sẵn',
            time: '2 giờ trước',
            source: 'Hệ thống'
        },
        {
            id: 3,
            level: 'error',
            message: 'Lỗi kết nối GPS xe VT-007',
            time: '5 giờ trước',
            source: 'GPS Tracking'
        },
        {
            id: 4,
            level: 'success',
            message: 'Backup dữ liệu hoàn thành',
            time: '1 ngày trước',
            source: 'Backup System'
        },
    ];
    const getLevelColor = (level) => {
        switch (level) {
            case 'error': return 'text-red-400 bg-red-500/20';
            case 'warning': return 'text-yellow-400 bg-yellow-500/20';
            case 'info': return 'text-blue-400 bg-blue-500/20';
            case 'success': return 'text-green-400 bg-green-500/20';
            default: return 'text-white bg-white/20';
        }
    };
    const getLevelIcon = (level) => {
        switch (level) {
            case 'error': return '❌';
            case 'warning': return '⚠️';
            case 'info': return 'ℹ️';
            case 'success': return '✅';
            default: return '📝';
        }
    };
    return (_jsxs(GlassCard, { className: "space-y-6", children: [_jsxs("div", { className: "flex items-center justify-between", children: [_jsx("h2", { className: "text-xl font-semibold text-white", children: "T\u1ED5ng quan h\u1EC7 th\u1ED1ng" }), _jsxs("div", { className: "flex gap-2", children: [_jsx(GlassButton, { size: "sm", variant: "green", children: "\uD83D\uDCCA B\u00E1o c\u00E1o" }), _jsx(GlassButton, { size: "sm", variant: "primary", children: "\u2699\uFE0F C\u00E0i \u0111\u1EB7t" })] })] }), _jsx("div", { className: "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4", children: systemStats.map((stat, index) => (_jsx(StatCard, { title: stat.title, value: stat.value, subtitle: stat.subtitle, trend: stat.trend, icon: stat.icon }, index))) }), _jsxs("div", { className: "grid grid-cols-1 lg:grid-cols-2 gap-6", children: [_jsxs("div", { className: "space-y-4", children: [_jsx("h3", { className: "text-lg font-medium text-white", children: "T\u00ECnh tr\u1EA1ng h\u1EC7 th\u1ED1ng" }), _jsx("div", { className: "space-y-3", children: [
                                    { service: 'API Gateway', status: 'Hoạt động', uptime: '99.9%' },
                                    { service: 'Database', status: 'Hoạt động', uptime: '99.8%' },
                                    { service: 'GPS Service', status: 'Cảnh báo', uptime: '98.2%' },
                                    { service: 'Notification', status: 'Hoạt động', uptime: '99.7%' },
                                ].map((service, index) => (_jsxs("div", { className: "backdrop-blur-lg bg-white/5 border border-white/10 rounded-lg p-4 flex items-center justify-between", children: [_jsxs("div", { className: "flex items-center gap-3", children: [_jsx("div", { className: `w-3 h-3 rounded-full ${service.status === 'Hoạt động' ? 'bg-green-400' : 'bg-yellow-400'}` }), _jsx("span", { className: "text-white font-medium", children: service.service })] }), _jsxs("div", { className: "text-right", children: [_jsx("div", { className: "text-white/80 text-sm", children: service.uptime }), _jsx("div", { className: `text-xs ${service.status === 'Hoạt động' ? 'text-green-400' : 'text-yellow-400'}`, children: service.status })] })] }, index))) })] }), _jsxs("div", { className: "space-y-4", children: [_jsxs("div", { className: "flex items-center justify-between", children: [_jsx("h3", { className: "text-lg font-medium text-white", children: "C\u1EA3nh b\u00E1o g\u1EA7n \u0111\u00E2y" }), _jsx(GlassButton, { size: "sm", variant: "secondary", children: "Xem t\u1EA5t c\u1EA3" })] }), _jsx("div", { className: "space-y-3", children: alerts.map((alert) => (_jsx("div", { className: "backdrop-blur-lg bg-white/5 border border-white/10 rounded-lg p-4", children: _jsxs("div", { className: "flex items-start gap-3", children: [_jsx("span", { className: "text-lg", children: getLevelIcon(alert.level) }), _jsxs("div", { className: "flex-1", children: [_jsx("p", { className: "text-white text-sm", children: alert.message }), _jsxs("div", { className: "flex items-center gap-2 mt-2", children: [_jsx("span", { className: `px-2 py-1 rounded-full text-xs font-medium ${getLevelColor(alert.level)}`, children: alert.level.toUpperCase() }), _jsx("span", { className: "text-white/60 text-xs", children: alert.time }), _jsxs("span", { className: "text-white/60 text-xs", children: ["\u2022 ", alert.source] })] })] })] }) }, alert.id))) })] })] }), _jsxs("div", { className: "space-y-4", children: [_jsx("h3", { className: "text-lg font-medium text-white", children: "H\u00E0nh \u0111\u1ED9ng nhanh" }), _jsx("div", { className: "grid grid-cols-2 md:grid-cols-4 gap-4", children: [
                            { label: 'Backup ngay', icon: '💾', variant: 'primary' },
                            { label: 'Restart services', icon: '🔄', variant: 'secondary' },
                            { label: 'Xem logs', icon: '📋', variant: 'secondary' },
                            { label: 'Maintenance mode', icon: '🛠️', variant: 'danger' },
                        ].map((action, index) => (_jsxs(GlassButton, { variant: action.variant, className: "flex items-center justify-center gap-2 h-16", children: [_jsx("span", { className: "text-xl", children: action.icon }), _jsx("span", { children: action.label })] }, index))) })] })] }));
}
