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

  const getLevelColor = (level: string) => {
    switch (level) {
      case 'error': return 'text-red-400 bg-red-500/20';
      case 'warning': return 'text-yellow-400 bg-yellow-500/20';
      case 'info': return 'text-blue-400 bg-blue-500/20';
      case 'success': return 'text-green-400 bg-green-500/20';
      default: return 'text-white bg-white/20';
    }
  };

  const getLevelIcon = (level: string) => {
    switch (level) {
      case 'error': return '❌';
      case 'warning': return '⚠️';
      case 'info': return 'ℹ️';
      case 'success': return '✅';
      default: return '📝';
    }
  };

  return (
    <GlassCard className="space-y-6">
      <div className="flex items-center justify-between">
        <h2 className="text-xl font-semibold text-white">Tổng quan hệ thống</h2>
        <div className="flex gap-2">
          <GlassButton size="sm" variant="green">
            📊 Báo cáo
          </GlassButton>
          <GlassButton size="sm" variant="primary">
            ⚙️ Cài đặt
          </GlassButton>
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
        {systemStats.map((stat, index) => (
          <StatCard
            key={index}
            title={stat.title}
            value={stat.value}
            subtitle={stat.subtitle}
            trend={stat.trend}
            icon={stat.icon}
          />
        ))}
      </div>

      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* System Health */}
        <div className="space-y-4">
          <h3 className="text-lg font-medium text-white">Tình trạng hệ thống</h3>
          <div className="space-y-3">
            {[
              { service: 'API Gateway', status: 'Hoạt động', uptime: '99.9%' },
              { service: 'Database', status: 'Hoạt động', uptime: '99.8%' },
              { service: 'GPS Service', status: 'Cảnh báo', uptime: '98.2%' },
              { service: 'Notification', status: 'Hoạt động', uptime: '99.7%' },
            ].map((service, index) => (
              <div 
                key={index}
                className="backdrop-blur-lg bg-white/5 border border-white/10 rounded-lg p-4 flex items-center justify-between"
              >
                <div className="flex items-center gap-3">
                  <div className={`w-3 h-3 rounded-full ${
                    service.status === 'Hoạt động' ? 'bg-green-400' : 'bg-yellow-400'
                  }`} />
                  <span className="text-white font-medium">{service.service}</span>
                </div>
                <div className="text-right">
                  <div className="text-white/80 text-sm">{service.uptime}</div>
                  <div className={`text-xs ${
                    service.status === 'Hoạt động' ? 'text-green-400' : 'text-yellow-400'
                  }`}>
                    {service.status}
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>

        {/* Recent Alerts */}
        <div className="space-y-4">
          <div className="flex items-center justify-between">
            <h3 className="text-lg font-medium text-white">Cảnh báo gần đây</h3>
            <GlassButton size="sm" variant="secondary">
              Xem tất cả
            </GlassButton>
          </div>
          <div className="space-y-3">
            {alerts.map((alert) => (
              <div 
                key={alert.id}
                className="backdrop-blur-lg bg-white/5 border border-white/10 rounded-lg p-4"
              >
                <div className="flex items-start gap-3">
                  <span className="text-lg">{getLevelIcon(alert.level)}</span>
                  <div className="flex-1">
                    <p className="text-white text-sm">{alert.message}</p>
                    <div className="flex items-center gap-2 mt-2">
                      <span className={`px-2 py-1 rounded-full text-xs font-medium ${getLevelColor(alert.level)}`}>
                        {alert.level.toUpperCase()}
                      </span>
                      <span className="text-white/60 text-xs">{alert.time}</span>
                      <span className="text-white/60 text-xs">• {alert.source}</span>
                    </div>
                  </div>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>

      {/* Quick Actions */}
      <div className="space-y-4">
        <h3 className="text-lg font-medium text-white">Hành động nhanh</h3>
        <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
          {[
            { label: 'Backup ngay', icon: '💾', variant: 'primary' as const },
            { label: 'Restart services', icon: '🔄', variant: 'secondary' as const },
            { label: 'Xem logs', icon: '📋', variant: 'secondary' as const },
            { label: 'Maintenance mode', icon: '🛠️', variant: 'danger' as const },
          ].map((action, index) => (
            <GlassButton
              key={index}
              variant={action.variant}
              className="flex items-center justify-center gap-2 h-16"
            >
              <span className="text-xl">{action.icon}</span>
              <span>{action.label}</span>
            </GlassButton>
          ))}
        </div>
      </div>
    </GlassCard>
  );
}
