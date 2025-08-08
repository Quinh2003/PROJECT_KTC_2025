import WeeklyPerformanceChart from '../../components/WeeklyPerformanceChart';

export default function OperationsOverview() {
  return (
    <div className="space-y-6">
      {/* Overview Stats */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-6">
        <div className="bg-white/30 backdrop-blur-lg border border-white/30 rounded-2xl p-6 shadow-xl hover:shadow-2xl transition-all duration-300 hover:scale-105">
          <div className="flex items-center justify-between mb-4">
            <h3 className="text-gray-700 font-medium">Đơn hàng hôm nay</h3>
            <span className="text-2xl">📦</span>
          </div>
          <p className="text-3xl font-bold text-gray-800">127</p>
          <p className="text-green-600 text-sm mt-2">+8.2% so với hôm qua</p>
        </div>
        
        <div className="bg-white/30 backdrop-blur-lg border border-white/30 rounded-2xl p-6 shadow-xl hover:shadow-2xl transition-all duration-300 hover:scale-105">
          <div className="flex items-center justify-between mb-4">
            <h3 className="text-gray-700 font-medium">Xe đang hoạt động</h3>
            <span className="text-2xl">🚛</span>
          </div>
          <p className="text-3xl font-bold text-gray-800">18/24</p>
          <p className="text-gray-600 text-sm mt-2">75% tổng số xe</p>
        </div>
        
        <div className="bg-white/30 backdrop-blur-lg border border-white/30 rounded-2xl p-6 shadow-xl hover:shadow-2xl transition-all duration-300 hover:scale-105">
          <div className="flex items-center justify-between mb-4">
            <h3 className="text-gray-700 font-medium">Doanh thu hôm nay</h3>
            <span className="text-2xl">💰</span>
          </div>
          <p className="text-3xl font-bold text-gray-800">2.4M</p>
          <p className="text-green-600 text-sm mt-2">+12.5% so với hôm qua</p>
        </div>
        
        <div className="bg-white/30 backdrop-blur-lg border border-white/30 rounded-2xl p-6 shadow-xl hover:shadow-2xl transition-all duration-300 hover:scale-105">
          <div className="flex items-center justify-between mb-4">
            <h3 className="text-gray-700 font-medium">Hiệu suất TB</h3>
            <span className="text-2xl">⚡</span>
          </div>
          <p className="text-3xl font-bold text-gray-800">87%</p>
          <p className="text-green-600 text-sm mt-2">+3.1% so với tuần trước</p>
        </div>
      </div>
      
      <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
        {/* Recent Activities */}
        <div className="bg-white/30 backdrop-blur-lg border border-white/30 rounded-2xl p-6 shadow-xl">
          <div className="flex items-center justify-between mb-4">
            <h3 className="text-xl font-semibold text-gray-800">Hoạt động gần đây</h3>
            <button className="px-3 py-1 text-sm bg-white/30 backdrop-blur-lg border border-white/30 rounded-lg hover:bg-white/40 transition-all duration-300">
              🔄 Làm mới
            </button>
          </div>
          <div className="space-y-3">
            {[
              { time: '14:30', action: 'Đơn hàng DH-127 đã được giao thành công tại Hải Phòng', status: 'success' },
              { time: '14:15', action: 'Xe VT-003 bắt đầu chuyến đi mới từ Hà Nội đi Quảng Ninh', status: 'info' },
              { time: '14:00', action: 'Cảnh báo: Xe VT-007 cần bảo trì định kỳ sau 45,000km', status: 'warning' },
              { time: '13:45', action: 'Tài xế Nguyễn Văn A hoàn thành ca làm việc với 95% hiệu suất', status: 'info' },
              { time: '13:30', action: 'Khách hàng ABC Corp đánh giá 5 sao cho đơn hàng DH-124', status: 'success' },
            ].map((activity, index) => (
              <div key={index} className="flex items-start gap-3 p-3 bg-white/20 rounded-lg hover:bg-white/30 transition-all duration-300">
                <div className={`w-2 h-2 rounded-full mt-2 flex-shrink-0 ${
                  activity.status === 'success' ? 'bg-green-500' :
                  activity.status === 'warning' ? 'bg-yellow-500' : 'bg-blue-500'
                }`} />
                <div className="flex-1 min-w-0">
                  <p className="text-gray-800 text-sm">{activity.action}</p>
                  <p className="text-gray-600 text-xs">{activity.time}</p>
                </div>
              </div>
            ))}
          </div>
        </div>

        {/* Weekly Performance Chart */}
        <WeeklyPerformanceChart />
      </div>

      {/* Fleet Status Overview */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div className="bg-white/30 backdrop-blur-lg border border-white/30 rounded-2xl p-6 shadow-xl">
          <h3 className="text-xl font-semibold text-gray-800 mb-4">Trạng thái đội xe</h3>
          <div className="space-y-3">
            {[
              { status: 'Đang giao hàng', count: 12, color: 'bg-green-500' },
              { status: 'Đang chờ', count: 6, color: 'bg-yellow-500' },
              { status: 'Bảo trì', count: 3, color: 'bg-red-500' },
              { status: 'Nghỉ', count: 3, color: 'bg-gray-500' },
            ].map((item, index) => (
              <div key={index} className="flex items-center justify-between p-3 bg-white/20 rounded-lg">
                <div className="flex items-center gap-3">
                  <div className={`w-3 h-3 rounded-full ${item.color}`} />
                  <span className="text-gray-800 font-medium">{item.status}</span>
                </div>
                <span className="text-gray-700 font-bold">{item.count}</span>
              </div>
            ))}
          </div>
        </div>

        <div className="bg-white/30 backdrop-blur-lg border border-white/30 rounded-2xl p-6 shadow-xl">
          <h3 className="text-xl font-semibold text-gray-800 mb-4">Cảnh báo hệ thống</h3>
          <div className="space-y-3">
            {[
              { type: 'Xe VT-007 cần bảo trì', priority: 'high', time: '2 giờ trước' },
              { type: 'Nhiên liệu xe VT-003 thấp', priority: 'medium', time: '3 giờ trước' },
              { type: 'Backup hoàn thành', priority: 'low', time: '5 giờ trước' },
            ].map((alert, index) => (
              <div key={index} className="flex items-center gap-3 p-3 bg-white/20 rounded-lg">
                <div className={`w-2 h-2 rounded-full ${
                  alert.priority === 'high' ? 'bg-red-500' :
                  alert.priority === 'medium' ? 'bg-yellow-500' : 'bg-green-500'
                }`} />
                <div className="flex-1">
                  <p className="text-gray-800 text-sm font-medium">{alert.type}</p>
                  <p className="text-gray-600 text-xs">{alert.time}</p>
                </div>
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>
  );
}
