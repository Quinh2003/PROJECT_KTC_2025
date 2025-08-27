"use client";

import Link from "next/link";

export default function CustomerAccount() {
  return (
    <div className="space-y-8">
      {/* Welcome Section */}
      <div className="bg-white/10 backdrop-blur-xl rounded-3xl border border-white/20 shadow-2xl p-8">
        <h2 className="text-3xl font-bold text-white mb-4 drop-shadow-lg">
          Chào mừng đến với Fast Route! 🚚
        </h2>
        <p className="text-white/80 text-lg leading-relaxed">
          Dịch vụ giao hàng thông minh với công nghệ tối ưu hóa tuyến đường. 
          Chúng tôi cam kết mang đến trải nghiệm giao hàng nhanh chóng, an toàn và tiết kiệm chi phí.
        </p>
      </div>

      {/* Quick Actions */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
        <Link 
          href="/account/orders/new"
          className="bg-white/10 backdrop-blur-xl rounded-2xl border border-white/20 shadow-xl p-6 hover:bg-white/15 transition-all duration-300 cursor-pointer group block"
        >
          <div className="text-4xl mb-4 group-hover:scale-110 transition-transform duration-300">📦</div>
          <h3 className="text-xl font-semibold text-white mb-2">Tạo đơn hàng</h3>
          <p className="text-white/70 text-sm">Tạo đơn hàng giao hàng mới một cách nhanh chóng</p>
        </Link>

        <Link 
          href="/account/orders"
          className="bg-white/10 backdrop-blur-xl rounded-2xl border border-white/20 shadow-xl p-6 hover:bg-white/15 transition-all duration-300 cursor-pointer group block"
        >
          <div className="text-4xl mb-4 group-hover:scale-110 transition-transform duration-300">📍</div>
          <h3 className="text-xl font-semibold text-white mb-2">Theo dõi đơn hàng</h3>
          <p className="text-white/70 text-sm">Xem trạng thái và vị trí đơn hàng real-time</p>
        </Link>

        <Link 
          href="/account/estimate"
          className="bg-white/10 backdrop-blur-xl rounded-2xl border border-white/20 shadow-xl p-6 hover:bg-white/15 transition-all duration-300 cursor-pointer group block"
        >
          <div className="text-4xl mb-4 group-hover:scale-110 transition-transform duration-300">💰</div>
          <h3 className="text-xl font-semibold text-white mb-2">Tính phí giao hàng</h3>
          <p className="text-white/70 text-sm">Ước tính chi phí giao hàng trước khi đặt</p>
        </Link>
      </div>

      {/* Recent Orders */}
      <div className="bg-white/10 backdrop-blur-xl rounded-3xl border border-white/20 shadow-2xl p-8">
        <div className="flex justify-between items-center mb-6">
          <h3 className="text-2xl font-bold text-white drop-shadow-lg">
            Đơn hàng gần đây
          </h3>
          <Link 
            href="/account/orders"
            className="text-blue-300 hover:text-blue-200 text-sm font-medium underline decoration-dotted"
          >
            Xem tất cả
          </Link>
        </div>
        <div className="text-center py-12">
          <div className="text-6xl mb-4 opacity-50">📋</div>
          <p className="text-white/60 text-lg">Chưa có đơn hàng nào</p>
          <p className="text-white/40 text-sm mt-2">Tạo đơn hàng đầu tiên để bắt đầu!</p>
          <Link 
            href="/account/orders/new"
            className="inline-block mt-4 px-6 py-3 bg-blue-500/20 hover:bg-blue-500/30 text-white rounded-xl border border-blue-400/30 transition-all duration-200 font-medium"
          >
            Tạo đơn hàng ngay
          </Link>
        </div>
      </div>

      {/* Stats Overview */}
      <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
        <div className="bg-white/10 backdrop-blur-xl rounded-2xl border border-white/20 shadow-xl p-6">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-white/70 text-sm">Tổng đơn hàng</p>
              <p className="text-white text-2xl font-bold">0</p>
            </div>
            <div className="text-3xl opacity-70">📊</div>
          </div>
        </div>

        <div className="bg-white/10 backdrop-blur-xl rounded-2xl border border-white/20 shadow-xl p-6">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-white/70 text-sm">Đang vận chuyển</p>
              <p className="text-white text-2xl font-bold">0</p>
            </div>
            <div className="text-3xl opacity-70">🚛</div>
          </div>
        </div>

        <div className="bg-white/10 backdrop-blur-xl rounded-2xl border border-white/20 shadow-xl p-6">
          <div className="flex items-center justify-between">
            <div>
              <p className="text-white/70 text-sm">Đã hoàn thành</p>
              <p className="text-white text-2xl font-bold">0</p>
            </div>
            <div className="text-3xl opacity-70">✅</div>
          </div>
        </div>
      </div>
    </div>
  );
}