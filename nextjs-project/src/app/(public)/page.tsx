import Link from "next/link";

// Force dynamic rendering to avoid prerendering issues
export const dynamic = 'force-dynamic'

export default function PublicHome() {
  return (
    <div className="min-h-screen bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
      <div className="container mx-auto px-4 py-16">
        <div className="text-center mb-12">
          <h1 className="text-5xl font-bold text-gray-900 mb-6">
            Fast Route Logistics
          </h1>
          <p className="text-xl text-gray-600 mb-8">
            Dịch vụ giao hàng thông minh với công nghệ tối ưu hóa tuyến đường
          </p>
          <div className="flex gap-4 justify-center">
            <Link 
              href="/login"
              className="bg-blue-600 hover:bg-blue-700 text-white px-8 py-3 rounded-lg font-semibold transition-colors"
            >
              Đăng nhập
            </Link>
            <Link 
              href="/register"
              className="bg-gray-200 hover:bg-gray-300 text-gray-800 px-8 py-3 rounded-lg font-semibold transition-colors"
            >
              Đăng ký
            </Link>
          </div>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-3 gap-8 mt-16">
          <div className="bg-white/50 backdrop-blur-sm rounded-xl p-6 text-center">
            <div className="text-4xl mb-4">🚚</div>
            <h3 className="text-xl font-semibold mb-2">Giao hàng nhanh</h3>
            <p className="text-gray-600">Tối ưu tuyến đường để giao hàng nhanh nhất</p>
          </div>
          
          <div className="bg-white/50 backdrop-blur-sm rounded-xl p-6 text-center">
            <div className="text-4xl mb-4">📍</div>
            <h3 className="text-xl font-semibold mb-2">Theo dõi real-time</h3>
            <p className="text-gray-600">Cập nhật vị trí đơn hàng liên tục</p>
          </div>
          
          <div className="bg-white/50 backdrop-blur-sm rounded-xl p-6 text-center">
            <div className="text-4xl mb-4">💰</div>
            <h3 className="text-xl font-semibold mb-2">Giá cả hợp lý</h3>
            <p className="text-gray-600">Chi phí vận chuyển tối ưu nhất</p>
          </div>
        </div>
      </div>
    </div>
  );
}