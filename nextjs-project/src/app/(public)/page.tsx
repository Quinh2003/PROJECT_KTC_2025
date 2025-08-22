"use client";

import Image from "next/image";
import { useState, useEffect } from "react";
import { useRouter, useSearchParams } from "next/navigation";

export default function PublicHome() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const [trackingCode, setTrackingCode] = useState("");
  const [trackingResult, setTrackingResult] = useState<{
    code: string;
    status: string;
    from: string;
    to: string;
    currentLocation: string;
    estimatedDelivery: string;
  } | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [isLoggedIn, setIsLoggedIn] = useState(false);

  useEffect(() => {
    // Check if user is logged in
    const token = localStorage.getItem("token");
    setIsLoggedIn(!!token);

    // Check if redirected from ReactJS login with token
    const urlToken = searchParams.get("token");
    const urlUser = searchParams.get("user");
    
    if (urlToken && urlUser) {
      try {
        // Store authentication data
        localStorage.setItem("token", urlToken);
        localStorage.setItem("user", decodeURIComponent(urlUser));
        setIsLoggedIn(true);
        
        // Clean URL and redirect to account dashboard
        window.history.replaceState({}, document.title, "/");
        router.push("/account");
      } catch (error) {
        console.error("Error processing authentication:", error);
      }
    }

    // Auto-fill tracking code if provided in URL
    const urlTrackingCode = searchParams.get("trackingCode");
    if (urlTrackingCode) {
      setTrackingCode(urlTrackingCode);
    }
  }, [searchParams, router]);

  const handleTrackingSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!trackingCode.trim()) return;

    setIsLoading(true);
    try {
      // Simulate API call - replace with actual API
      setTimeout(() => {
        setTrackingResult({
          code: trackingCode,
          status: "Đang giao hàng",
          from: "123 Đường Lê Lợi, Q1, TP.HCM",
          to: "456 Đường Nguyễn Huệ, Q3, TP.HCM",
          currentLocation: "Đang trên đường giao",
          estimatedDelivery: "Hôm nay, 15:30"
        });
        setIsLoading(false);
      }, 1000);
    } catch (error) {
      console.error("Error tracking order:", error);
      setIsLoading(false);
    }
  };

  const handleLoginRedirect = () => {
    // Redirect to Next.js login page
    router.push("/login");
  };

  const handleCreateOrder = () => {
    // Check if user is logged in, if not redirect to login
    if (!isLoggedIn) {
      alert("Bạn cần đăng nhập để tạo đơn hàng!");
      handleLoginRedirect();
    } else {
      // Redirect to create order page
      router.push("/account/orders/new");
    }
  };

  const handleDashboard = () => {
    router.push("/account");
  };

  const handleLogout = () => {
    localStorage.removeItem("token");
    localStorage.removeItem("user");
    setIsLoggedIn(false);
    setTrackingResult(null);
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-50 via-white to-red-50">
      {/* Header */}
      <header className="bg-white shadow-md">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex justify-between items-center h-16">
            <div className="flex items-center">
              <Image
                src="/next.svg"
                alt="Fast Route Logo"
                width={120}
                height={30}
                priority
              />
              <span className="ml-2 text-xl font-bold text-red-600">Fast Route</span>
            </div>
            <div className="flex items-center gap-4">
              {isLoggedIn ? (
                <>
                  <button
                    onClick={handleDashboard}
                    className="text-red-600 px-4 py-2 rounded-lg hover:bg-red-50 transition-colors"
                  >
                    Tài khoản
                  </button>
                  <button
                    onClick={handleCreateOrder}
                    className="bg-red-600 text-white px-4 py-2 rounded-lg hover:bg-red-700 transition-colors"
                  >
                    Tạo đơn hàng
                  </button>
                  <button
                    onClick={handleLogout}
                    className="border border-gray-300 text-gray-600 px-4 py-2 rounded-lg hover:bg-gray-50 transition-colors"
                  >
                    Đăng xuất
                  </button>
                </>
              ) : (
                <>
                  <button
                    onClick={handleCreateOrder}
                    className="bg-red-600 text-white px-4 py-2 rounded-lg hover:bg-red-700 transition-colors"
                  >
                    Tạo đơn hàng
                  </button>
                  <button
                    onClick={handleLoginRedirect}
                    className="border border-red-600 text-red-600 px-4 py-2 rounded-lg hover:bg-red-50 transition-colors"
                  >
                    Đăng nhập
                  </button>
                </>
              )}
            </div>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        {/* Hero Section */}
        <div className="text-center mb-12">
          <h1 className="text-4xl font-bold text-gray-800 mb-4">
            Dịch vụ giao hàng nhanh chóng, đáng tin cậy
          </h1>
          <p className="text-xl text-gray-600 mb-8">
            Tra cứu mã vận đơn hoặc tạo đơn hàng mới ngay hôm nay
          </p>
        </div>

        {/* Tracking Section */}
        <div className="bg-white rounded-2xl shadow-lg p-8 mb-12">
          <div className="max-w-2xl mx-auto">
            <h2 className="text-2xl font-bold text-gray-800 mb-6 text-center">
              Tra cứu mã vận đơn
            </h2>
            <form onSubmit={handleTrackingSubmit} className="space-y-4">
              <div className="flex gap-4">
                <input
                  type="text"
                  value={trackingCode}
                  onChange={(e) => setTrackingCode(e.target.value)}
                  placeholder="Nhập mã vận đơn (VD: FR001, FR002...)"
                  className="flex-1 px-4 py-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-red-500 focus:border-red-500"
                />
                <button
                  type="submit"
                  disabled={isLoading}
                  className="bg-red-600 text-white px-8 py-3 rounded-lg hover:bg-red-700 transition-colors disabled:opacity-50"
                >
                  {isLoading ? "Đang tìm..." : "Tra cứu"}
                </button>
              </div>
            </form>

            {/* Tracking Result */}
            {trackingResult && (
              <div className="mt-8 p-6 bg-green-50 border border-green-200 rounded-lg">
                <h3 className="text-lg font-semibold text-green-800 mb-4">
                  Thông tin đơn hàng: {trackingResult.code}
                </h3>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div>
                    <p className="text-sm text-gray-600">Trạng thái:</p>
                    <p className="font-semibold text-green-700">{trackingResult.status}</p>
                  </div>
                  <div>
                    <p className="text-sm text-gray-600">Dự kiến giao:</p>
                    <p className="font-semibold">{trackingResult.estimatedDelivery}</p>
                  </div>
                  <div>
                    <p className="text-sm text-gray-600">Từ:</p>
                    <p className="font-semibold">{trackingResult.from}</p>
                  </div>
                  <div>
                    <p className="text-sm text-gray-600">Đến:</p>
                    <p className="font-semibold">{trackingResult.to}</p>
                  </div>
                  <div className="md:col-span-2">
                    <p className="text-sm text-gray-600">Vị trí hiện tại:</p>
                    <p className="font-semibold text-blue-600">{trackingResult.currentLocation}</p>
                  </div>
                </div>
              </div>
            )}
          </div>
        </div>

        {/* Features Section */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-8 mb-12">
          <div className="text-center p-6 bg-white rounded-xl shadow-lg">
            <div className="w-16 h-16 bg-red-100 rounded-full flex items-center justify-center mx-auto mb-4">
              <span className="text-2xl">🚚</span>
            </div>
            <h3 className="text-xl font-semibold mb-2">Giao hàng nhanh</h3>
            <p className="text-gray-600">Cam kết giao hàng trong vòng 24 giờ</p>
          </div>
          <div className="text-center p-6 bg-white rounded-xl shadow-lg">
            <div className="w-16 h-16 bg-blue-100 rounded-full flex items-center justify-center mx-auto mb-4">
              <span className="text-2xl">📍</span>
            </div>
            <h3 className="text-xl font-semibold mb-2">Theo dõi real-time</h3>
            <p className="text-gray-600">Theo dõi đơn hàng trực tiếp trên bản đồ</p>
          </div>
          <div className="text-center p-6 bg-white rounded-xl shadow-lg">
            <div className="w-16 h-16 bg-green-100 rounded-full flex items-center justify-center mx-auto mb-4">
              <span className="text-2xl">🔒</span>
            </div>
            <h3 className="text-xl font-semibold mb-2">An toàn bảo mật</h3>
            <p className="text-gray-600">Đảm bảo hàng hóa được giao đúng người nhận</p>
          </div>
        </div>

        {/* Call to Action */}
        <div className="bg-gradient-to-r from-red-500 to-red-600 rounded-2xl p-8 text-center text-white">
          <h2 className="text-3xl font-bold mb-4">Bắt đầu giao hàng ngay hôm nay</h2>
          <p className="text-xl mb-6">Đăng ký tài khoản để trải nghiệm dịch vụ tốt nhất</p>
          <div className="flex gap-4 justify-center">
            <button
              onClick={handleCreateOrder}
              className="bg-white text-red-600 px-8 py-3 rounded-lg font-semibold hover:bg-gray-100 transition-colors"
            >
              Tạo đơn hàng ngay
            </button>
            <button
              onClick={handleLoginRedirect}
              className="border-2 border-white text-white px-8 py-3 rounded-lg font-semibold hover:bg-white hover:text-red-600 transition-colors"
            >
              Đăng nhập / Đăng ký
            </button>
          </div>
        </div>
      </main>

      {/* Footer */}
      <footer className="bg-gray-800 text-white py-12 mt-16">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
            <div>
              <h3 className="text-lg font-semibold mb-4">Fast Route</h3>
              <p className="text-gray-400">Dịch vụ giao hàng nhanh chóng và đáng tin cậy</p>
            </div>
            <div>
              <h4 className="font-semibold mb-4">Dịch vụ</h4>
              <ul className="space-y-2 text-gray-400">
                <li>Giao hàng nội thành</li>
                <li>Giao hàng liên tỉnh</li>
                <li>Giao hàng quốc tế</li>
              </ul>
            </div>
            <div>
              <h4 className="font-semibold mb-4">Hỗ trợ</h4>
              <ul className="space-y-2 text-gray-400">
                <li>Hotline: 1900-xxxx</li>
                <li>Email: support@fastroute.com</li>
                <li>FAQ</li>
              </ul>
            </div>
            <div>
              <h4 className="font-semibold mb-4">Theo dõi</h4>
              <ul className="space-y-2 text-gray-400">
                <li>Facebook</li>
                <li>Instagram</li>
                <li>LinkedIn</li>
              </ul>
            </div>
          </div>
          <div className="border-t border-gray-700 mt-8 pt-8 text-center text-gray-400">
            <p>&copy; 2025 Fast Route. All rights reserved.</p>
          </div>
        </div>
      </footer>
    </div>
  );
}