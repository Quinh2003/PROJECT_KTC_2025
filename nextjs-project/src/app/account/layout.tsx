"use client";

import { useEffect, useState } from "react";
import { useRouter, usePathname } from "next/navigation";
import Link from "next/link";
import { User } from "../../types/User";

interface AccountLayoutProps {
  children: React.ReactNode;
}

export default function AccountLayout({ children }: AccountLayoutProps) {
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(true);
  const router = useRouter();
  const pathname = usePathname();

  useEffect(() => {
    // Check authentication and role
    let storedUser = null;
    let token = null;
    if (typeof window !== "undefined") {
      storedUser = localStorage.getItem("user");
      token = localStorage.getItem("token");
    }

    if (!storedUser || !token) {
      router.push("/login");
      return;
    }

    const userData = JSON.parse(storedUser);
    
    // Verify user is customer
    if (userData.role?.toLowerCase() !== "customer") {
      alert("Truy cập không được phép!");
      if (typeof window !== "undefined") {
        localStorage.clear();
      }
      router.push("/login");
      return;
    }

    setUser(userData);
    setLoading(false);
  }, [router]);

  const handleLogout = () => {
    if (typeof window !== "undefined") {
      localStorage.clear();
    }
    router.push("/login");
  };

  const navItems = [
    { href: "/account", label: "Trang chủ", icon: "🏠" },
    { href: "/account/orders", label: "Đơn hàng", icon: "📦" },
    { href: "/account/estimate", label: "Tính phí", icon: "💰" },
    { href: "/account/profile", label: "Thông tin cá nhân", icon: "👤" },
  ];

  if (loading) {
    return (
      <div className="min-h-screen bg-gradient-to-br from-blue-600 via-purple-600 to-cyan-500 flex items-center justify-center">
        <div className="bg-white/10 backdrop-blur-xl rounded-3xl border border-white/20 shadow-2xl p-8">
          <div className="flex items-center gap-3">
            <div className="w-6 h-6 border-2 border-white/30 border-t-white rounded-full animate-spin"></div>
            <span className="text-white font-medium">Đang tải...</span>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen bg-gradient-to-br from-blue-600 via-purple-600 to-cyan-500">
      {/* Header */}
      <header className="bg-white/10 backdrop-blur-xl border-b border-white/20 sticky top-0 z-50">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex justify-between items-center py-4">
            <div className="flex items-center">
              <Link href="/account" className="text-2xl font-bold text-white drop-shadow-lg hover:text-blue-200 transition-colors">
                Fast Route Logistics
              </Link>
              <span className="ml-4 px-3 py-1 bg-blue-500/30 backdrop-blur-sm rounded-full text-sm text-white border border-blue-400/30">
                Khách hàng
              </span>
            </div>
            <div className="flex items-center gap-4">
              <span className="text-white/90 text-sm hidden sm:block">
                Xin chào, <span className="font-semibold">{user?.fullName}</span>
              </span>
              <button
                onClick={handleLogout}
                className="px-4 py-2 bg-red-500/20 hover:bg-red-500/30 text-white rounded-xl border border-red-400/30 transition-all duration-200 text-sm font-medium"
              >
                Đăng xuất
              </button>
            </div>
          </div>
        </div>
      </header>

      {/* Navigation */}
      <nav className="bg-white/5 backdrop-blur-xl border-b border-white/10 sticky top-[73px] z-40">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex space-x-1 py-2 overflow-x-auto">
            {navItems.map((item) => {
              const isActive = pathname === item.href;
              return (
                <Link 
                  key={item.href}
                  href={item.href} 
                  className={`flex items-center gap-2 px-4 py-3 rounded-xl transition-all duration-200 text-sm font-medium whitespace-nowrap ${
                    isActive 
                      ? 'bg-blue-500/30 text-white border border-blue-400/50' 
                      : 'text-white/70 hover:text-white hover:bg-white/10'
                  }`}
                >
                  <span className="text-base">{item.icon}</span>
                  {item.label}
                </Link>
              );
            })}
          </div>
        </div>
      </nav>

      {/* Main Content */}
      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
        {children}
      </main>

      {/* Footer */}
      <footer className="bg-white/5 backdrop-blur-xl border-t border-white/10 mt-16">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-6">
          <div className="text-center">
            <p className="text-white/50 text-sm">
              © 2025 Fast Route Logistics. Tất cả quyền được bảo lưu.
            </p>
          </div>
        </div>
      </footer>
    </div>
  );
}
