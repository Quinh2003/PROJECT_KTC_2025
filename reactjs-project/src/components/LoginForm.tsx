import { useState } from "react";
import type { User } from "../types/User";

const demoAccounts: User[] = [
  { role: "ADMIN", email: "admin@ktc.com", password: "123456", name: "Admin" },
  { role: "DISPATCHER", email: "dispatcher@ktc.com", password: "123456", name: "Dispatcher" },
  { role: "FLEET_MANAGER", email: "fleet@ktc.com", password: "123456", name: "Fleet Manager" },
  { role: "DRIVER", email: "driver@ktc.com", password: "123456", name: "Driver" },
  { role: "OPERATIONS_MANAGER", email: "operations@ktc.com", password: "123456", name: "Operations" },
];

export default function LoginForm({ onLogin }: { onLogin: (user: User) => void }) {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [error, setError] = useState("");
  const [showPassword, setShowPassword] = useState(false);

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    const found = demoAccounts.find(
      (acc) => acc.email === email && acc.password === password
    );
    if (found) {
      setError("");
      onLogin(found);
    } else {
      setError("Sai tài khoản hoặc mật khẩu demo!");
    }
  };

  return (
    <div className="min-h-screen bg-gradient-to-br from-purple-100 via-pink-50 to-purple-100 flex items-center justify-center p-5">
      <div className="w-full max-w-md">
        <div className="bg-white rounded-3xl shadow-2xl p-8 backdrop-blur-sm border border-white/20">
          {/* Logo & Header */}
          <div className="text-center mb-8">
            <div className="w-20 h-20 bg-gradient-to-r from-purple-400 to-purple-600 rounded-2xl mx-auto mb-4 flex items-center justify-center shadow-lg">
              <div className="w-10 h-10 bg-white rounded-lg flex items-center justify-center">
                📦
              </div>
            </div>
            <h2 className="text-3xl font-bold bg-gradient-to-r from-purple-600 to-purple-800 bg-clip-text text-transparent mb-2">
              KTC Logistics
            </h2>
            <p className="text-gray-500 text-sm">
              Đăng nhập vào hệ thống
            </p>
          </div>

          {/* Form */}
          <form onSubmit={handleSubmit} className="space-y-6">
            <div>
              <label className="block text-sm font-medium text-purple-700 mb-2">
                Email
              </label>
              <div className="relative">
                <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                  <span className="text-gray-400 text-lg">✉️</span>
                </div>
                <input
                  type="email"
                  value={email}
                  onChange={e => setEmail(e.target.value)}
                  className="w-full pl-12 pr-4 py-3 border-2 border-purple-100 rounded-xl focus:ring-2 focus:ring-purple-500 focus:border-purple-500 outline-none transition-all placeholder-gray-400"
                  placeholder="Nhập email của bạn"
                  required
                />
              </div>
            </div>

            <div>
              <label className="block text-sm font-medium text-purple-700 mb-2">
                Mật khẩu
              </label>
              <div className="relative">
                <div className="absolute inset-y-0 left-0 pl-3 flex items-center pointer-events-none">
                  <span className="text-gray-400 text-lg">🔒</span>
                </div>
                <input
                  type={showPassword ? "text" : "password"}
                  value={password}
                  onChange={e => setPassword(e.target.value)}
                  className="w-full pl-12 pr-12 py-3 border-2 border-purple-100 rounded-xl focus:ring-2 focus:ring-purple-500 focus:border-purple-500 outline-none transition-all placeholder-gray-400"
                  placeholder="Nhập mật khẩu"
                  required
                />
                <button
                  type="button"
                  onClick={() => setShowPassword(!showPassword)}
                  className="absolute inset-y-0 right-0 pr-3 flex items-center text-gray-400 hover:text-purple-600 transition-colors"
                >
                  <span className="text-lg">{showPassword ? '👁️' : '👁️‍🗨️'}</span>
                </button>
              </div>
            </div>

            {error && (
              <div className="bg-red-50 border border-red-200 rounded-xl p-3 flex items-center space-x-2">
                <span className="text-red-500">⚠️</span>
                <span className="text-red-600 text-sm">{error}</span>
              </div>
            )}

            <button
              type="submit"
              className="w-full bg-gradient-to-r from-purple-500 to-purple-600 text-white py-3 px-4 rounded-xl font-semibold text-lg hover:from-purple-600 hover:to-purple-700 focus:ring-4 focus:ring-purple-200 transition-all duration-200 transform hover:scale-[1.02] active:scale-[0.98] shadow-lg"
            >
              Đăng nhập
            </button>
          </form>

          {/* Footer */}
          <div className="text-center mt-6">
            <p className="text-gray-400 text-xs">
              © 2025 KTC Logistics. All rights reserved.
            </p>
          </div>
        </div>
      </div>
    </div>
    );
}