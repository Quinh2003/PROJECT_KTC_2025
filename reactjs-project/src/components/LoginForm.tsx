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
  const [showDemo] = useState(false);
  const [showPassword, setShowPassword] = useState(false);

  const handleDemoClick = (account: User) => {
    setEmail(account.email);
    setPassword(account.password);
    setError("");
  };

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
    <div className="min-h-screen flex items-center justify-center bg-gradient-to-br from-blue-100 via-white to-teal-100">
      <div className="w-full max-w-md px-4">
        <div className="bg-white shadow-xl rounded-2xl p-8 border-0">
          <div className="text-center mb-6">
            <div className="bg-gradient-to-br from-blue-400 to-teal-300 p-4 rounded-full mx-auto mb-4 w-16 h-16 flex items-center justify-center">
              <span className="text-3xl">📦</span>
            </div>
            <h2 className="font-bold text-blue-700 mb-1 text-2xl">KTC Logistics</h2>
            <div className="text-teal-500 mb-2">Đăng nhập vào hệ thống</div>
          </div>
          <form onSubmit={handleSubmit}>
            <div className="mb-4">
              <label className="block text-blue-700 font-semibold mb-1">Email</label>
              <div className="flex items-center border rounded-lg bg-white">
                <span className="px-3 text-lg">✉️</span>
                <input
                  type="email"
                  className="flex-1 py-2 px-3 rounded-r-lg outline-none bg-transparent"
                  placeholder="Nhập email của bạn"
                  value={email}
                  onChange={e => setEmail(e.target.value)}
                  required
                />
              </div>
            </div>
            <div className="mb-4">
              <label className="block text-blue-700 font-semibold mb-1">Mật khẩu</label>
              <div className="flex items-center border rounded-lg bg-white">
                <span className="px-3 text-lg">🔒</span>
                <input
                  type={showPassword ? "text" : "password"}
                  className="flex-1 py-2 px-3 rounded-r-lg outline-none bg-transparent"
                  placeholder="Nhập mật khẩu"
                  value={password}
                  onChange={e => setPassword(e.target.value)}
                  required
                />
                <button
                  type="button"
                  className="px-3 text-gray-500 hover:text-blue-700 focus:outline-none"
                  tabIndex={-1}
                  onClick={() => setShowPassword(!showPassword)}
                >
                  {showPassword ? "👁️" : "👁️‍🗨️"}
                </button>
              </div>
            </div>
            {error && (
              <div className="bg-red-100 text-red-700 px-4 py-2 rounded-lg mb-4 flex items-center gap-2">
                <span>⚠️</span>
                <span>{error}</span>
              </div>
            )}
            <button
              type="submit"
              className="w-full py-2 font-bold mb-2 text-lg rounded-lg bg-blue-600 text-white hover:bg-teal-600 transition"
            >
              Đăng nhập
            </button>
          </form>
          <div className="text-center mt-3"></div>
          {showDemo && (
            <div className="mt-6 pt-4 border-t">
              <div className="font-semibold text-teal-500 mb-2 text-center">Tài khoản demo</div>
              <div className="grid gap-2">
                {demoAccounts.map((acc, idx) => (
                  <button
                    key={idx}
                    type="button"
                    className="bg-blue-50 border flex justify-between items-center px-4 py-2 rounded-lg hover:bg-teal-50 transition"
                    onClick={() => handleDemoClick(acc)}
                  >
                    <div>
                      <div className="font-semibold">{acc.name}</div>
                      <div className="text-teal-500 text-sm">{acc.email}</div>
                    </div>
                    <span className="bg-blue-600 text-white px-2 py-1 rounded text-xs font-semibold">
                      {acc.role.replace("_", " ")}
                    </span>
                  </button>
                ))}
              </div>
            </div>
          )}
        </div>
        <div className="text-center mt-3 text-teal-400 text-xs">
          © 2025 KTC Logistics. All rights reserved.
        </div>
      </div>
    </div>
  );
}