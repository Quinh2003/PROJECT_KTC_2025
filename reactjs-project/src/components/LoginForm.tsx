import { useState } from "react";
import type { User } from "../types/User";

export default function LoginForm({ onLogin }: { onLogin: (user: User) => void }) {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [error, setError] = useState("");
  const [showPassword, setShowPassword] = useState(false);
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError("");
    setLoading(true);
    try {
      const res = await fetch("http://localhost:8080/api/auth/login", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ email, password }),
      });
      if (!res.ok) {
        setError("Sai tài khoản hoặc mật khẩu!");
        setLoading(false);
        return;
      }
      const data = await res.json();
      // Lưu token và user vào localStorage
      localStorage.setItem("token", data.token);
      localStorage.setItem("user", JSON.stringify(data.user));
      onLogin(data.user);
    } catch (err) {
      setError("Không thể kết nối tới máy chủ!");
    } finally {
      setLoading(false);
    }
  };

  return (
    <div
      className="min-h-screen w-full flex items-center justify-center bg-cover bg-center relative"
      style={{
        backgroundImage: "url('/login.jpg')",
      }}
    >
      <div className="absolute inset-0 bg-black/10 backdrop-blur-sm z-0"></div>
      <div className="relative z-10 flex flex-col items-center w-full max-w-md">
        <form
          onSubmit={handleSubmit}
          className="w-full h-[450px] bg-white/10 backdrop-blur-lg justify-center rounded-2xl border border-white/30 shadow-xl px-10 py-10 flex flex-col gap-6"
        >
          <h2 className="text-3xl font-bold text-center text-white mb-6 drop-shadow">
            Login
          </h2>
          <div>
            <input
              id="email"
              type="email"
              value={email}
              onChange={e => setEmail(e.target.value)}
              className="w-full bg-transparent border-0 border-b border-white/70 focus:border-blue-300 focus:ring-0 text-white placeholder-gray-200 py-2 mb-2 transition"
              placeholder="Enter your email"
              required
              autoComplete="username"
            />
          </div>
          <div>
            <div className="relative">
              <input
                id="password"
                type={showPassword ? "text" : "password"}
                value={password}
                onChange={e => setPassword(e.target.value)}
                className="w-full bg-transparent border-0 border-b border-white/70 focus:border-blue-300 focus:ring-0 text-white placeholder-gray-200 py-2 mb-2 transition"
                placeholder="Enter your password"
                required
                autoComplete="current-password"
              />
              <button
                type="button"
                onClick={() => setShowPassword(!showPassword)}
                className="absolute right-2 top-1/2 -translate-y-1/2 text-gray-300 hover:text-white"
                tabIndex={-1}
                aria-label="Show password"
              >
                {showPassword ? "👁️" : "👁️‍🗨️"}
              </button>
            </div>
          </div>
          {error && (
            <div className="bg-red-100 border border-red-300 text-red-700 rounded px-4 py-2 text-sm">
              {error}
            </div>
          )}
          <button
            type="submit"
            className="w-full bg-white text-blue-900 font-semibold py-3 rounded-lg shadow hover:bg-blue-50 transition"
            disabled={loading}
          >
            {loading ? "Đang đăng nhập..." : "Log In"}
          </button>
        </form>
      </div>
    </div>
  );
}