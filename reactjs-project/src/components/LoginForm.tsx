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
    <div className="bg-light min-vh-100 d-flex align-items-center justify-content-center" style={{background: "linear-gradient(135deg, #e9d5ff 0%, #f3e8ff 100%)"}}>
      <div className="container" style={{maxWidth: 400}}>
        <div className="card shadow-lg rounded-4 p-4 border-0">
          <div className="text-center mb-4">
            <div className="bg-gradient p-3 rounded-circle mx-auto mb-3" style={{width: 64, height: 64, background: "linear-gradient(135deg, #a78bfa 0%, #c4b5fd 100%)"}}>
              <span style={{fontSize: 32}}>📦</span>
            </div>
            <h2 className="fw-bold text-primary mb-1" style={{fontSize: 28}}>KTC Logistics</h2>
            <div className="text-muted mb-2">Đăng nhập vào hệ thống</div>
          </div>
          <form onSubmit={handleSubmit}>
            <div className="mb-3">
              <label className="form-label text-primary fw-semibold">Email</label>
              <div className="input-group">
                <span className="input-group-text bg-white border-end-0"><span style={{fontSize: 18}}>✉️</span></span>
                <input
                  type="email"
                  className="form-control border-start-0"
                  placeholder="Nhập email của bạn"
                  value={email}
                  onChange={e => setEmail(e.target.value)}
                  required
                />
              </div>
            </div>
            <div className="mb-3">
              <label className="form-label text-primary fw-semibold">Mật khẩu</label>
              <div className="input-group">
                <span className="input-group-text bg-white border-end-0"><span style={{fontSize: 18}}>🔒</span></span>
                <input
                  type={showPassword ? "text" : "password"}
                  className="form-control border-start-0"
                  placeholder="Nhập mật khẩu"
                  value={password}
                  onChange={e => setPassword(e.target.value)}
                  required
                />
                <button
                  type="button"
                  className="btn btn-outline-secondary"
                  tabIndex={-1}
                  onClick={() => setShowPassword(!showPassword)}
                >
                  {showPassword ? "👁️" : "👁️‍🗨️"}
                </button>
              </div>
            </div>
            {error && (
              <div className="alert alert-danger py-2 px-3 mb-3 d-flex align-items-center gap-2">
                <span>⚠️</span>
                <span>{error}</span>
              </div>
            )}
            <button type="submit" className="btn btn-primary w-100 py-2 fw-bold mb-2" style={{fontSize: 18}}>
              Đăng nhập
            </button>
          </form>
          <div className="text-center mt-3">
          </div>
          
        </div>
        <div className="text-center mt-3 text-muted small">
          © 2025 KTC Logistics. All rights reserved.
        </div>
      </div>
    </div>
  );
}
