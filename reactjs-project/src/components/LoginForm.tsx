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
  const [showDemo, setShowDemo] = useState(false);
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
            <button
              type="button"
              className="btn btn-link text-decoration-none text-primary"
              onClick={() => setShowDemo(!showDemo)}
            >
              {showDemo ? "Ẩn tài khoản demo" : "Quên mật khẩu?"}
            </button>
            <div className="text-muted">
              Chưa có tài khoản?{" "}
              <button
                type="button"
                className="btn btn-link text-decoration-none text-primary fw-semibold"
                onClick={() => setShowDemo(!showDemo)}
              >
                Đăng ký ngay
              </button>
            </div>
          </div>
          {showDemo && (
            <div className="mt-4 pt-3 border-top">
              <div className="fw-semibold text-secondary mb-2 text-center">Tài khoản demo</div>
              <div className="d-grid gap-2">
                {demoAccounts.map((acc, idx) => (
                  <button
                    key={idx}
                    type="button"
                    className="btn btn-light border d-flex justify-content-between align-items-center"
                    onClick={() => handleDemoClick(acc)}
                  >
                    <div>
                      <div className="fw-semibold">{acc.name}</div>
                      <div className="text-muted small">{acc.email}</div>
                    </div>
                    <span className="badge bg-primary">{acc.role.replace("_", " ")}</span>
                  </button>
                ))}
              </div>
            </div>
          )}
        </div>
        <div className="text-center mt-3 text-muted small">
          © 2025 KTC Logistics. All rights reserved.
        </div>
      </div>
    </div>
  );
}