import { useState } from "react";
import type { User } from "../../types/User";
import UserTable from "./UserTable";

interface AdminDashboardProps {
  user: User;
  onLogout: () => void;
}

const MENU = [
  { key: "overview", label: "Tổng quan", icon: "🏠" },
  { key: "users", label: "Quản lý người dùng", icon: "👤" },
  { key: "roles", label: "Phân quyền vai trò", icon: "🔐" },
  { key: "settings", label: "Cấu hình hệ thống", icon: "⚙️" },
  { key: "logs", label: "Nhật ký hệ thống", icon: "📑" },
];

export default function AdminDashboard({ user, onLogout }: AdminDashboardProps) {
  const [active, setActive] = useState("overview");

  return (
    <div className="min-h-screen flex bg-gradient-to-br from-blue-100 via-white to-teal-100">
      {/* Sidebar */}
      <aside className="w-20 bg-white shadow-lg flex flex-col items-center py-6">
        <div className="mb-8">
          <span className="text-3xl text-teal-600">👑</span>
        </div>
        <nav className="flex flex-col gap-8">
          {MENU.map(item => (
            <button
              key={item.key}
              className={`text-2xl transition ${
                active === item.key
                  ? "text-teal-700 font-bold"
                  : "text-gray-700 hover:text-teal-700"
              }`}
              title={item.label}
              onClick={() => setActive(item.key)}
            >
              <span>{item.icon}</span>
            </button>
          ))}
        </nav>
        <button
          onClick={onLogout}
          className="mt-auto mb-2 px-3 py-2 rounded-lg bg-teal-600 text-white font-bold hover:bg-blue-700 transition"
        >
          Đăng xuất
        </button>
      </aside>
      {/* Main content */}
      <main className="flex-1 p-10">
        <h1 className="text-3xl font-bold text-blue-700 mb-6 flex items-center gap-2">
          Admin Dashboard
        </h1>
        {active === "overview" && (
          <>
            {/* Thống kê tổng quan */}
            <div className="grid grid-cols-2 md:grid-cols-4 gap-6 mb-8">
              <div className="bg-white rounded-xl shadow flex items-center gap-4 p-5">
                <span className="text-3xl text-teal-500">👤</span>
                <div>
                  <div className="text-lg font-bold text-blue-700">120</div>
                  <div className="text-sm text-gray-500">Người dùng</div>
                </div>
              </div>
              <div className="bg-white rounded-xl shadow flex items-center gap-4 p-5">
                <span className="text-3xl text-teal-500">🛡️</span>
                <div>
                  <div className="text-lg font-bold text-blue-700">6</div>
                  <div className="text-sm text-gray-500">Vai trò</div>
                </div>
              </div>
              <div className="bg-white rounded-xl shadow flex items-center gap-4 p-5">
                <span className="text-3xl text-teal-500">⚙️</span>
                <div>
                  <div className="text-lg font-bold text-blue-700">3</div>
                  <div className="text-sm text-gray-500">Cấu hình hệ thống</div>
                </div>
              </div>
              <div className="bg-white rounded-xl shadow flex items-center gap-4 p-5">
                <span className="text-3xl text-teal-500">📑</span>
                <div>
                  <div className="text-lg font-bold text-blue-700">58</div>
                  <div className="text-sm text-gray-500">Nhật ký hoạt động</div>
                </div>
              </div>
            </div>
            <div className="bg-white rounded-xl shadow p-8 mb-8">
              <p className="mb-2 text-teal-700">
                Xin chào <span className="font-semibold">{user.name}</span> (<span className="italic">{user.email}</span>)
              </p>
              <p className="text-gray-500">Chọn chức năng ở sidebar để quản lý hệ thống.</p>
            </div>
          </>
        )}
        {active === "users" && <UserTable />}
        {active === "roles" && (
          <div className="bg-blue-50 rounded-xl p-8 shadow">
            <h3 className="text-xl font-semibold text-blue-600 mb-3">🔐 Phân quyền vai trò</h3>
            <ul className="list-disc ml-5 text-blue-700 space-y-1">
              <li>Dispatcher, Driver, Manager...</li>
              <li>Thiết lập quyền truy cập</li>
            </ul>
            <button className="mt-4 px-4 py-2 rounded-lg bg-teal-600 text-white font-bold hover:bg-blue-700 transition">
              Phân quyền
            </button>
          </div>
        )}
        {active === "settings" && (
          <div className="bg-blue-50 rounded-xl p-8 shadow">
            <h3 className="text-xl font-semibold text-blue-600 mb-3">⚙️ Cấu hình hệ thống</h3>
            <ul className="list-disc ml-5 text-blue-700 space-y-1">
              <li>Cấu hình API, IoT, lịch định kỳ</li>
              <li>Thiết lập thông số hệ thống</li>
            </ul>
            <button className="mt-4 px-4 py-2 rounded-lg bg-teal-600 text-white font-bold hover:bg-blue-700 transition">
              Cấu hình hệ thống
            </button>
          </div>
        )}
        {active === "logs" && (
          <div className="bg-blue-50 rounded-xl p-8 shadow">
            <h3 className="text-xl font-semibold text-blue-600 mb-3">📑 Nhật ký hệ thống</h3>
            <ul className="list-disc ml-5 text-blue-700 space-y-1">
              <li>Theo dõi hoạt động (logs)</li>
              <li>Kiểm tra lịch sử thao tác</li>
            </ul>
            <button className="mt-4 px-4 py-2 rounded-lg bg-teal-600 text-white font-bold hover:bg-blue-700 transition">
              Xem nhật ký
            </button>
          </div>
        )}
      </main>
    </div>
  );
}