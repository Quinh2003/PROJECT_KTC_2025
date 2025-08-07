import { useState } from "react";
import UserForm from "./UserForm";

const initialUsers = [
  {
    name: "Nguyễn Văn A",
    email: "nguyenvana@company.com",
    role: "Dispatcher",
    roleIcon: "🛎️",
    status: "active",
    lastLogin: "2024-01-15 09:30",
  },
  {
    name: "Trần Thị B",
    email: "tranthib@company.com",
    role: "Fleet Manager",
    roleIcon: "🛠️",
    status: "active",
    lastLogin: "2024-01-15 08:45",
  },
  {
    name: "Lê Văn C",
    email: "levanc@company.com",
    role: "Driver",
    roleIcon: "🚚",
    status: "inactive",
    lastLogin: "2024-01-10 16:20",
  },
];

export default function UserTable() {
  const [search, setSearch] = useState("");
  const [users, setUsers] = useState(initialUsers);
  const [showForm, setShowForm] = useState(false);
  const [editUser, setEditUser] = useState<{
    name: string;
    email: string;
    role: string;
    roleIcon: string;
    status: string;
    lastLogin: string;
  } | null>(null);
  const [viewUser, setViewUser] = useState<{
    name: string;
    email: string;
    role: string;
    roleIcon: string;
    status: string;
    lastLogin: string;
  } | null>(null);

  const filtered = users.filter(
    (u) =>
      u.name.toLowerCase().includes(search.toLowerCase()) ||
      u.email.toLowerCase().includes(search.toLowerCase())
  );

  const handleAddUser = (user: { name: string; email: string; role: string; roleIcon: string; status: string; lastLogin: string; }) => {
    setUsers([...users, user]);
  };

  const handleEditUser = (user: { name: string; email: string; role: string; roleIcon: string; status: string; lastLogin: string; }) => {
    setEditUser(user);
    setShowForm(true);
  };

  const handleUpdateUser = (updatedUser: { name: string; email: string; role: string; roleIcon: string; status: string; lastLogin: string; }) => {
    setUsers(users.map(u => (u.email === updatedUser.email ? updatedUser : u)));
    setShowForm(false);
    setEditUser(null);
  };

  const handleDeleteUser = (email: string) => {
    if (window.confirm("Bạn có chắc muốn xoá người dùng này?")) {
      setUsers(users.filter(u => u.email !== email));
    }
  };

  return (
    <div className="bg-white rounded-xl shadow p-6">
      <div className="flex justify-between items-center mb-4">
        <input
          className="border rounded px-4 py-2 w-72"
          placeholder="Tìm kiếm người dùng..."
          value={search}
          onChange={(e) => setSearch(e.target.value)}
        />
        <button
          className="bg-black text-white px-4 py-2 rounded font-bold flex items-center gap-2 hover:bg-gray-800"
          onClick={() => {
            setShowForm(true);
            setEditUser(null);
          }}
        >
          <span className="text-xl">+</span> Thêm người dùng
        </button>
      </div>
      {showForm && (
        <UserForm
          onAdd={editUser ? handleUpdateUser : handleAddUser}
          onClose={() => {
            setShowForm(false);
            setEditUser(null);
          }}
          user={editUser}
        />
      )}
      <div>
        <h2 className="text-2xl font-bold mb-5">Danh sách người dùng</h2>
        <div className="overflow-x-auto">
          <table className="min-w-full">
            <thead>
              <tr className="text-left text-gray-600 border-b">
                <th className="py-2 pr-4">Người dùng</th>
                <th className="py-2 pr-4">Email</th>
                <th className="py-2 pr-4">Vai trò</th>
                <th className="py-2 pr-4">Trạng thái</th>
                <th className="py-2 pr-4">Đăng nhập cuối</th>
                <th className="py-2 pr-4">Thao tác</th>
              </tr>
            </thead>
            <tbody>
              {filtered.map((u, idx) => (
                <tr key={idx} className="border-b hover:bg-gray-50">
                  <td className="py-3 pr-4 font-medium">{u.name}</td>
                  <td className="py-3 pr-4">{u.email}</td>
                  <td className="py-3 pr-4">
                    <span
                      className={`inline-flex items-center gap-1 px-3 py-1 rounded-full text-sm font-semibold ${
                        u.role === "Dispatcher"
                          ? "bg-black text-white"
                          : u.role === "Fleet Manager"
                          ? "bg-gray-100 text-black"
                          : "bg-yellow-50 text-black border"
                      }`}
                    >
                      <span>{u.roleIcon}</span>
                      {u.role}
                    </span>
                  </td>
                  <td className="py-3 pr-4">
                    {u.status === "active" ? (
                      <span className="inline-flex items-center px-6 py-2 rounded-full bg-[#22c55e] text-white text-base font-semibold gap-2 shadow-sm">
                        <span className="w-4 h-4 rounded-full bg-white flex items-center justify-center">
                          <span className="w-2.5 h-2.5 rounded-full bg-[#22c55e] block"></span>
                        </span>
                        Hoạt động
                      </span>
                    ) : (
                      <span className="inline-flex items-center px-6 py-2 rounded-full bg-gray-100 text-gray-700 text-base font-semibold gap-2 shadow-sm">
                        <span className="w-4 h-4 rounded-full bg-white flex items-center justify-center">
                          <span className="w-2.5 h-2.5 rounded-full bg-gray-400 block"></span>
                        </span>
                        Không hoạt động
                      </span>
                    )}
                  </td>
                  <td className="py-3 pr-4">{u.lastLogin}</td>
                  <td className="py-3 pr-4 flex gap-2">
                    <button
                      className="p-2 rounded hover:bg-gray-200"
                      title="Sửa"
                      onClick={() => handleEditUser(u)}
                    >
                      <svg width="20" height="20" fill="none">
                        <path
                          d="M4 13.5V16h2.5l7.1-7.1-2.5-2.5L4 13.5z"
                          stroke="#222"
                          strokeWidth="1.5"
                          strokeLinecap="round"
                          strokeLinejoin="round"
                        />
                        <path
                          d="M14.7 6.3a1 1 0 0 0 0-1.4l-1.6-1.6a1 1 0 0 0-1.4 0l-1.1 1.1 3 3 1.1-1.1z"
                          stroke="#222"
                          strokeWidth="1.5"
                          strokeLinecap="round"
                          strokeLinejoin="round"
                        />
                      </svg>
                    </button>
                    <button
                      className="p-2 rounded hover:bg-gray-200"
                      title="Xem"
                      onClick={() => setViewUser(u)}
                    >
                      <svg width="20" height="20" fill="none">
                        <circle
                          cx="10"
                          cy="10"
                          r="8"
                          stroke="#222"
                          strokeWidth="1.5"
                        />
                        <circle
                          cx="10"
                          cy="10"
                          r="3"
                          stroke="#222"
                          strokeWidth="1.5"
                        />
                      </svg>
                    </button>
                    <button
                      className="p-2 rounded hover:bg-red-100"
                      title="Xoá"
                      onClick={() => handleDeleteUser(u.email)}
                    >
                      <svg width="20" height="20" fill="none">
                        <rect
                          x="5"
                          y="7"
                          width="10"
                          height="8"
                          rx="2"
                          stroke="#ef4444"
                          strokeWidth="1.5"
                        />
                        <path
                          d="M8 7V5a2 2 0 0 1 4 0v2"
                          stroke="#ef4444"
                          strokeWidth="1.5"
                        />
                      </svg>
                    </button>
                  </td>
                </tr>
              ))}
              {filtered.length === 0 && (
                <tr>
                  <td colSpan={6} className="py-6 text-center text-gray-400">
                    Không có người dùng nào.
                  </td>
                </tr>
              )}
            </tbody>
          </table>
        </div>
      </div>
      {/* Modal xem chi tiết người dùng */}
      {viewUser && (
        <div className="fixed inset-0 bg-black/30 flex items-center justify-center z-50">
          <div className="bg-white rounded-xl shadow-lg p-8 w-full max-w-md">
            <h2 className="text-xl font-bold mb-2">Thông tin người dùng</h2>
            <div><b>Họ tên:</b> {viewUser.name}</div>
            <div><b>Email:</b> {viewUser.email}</div>
            <div><b>Vai trò:</b> {viewUser.role}</div>
            <div><b>Trạng thái:</b> {viewUser.status === "active" ? "Hoạt động" : "Không hoạt động"}</div>
            <div><b>Đăng nhập cuối:</b> {viewUser.lastLogin}</div>
            <button className="mt-4 px-4 py-2 rounded bg-teal-600 text-white" onClick={() => setViewUser(null)}>
              Đóng
            </button>
          </div>
        </div>
      )}
    </div>
  );
}