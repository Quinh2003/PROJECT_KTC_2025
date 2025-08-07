import { useState, useEffect } from "react";

interface UserFormProps {
  onAdd: (user: {
    name: string;
    email: string;
    role: string;
    roleIcon: string;
    status: string;
    lastLogin: string;
  }) => void;
  onClose: () => void;
  user?: {
    name: string;
    email: string;
    role: string;
    roleIcon: string;
    status: string;
    lastLogin: string;
  } | null;
}

const roles = [
  { label: "Dispatcher", value: "Dispatcher", icon: "🛎️" },
  { label: "Fleet Manager", value: "Fleet Manager", icon: "🛠️" },
  { label: "Driver", value: "Driver", icon: "🚚" },
];

export default function UserForm({ onAdd, onClose, user }: UserFormProps) {
  const [name, setName] = useState(user?.name || "");
  const [email, setEmail] = useState(user?.email || "");
  const [role, setRole] = useState(user?.role || roles[0].value);
  const [status, setStatus] = useState(user?.status || "active");

  // Khi user thay đổi (bấm edit user khác), cập nhật lại form
  useEffect(() => {
    setName(user?.name || "");
    setEmail(user?.email || "");
    setRole(user?.role || roles[0].value);
    setStatus(user?.status || "active");
  }, [user]);

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    const icon =
      role === "Dispatcher"
        ? "🛎️"
        : role === "Fleet Manager"
        ? "🛠️"
        : "🚚";
    onAdd({
      name,
      email,
      role,
      roleIcon: icon,
      status,
      lastLogin: user?.lastLogin || "-", // giữ nguyên lastLogin nếu edit
    });
    onClose();
  };

  return (
    <div className="fixed inset-0 bg-black/30 flex items-center justify-center z-50">
      <form
        className="bg-white rounded-xl shadow-lg p-8 w-full max-w-md space-y-4"
        onSubmit={handleSubmit}
      >
        <h2 className="text-xl font-bold mb-2">
          {user ? "Chỉnh sửa người dùng" : "Thêm người dùng mới"}
        </h2>
        <div>
          <label className="block mb-1 font-semibold">Họ tên</label>
          <input
            className="border rounded px-3 py-2 w-full"
            value={name}
            onChange={e => setName(e.target.value)}
            required
          />
        </div>
        <div>
          <label className="block mb-1 font-semibold">Email</label>
          <input
            className="border rounded px-3 py-2 w-full"
            type="email"
            value={email}
            onChange={e => setEmail(e.target.value)}
            required
            disabled={!!user} // Không cho sửa email khi edit
          />
        </div>
        <div>
          <label className="block mb-1 font-semibold">Vai trò</label>
          <select
            className="border rounded px-3 py-2 w-full"
            value={role}
            onChange={e => setRole(e.target.value)}
          >
            {roles.map(r => (
              <option key={r.value} value={r.value}>
                {r.icon} {r.label}
              </option>
            ))}
          </select>
        </div>
        <div>
          <label className="block mb-1 font-semibold">Trạng thái</label>
          <select
            className="border rounded px-3 py-2 w-full"
            value={status}
            onChange={e => setStatus(e.target.value)}
          >
            <option value="active">Hoạt động</option>
            <option value="inactive">Không hoạt động</option>
          </select>
        </div>
        <div className="flex gap-2 justify-end pt-2">
          <button
            type="button"
            className="px-4 py-2 rounded bg-gray-200 hover:bg-gray-300"
            onClick={onClose}
          >
            Hủy
          </button>
          <button
            type="submit"
            className="px-4 py-2 rounded bg-teal-600 text-white font-bold hover:bg-blue-700"
          >
            {user ? "Lưu" : "Thêm"}
          </button>
        </div>
      </form>
    </div>
  );
}