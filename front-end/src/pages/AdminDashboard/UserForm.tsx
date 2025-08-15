import { useState, useEffect } from "react";
import type { User } from "../../types/User";

interface UserFormProps {
  onAdd: (user: User) => void;
  onClose: () => void;
  user?: User | null;
}

const roles = [
  { label: "ADMIN", value: "ADMIN" },
  { label: "DISPATCHER", value: "DISPATCHER" },
  { label: "FLEET", value: "FLEET" },
  { label: "DRIVER", value: "DRIVER" },
  { label: "OPERATIONS", value: "OPERATIONS" },
  { label: "CUSTOMER", value: "CUSTOMER" },
];

export default function UserForm({ onAdd, onClose, user }: UserFormProps) {
  const [phoneError, setPhoneError] = useState("");
  const [name, setName] = useState(user?.name || "");
  const [email, setEmail] = useState(user?.email || "");
  const [role, setRole] = useState(user?.role || roles[0].value);
  const [status, setStatus] = useState(
    user?.status === "inactive" ? "inactive" : "active"
  );
  const [password, setPassword] = useState(user?.password || "");
  const [phone, setPhone] = useState(user?.phone || "");

  // Debug password issue
  console.log("UserForm - user:", user);
  console.log("UserForm - user?.password:", user?.password);
  console.log("UserForm - password state:", password);

  useEffect(() => {
    console.log("useEffect - user:", user);
    console.log("useEffect - user?.password:", user?.password);
    setName(user?.name || "");
    setEmail(user?.email || "");
    setRole(user?.role || roles[0].value);
    setStatus(user?.status === "inactive" ? "inactive" : "active");
    // Hiển thị password từ API
    setPassword(user?.password || "");
    setPhone(user?.phone || "");
  }, [user]);

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    // Kiểm tra định dạng số điện thoại: chỉ cho phép số, tối thiểu 8 ký tự, tối đa 15 ký tự
    const phoneRegex = /^\d{8,15}$/;
    if (phone && !phoneRegex.test(phone)) {
      setPhoneError("Số điện thoại phải là số và từ 8 đến 15 ký tự!");
      return;
    } else {
      setPhoneError("");
    }
    const userToSubmit: User = {
      id: user?.id || 0,
      name,
      email,
      role,
      status,
      lastLogin: user?.lastLogin || "-",
      password,
      phone,
    };
    onAdd(userToSubmit);
    onClose();
  };

  return (
    <div className="fixed inset-0 bg-black/30 flex items-center justify-center z-50">
      <form
        className="bg-white rounded-xl shadow-lg p-8 w-full max-w-md space-y-4"
        onSubmit={handleSubmit}
      >
        <h2 className="text-xl font-bold mb-2">
          {user ? "Edit User" : "Add New User"}
        </h2>
        <div>
          <label className="block mb-1 font-semibold">Full Name</label>
          <input
            className="border rounded px-3 py-2 w-full"
            value={name}
            onChange={e => setName(e.target.value)}
            required
          />
        </div>
        <div>
          <label className="block mb-1 font-semibold">Phone</label>
          <input
            className="border rounded px-3 py-2 w-full"
            type="text"
            value={phone}
            onChange={e => setPhone(e.target.value)}
            pattern="\d{8,15}"
            title="Số điện thoại phải là số và từ 8 đến 15 ký tự"
          />
          {phoneError && (
            <div className="text-red-500 text-sm mt-1">{phoneError}</div>
          )}
        </div>
        <div>
          <label className="block mb-1 font-semibold">Email</label>
          <input
            className="border rounded px-3 py-2 w-full"
            type="email"
            value={email}
            onChange={e => setEmail(e.target.value)}
            required
            disabled={!!user}
          />
        </div>
        <div>
          <label className="block mb-1 font-semibold">Password</label>
          <input
            className="border rounded px-3 py-2 w-full"
            type="text"
            value={password}
            onChange={e => setPassword(e.target.value)}
            placeholder={user ? "Enter new password or keep current" : "Enter password"}
            required
          />
        </div>
        <div>
          <label className="block mb-1 font-semibold">Role</label>
          <select
            className="border rounded px-3 py-2 w-full"
            value={role}
            onChange={e => setRole(e.target.value)}
          >
            {roles.map(r => (
              <option key={r.value} value={r.value}>
                {r.label}
              </option>
            ))}
          </select>
          <div className="mt-1">
            <span>{roles.find(r => r.value === role)?.label}</span>
          </div>
        </div>
        <div>
          <label className="block mb-1 font-semibold">Status</label>
          <select
            className="border rounded px-3 py-2 w-full"
            value={status}
            onChange={e => setStatus(e.target.value)}
          >
            <option value="active">Active</option>
            <option value="inactive">Inactive</option>
            <option value="suspended">Suspended</option>
          </select>
        </div>
        <div className="flex gap-2 justify-end pt-2">
          <button
            type="button"
            className="px-4 py-2 rounded bg-gray-200 hover:bg-gray-300"
            onClick={onClose}
          >
            Cancel
          </button>
          <button
            type="submit"
            className="px-4 py-2 rounded bg-teal-600 text-white font-bold hover:bg-blue-700"
          >
            {user ? "Save" : "Add"}
          </button>
        </div>
      </form>
    </div>
  );
}