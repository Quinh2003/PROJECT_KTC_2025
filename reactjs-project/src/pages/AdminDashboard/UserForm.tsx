
import { useState, useEffect } from "react";
import { createPortal } from "react-dom";
import { FaTools, FaChartBar, FaTruck } from "react-icons/fa";
import { FaBellConcierge } from "react-icons/fa6";

interface UserFormProps {
  onAdd: (user: {
    name: string;
    email: string;
    password: string;
    phone: string;
    role: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    roleIcon: any;
    status: string;
    lastLogin: string;
  }) => void;
  onClose: () => void;
  user?: {
    name: string;
    email: string;
    password?: string;
    phone?: string;
    role: string;
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    roleIcon: any;
    status: string;
    lastLogin: string;
  } | null;
}

const roles = [
  { label: "Dispatcher", value: "Dispatcher", icon: <FaBellConcierge className="inline mr-1" /> },
  { label: "Fleet Manager", value: "Fleet Manager", icon: <FaTools className="inline mr-1" /> },
  { label: "Driver", value: "Driver", icon: <FaTruck className="inline mr-1" /> },
  { label: "Operations Manager", value: "Operations Manager", icon: <FaChartBar className="inline mr-1" /> },
];

export default function UserForm({ onAdd, onClose, user }: UserFormProps) {
  const [name, setName] = useState(user?.name || "");
  const [email, setEmail] = useState(user?.email || "");
  const [password, setPassword] = useState(user?.password || "");
  const [phone, setPhone] = useState(user?.phone || "");
  const [role, setRole] = useState(user?.role || roles[0].value);
  const [status, setStatus] = useState(user?.status || "active");

  useEffect(() => {
    setName(user?.name || "");
    setEmail(user?.email || "");
    setPassword(user?.password || "");
    setPhone(user?.phone || "");
    setRole(user?.role || roles[0].value);
    setStatus(user?.status || "active");
  }, [user]);

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    const selectedRole = roles.find(r => r.value === role) || roles[0];
    onAdd({
      name,
      email,
      password,
      phone,
      role,
      roleIcon: selectedRole.icon,
      status,
      lastLogin: user?.lastLogin || "-",
    });
    onClose();
  };

  return createPortal(
    <div className="fixed inset-0 z-[9999] flex items-center justify-center">
      <div className="absolute inset-0 bg-black/60 backdrop-blur-sm"></div>
      <form
        className="relative z-10 bg-white border border-red-200 shadow-2xl rounded-2xl p-8 w-[380px] space-y-3 overflow-hidden"
        onSubmit={handleSubmit}
      >
        {/* Loang effect inside form */}
        <div className="absolute top-0 left-0 w-[350px] h-[120px] bg-gradient-to-br from-red-100/30 via-red-50/40 to-white/20 rounded-3xl blur-2xl pointer-events-none animate-[loangMove_6s_ease-in-out_infinite]" style={{zIndex:0}}></div>
        <div className="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2 w-[200px] h-[200px] bg-gradient-to-br from-red-50/40 via-white/30 to-red-25/20 rounded-full blur-3xl pointer-events-none animate-[loangFade_8s_ease-in-out_infinite]" style={{zIndex:0}}></div>
                    <h2 className="relative text-xl font-bold mb-2 text-gray-800 drop-shadow-sm z-10">
          {user ? "Edit User" : "Add New User"}
        </h2>
        <div>
          <label className="block mb-1 font-semibold text-gray-700">Full Name</label>
          <input
            className="border border-red-200 bg-white text-gray-700 rounded px-3 py-2 w-full focus:outline-none focus:border-red-500 placeholder:text-gray-400"
            value={name}
            onChange={e => setName(e.target.value)}
            required
            placeholder="Enter full name"
          />
        </div>
        <div>
          <label className="block mb-1 font-semibold text-gray-700">Email</label>
          <input
            className="border border-red-200 bg-white text-gray-700 rounded px-3 py-2 w-full focus:outline-none focus:border-red-500 placeholder:text-gray-400"
            type="email"
            value={email}
            onChange={e => setEmail(e.target.value)}
            required
            disabled={!!user}
            placeholder="Enter email address"
          />
        </div>
        <div>
          <label className="block mb-1 font-semibold text-gray-700">Password</label>
          <input
            className="border border-red-200 bg-white text-gray-700 rounded px-3 py-2 w-full focus:outline-none focus:border-red-500 placeholder:text-gray-400"
            type="password"
            value={password}
            onChange={e => setPassword(e.target.value)}
            required={!user}
            placeholder="Enter password"
          />
        </div>
        <div>
          <label className="block mb-1 font-semibold text-gray-700">Phone</label>
          <input
            className="border border-red-200 bg-white text-gray-700 rounded px-3 py-2 w-full focus:outline-none focus:border-red-500 placeholder:text-gray-400"
            type="tel"
            value={phone}
            onChange={e => setPhone(e.target.value)}
            required
            placeholder="Enter phone number"
          />
        </div>
        <div>
          <label className="block mb-1 font-semibold text-gray-700">Role</label>
          <select
            className="border border-red-200 bg-white text-gray-700 rounded px-3 py-2 w-full focus:outline-none focus:border-red-500"
            value={role}
            onChange={e => setRole(e.target.value)}
          >
            {roles.map(r => (
              <option key={r.value} value={r.value}>
                {r.label}
              </option>
            ))}
          </select>
          <div className="mt-1 flex items-center gap-1 text-gray-600">
            {roles.find(r => r.value === role)?.icon}
            <span>{roles.find(r => r.value === role)?.label}</span>
          </div>
        </div>
        <div>
          <label className="block mb-1 font-semibold text-gray-700">Status</label>
          <select
            className="border border-red-200 bg-white text-gray-700 rounded px-3 py-2 w-full focus:outline-none focus:border-red-500"
            value={status}
            onChange={e => setStatus(e.target.value)}
          >
            <option value="active">Active</option>
            <option value="inactive">Inactive</option>
          </select>
        </div>
        <div className="flex gap-2 justify-end pt-2">
          <button
            type="button"
            className="px-4 py-2 rounded-lg bg-gray-700 text-white font-semibold hover:bg-gray-600 transition"
            onClick={onClose}
          >
            Cancel
          </button>
          <button
            type="submit"
            className="px-4 py-2 rounded-lg bg-gradient-to-br from-red-500 to-red-600 text-white font-bold shadow hover:opacity-90 transition"
          >
            {user ? "Save" : "Add"}
          </button>
        </div>
      </form>
    </div>,
    document.body
  );
}