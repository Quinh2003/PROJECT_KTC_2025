
import { useState, useEffect } from "react";
import { FaTools, FaChartBar, FaTruck } from "react-icons/fa";
import { FaBellConcierge } from "react-icons/fa6";
import type { User } from "../../types/dashboard";

interface UserFormProps {
  onAdd: (user: User & { roleIcon: React.ReactNode }) => void;
  onClose: () => void;
  user?: (User & { roleIcon: React.ReactNode }) | null;
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
  const [role, setRole] = useState(user?.role || roles[0].value);
  const [status, setStatus] = useState(user?.status || "active");

  useEffect(() => {
    setName(user?.name || "");
    setEmail(user?.email || "");
    setRole(user?.role || roles[0].value);
    setStatus(user?.status || "active");
  }, [user]);

  const handleSubmit = (e: React.FormEvent) => {
    e.preventDefault();
    const selectedRole = roles.find(r => r.value === role) || roles[0];
    onAdd({
      id: user?.id || "",
      name,
      email,
      role,
      roleIcon: selectedRole.icon,
      status,
      lastLogin: user?.lastLogin || "-",
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
            {roles.find(r => r.value === role)?.icon}
            <span className="ml-1">{roles.find(r => r.value === role)?.label}</span>
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