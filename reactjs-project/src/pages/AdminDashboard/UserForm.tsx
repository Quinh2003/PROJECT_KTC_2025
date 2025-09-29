import { useState, useEffect } from "react";

// Định nghĩa User interface cho UserForm
interface User {
  id?: number;
  name: string;
  email: string;
  role: string;
  status: string;
  lastLogin: string;
  phone?: string;
  password?: string;
}

// Có thể import icons hoặc sử dụng Unicode
// import { FaEye, FaEyeSlash } from "react-icons/fa";

interface UserFormProps {
  onAdd: (user: User & { roleIcon: React.ReactNode }) => void;
  onClose: () => void;
  user?: (User & { roleIcon: React.ReactNode }) | null;
}

const roles = [
  { label: "Admin", value: "Admin", icon: null },
  { label: "Dispatcher", value: "Dispatcher", icon: null },
  { label: "Fleet Manager", value: "Fleet Manager", icon: null },
  { label: "Driver", value: "Driver", icon: null },
  { label: "Operations Manager", value: "Operations Manager", icon: null },
  { label: "Customer", value: "Customer", icon: null },
];

export default function UserForm({ onAdd, onClose, user }: UserFormProps) {
  const [name, setName] = useState(user?.name || "");
  const [email, setEmail] = useState(user?.email || "");
  const [emailError, setEmailError] = useState("");
  const [phoneError, setPhoneError] = useState("");
  const [role, setRole] = useState(user?.role || roles[0].value);
  const [status, setStatus] = useState(
    user?.status === "inactive" 
      ? "inactive" 
      : user?.status === "suspended" 
        ? "suspended" 
        : "active"
  );
  const [password, setPassword] = useState(user?.password || "");
  const [phone, setPhone] = useState(user?.phone || "");

  // Format số điện thoại
  const formatPhoneNumber = (value: string) => {
    // Chỉ giữ lại số
    const phoneNumber = value.replace(/[^\d]/g, '');
    
    // Format theo pattern: 0123 456 789
    if (phoneNumber.length <= 4) {
      return phoneNumber;
    } else if (phoneNumber.length <= 7) {
      return `${phoneNumber.slice(0, 4)} ${phoneNumber.slice(4)}`;
    } else {
      return `${phoneNumber.slice(0, 4)} ${phoneNumber.slice(4, 7)} ${phoneNumber.slice(7, 10)}`;
    }
  };

  useEffect(() => {
    setName(user?.name || "");
    setEmail(user?.email || "");
    setRole(user?.role || roles[0].value);
    
    // Fix status mapping
    if (user?.status === "inactive") {
      setStatus("inactive");
    } else if (user?.status === "suspended") {
      setStatus("suspended");
    } else {
      setStatus("active");
    }
    
    setPassword(user?.password || "");
    
    // Format số điện thoại khi load
    if (user?.phone) {
      setPhone(formatPhoneNumber(user.phone));
    } else {
      setPhone("");
    }
    
    setEmailError("");
    setPhoneError("");
  }, [user]);

  // Kiểm tra email
  const handleEmailChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const value = e.target.value;
    setEmail(value);
    
    if (value && !value.includes('@')) {
      setEmailError(`Vui lòng bao gồm '@' trong địa chỉ email. '${value}' bị thiếu '@'.`);
    } else {
      setEmailError("");
    }
  };

  // Xử lý số điện thoại
  const handlePhoneChange = (e: React.ChangeEvent<HTMLInputElement>) => {
  const rawValue = e.target.value;
  
  // Chỉ giữ lại số từ input
  const phoneDigits = rawValue.replace(/[^\d]/g, '');
  
  // Giới hạn ở 10 chữ số
  const limitedDigits = phoneDigits.slice(0, 10);
  
  // Format theo pattern: 0123 456 789
  let formattedValue = '';
  if (limitedDigits.length <= 4) {
    formattedValue = limitedDigits;
  } else if (limitedDigits.length <= 7) {
    formattedValue = `${limitedDigits.slice(0, 4)} ${limitedDigits.slice(4)}`;
  } else {
    formattedValue = `${limitedDigits.slice(0, 4)} ${limitedDigits.slice(4, 7)} ${limitedDigits.slice(7)}`;
  }
  
  // Luôn cập nhật state phone để có thể nhập từng chữ số
  setPhone(formattedValue);
  
  // Hiển thị lỗi nếu không đủ 10 chữ số
  if (formattedValue.trim() !== "" && limitedDigits.length !== 10) {
    setPhoneError("Số điện thoại phải đúng 10 chữ số!");
  } else {
    setPhoneError("");
  }
};

// Sửa validate trong handleSubmit
const handleSubmit = (e: React.FormEvent) => {
  e.preventDefault();
  
  // Validate phone required
  const phoneDigits = phone.replace(/[^\d]/g, '');
  if (!phone.trim()) {
    setPhoneError("Số điện thoại không được để trống!");
    return;
  } else if (phoneDigits.length !== 10) {
    setPhoneError("Số điện thoại phải đúng 10 chữ số!");
    return;
  }
    
    // Validate email
    if (!email.includes('@')) {
      setEmailError(`Vui lòng bao gồm '@' trong địa chỉ email. '${email}' bị thiếu '@'.`);
      return;
    }
    
    // Kiểm tra lỗi trước khi submit
    if (emailError || phoneError) {
      return;
    }
    
    console.log("[UserForm] handleSubmit with role:", role);
    const selectedRole = roles.find(r => r.value === role) || roles[0];
    
    // Lưu số điện thoại chỉ là số, bỏ định dạng khoảng trắng
    const cleanPhone = phone.replace(/\s/g, '');
    
    const userToSubmit = {
      id: user?.id || undefined,
      name,
      email,
      role,
      roleIcon: selectedRole.icon,
      status,
      lastLogin: user?.lastLogin || "-",
      password,
      phone: cleanPhone,
    };
    console.log("[UserForm] Submitting user:", userToSubmit);
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
            className={`border rounded px-3 py-2 w-full ${phoneError ? 'border-red-500' : ''}`}
            type="text"
            value={phone}
            onChange={handlePhoneChange}
            placeholder="0123 456 789"
            maxLength={12}
            required
          />
          {phoneError && (
            <div className="bg-orange-100 border border-orange-400 text-orange-700 px-3 py-2 rounded mt-2 text-sm flex items-center">
              <span className="text-orange-500 mr-2">⚠</span>
              {phoneError}
            </div>
          )}
        </div>
        <div>
          <label className="block mb-1 font-semibold">Email</label>
          <input
            className={`border rounded px-3 py-2 w-full ${emailError ? 'border-red-500' : ''}`}
            type="text"
            value={email}
            onChange={handleEmailChange}
            required
            disabled={!!user}
          />
          {emailError && (
            <div className="bg-orange-100 border border-orange-400 text-orange-700 px-3 py-2 rounded mt-2 text-sm flex items-center">
              <span className="text-orange-500 mr-2">⚠</span>
              {emailError}
            </div>
          )}
        </div>
        <div>
          <label className="block mb-1 font-semibold">Password</label>
          <div className="relative">
            <input
              className="border rounded px-3 py-2 w-full pr-10"
              type="text"
              value={password}
              onChange={e => setPassword(e.target.value)}
              required
            />
          </div>
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
        </div>
        {/* <div>
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
        </div> */}
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