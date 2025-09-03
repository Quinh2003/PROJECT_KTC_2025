import { useState } from "react";
import Link from "next/link";
import { registerUserApi } from "../../server/user.api";
import { loginApi } from "../../server/auth.api";
import { FaLock, FaLockOpen } from "react-icons/fa";
import { AuthResponse } from "../../types/User";

// Validation functions cho registration (chi tiết hơn login)
const validateEmail = (email: string) => {
  if (!email.trim()) return "Email is required";
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  if (!emailRegex.test(email)) return "Please enter a valid email address";
  if (email.length > 100) return "Email must be less than 100 characters";
  return "";
};

const validatePassword = (password: string) => {
  if (!password.trim()) return "Password is required";
  if (password.length < 6) return "Password must be at least 6 characters long";
  if (password.length > 50) return "Password must be less than 50 characters";
  if (!/(?=.*[a-z])/.test(password)) return "Password must contain at least one lowercase letter";
  if (!/(?=.*[A-Z])/.test(password)) return "Password must contain at least one uppercase letter";
  if (!/(?=.*\d)/.test(password)) return "Password must contain at least one number";
  return "";
};

const validateConfirmPassword = (password: string, confirmPassword: string) => {
  if (!confirmPassword.trim()) return "Please confirm your password";
  if (password !== confirmPassword) return "Passwords do not match";
  return "";
};

const validateFullName = (fullName: string) => {
  if (!fullName.trim()) return "Full name is required";
  if (fullName.trim().length < 2) return "Full name must be at least 2 characters long";
  if (fullName.length > 50) return "Full name must be less than 50 characters";
  if (!/^[a-zA-Z\u00C0-\u024F\u1E00-\u1EFF\s]+$/.test(fullName.trim())) {
    return "Full name can only contain letters and spaces";
  }
  return "";
};

const validatePhone = (phone: string) => {
  if (!phone.trim()) return "Phone number is required";
  // Remove all non-digit characters for validation
  const cleanPhone = phone.replace(/\D/g, '');
  if (cleanPhone.length < 10) return "Phone number must be at least 10 digits";
  
  // Vietnamese phone number pattern (basic) - chỉ kiểm tra format, không giới hạn tối đa
  if (!/^(0|\+84)[0-9]{9,}$/.test(phone.trim())) {
    return "Please enter a valid phone number (e.g., 0912345678 or +84912345678)";
  }
  return "";
};

interface RegisterFormProps {
  onRegister: (response: AuthResponse) => void;
}

export default function RegisterForm({ onRegister }: RegisterFormProps) {
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [confirmPassword, setConfirmPassword] = useState("");
  const [fullName, setFullName] = useState("");
  const [phone, setPhone] = useState("");
  const [error, setError] = useState("");
  const [loading, setLoading] = useState(false);
  const [showPassword, setShowPassword] = useState(false);
  const [showConfirmPassword, setShowConfirmPassword] = useState(false);
  
  // Validation error states
  const [emailError, setEmailError] = useState("");
  const [passwordError, setPasswordError] = useState("");
  const [confirmPasswordError, setConfirmPasswordError] = useState("");
  const [fullNameError, setFullNameError] = useState("");
  const [phoneError, setPhoneError] = useState("");

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError("");
    setEmailError("");
    setPasswordError("");
    setConfirmPasswordError("");
    setFullNameError("");
    setPhoneError("");
    
    // Validate all fields
    const emailErr = validateEmail(email);
    const passwordErr = validatePassword(password);
    const confirmPasswordErr = validateConfirmPassword(password, confirmPassword);
    const fullNameErr = validateFullName(fullName);
    const phoneErr = validatePhone(phone);
    
    // Set error states
    if (emailErr) setEmailError(emailErr);
    if (passwordErr) setPasswordError(passwordErr);
    if (confirmPasswordErr) setConfirmPasswordError(confirmPasswordErr);
    if (fullNameErr) setFullNameError(fullNameErr);
    if (phoneErr) setPhoneError(phoneErr);
    
    // If any validation errors, stop submission
    if (emailErr || passwordErr || confirmPasswordErr || fullNameErr || phoneErr) {
      return;
    }
    
    setLoading(true);
    try {
      const res = await registerUserApi(email, password, fullName, phone);
      if (!res.ok) {
        const errorData = await res.json();
        setError(errorData.error || "Registration failed!");
        return;
      }
      // Register successful, now auto-login to get token
      const loginRes = await loginApi(email, password);
      if (!loginRes.ok) {
        const loginError = await loginRes.text();
        console.error("Login error:", loginError);
        setError("Registration successful but login failed!");
        return;
      }
      const loginData = await loginRes.json();
      // Check if user is customer - fix role checking
      const userRole = loginData.user?.role?.roleName?.toLowerCase() || 
                      loginData.user?.role?.toLowerCase() ||
                      loginData.role?.roleName?.toLowerCase() ||
                      loginData.role?.toLowerCase();
      if (userRole !== "customer") {
        setError("This account does not have access permission!");
        return;
      }
      onRegister(loginData);
    } catch {
      setError("Unable to connect to server!");
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="w-full bg-white/10 backdrop-blur-xl rounded-3xl border border-white/20 shadow-2xl px-8 py-10 space-y-6">
      <div className="text-center mb-8">
        <h2 className="text-2xl font-semibold text-white drop-shadow-lg">
          Welcome to FastRoute
        </h2>
        <p className="text-white/70 text-sm mt-2">
          Create an account to use our services
        </p>
      </div>

      <form onSubmit={handleSubmit} className="space-y-6">
        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          <div className="space-y-2">
            <label htmlFor="fullName" className="block text-white/90 text-sm font-medium drop-shadow">
              Full Name
            </label>
            <input
              id="fullName"
              type="text"
              value={fullName}
              onChange={e => {
                setFullName(e.target.value);
                if (fullNameError) setFullNameError("");
              }}
              onBlur={e => {
                const err = validateFullName(e.target.value);
                setFullNameError(err);
              }}
              className={`w-full bg-transparent border ${fullNameError ? 'border-red-400' : 'border-white/20'} rounded-xl px-4 py-3 text-white placeholder-white/60 focus:outline-none focus:ring-2 ${fullNameError ? 'focus:ring-red-400/40 focus:border-red-400/40' : 'focus:ring-blue-400/40 focus:border-blue-400/40'} transition-all duration-300`}
              placeholder="Enter full name"
              required
            />
            {fullNameError && (
              <p className="text-red-300 text-sm mt-1 flex items-center gap-1">
                <span>⚠️</span>
                {fullNameError}
              </p>
            )}
          </div>
          <div className="space-y-2">
            <label htmlFor="email" className="block text-white/90 text-sm font-medium drop-shadow">
              Email
            </label>
            <input
              id="email"
              type="email"
              value={email}
              onChange={e => {
                setEmail(e.target.value);
                if (emailError) setEmailError("");
              }}
              onBlur={e => {
                const err = validateEmail(e.target.value);
                setEmailError(err);
              }}
              className={`w-full bg-transparent border ${emailError ? 'border-red-400' : 'border-white/20'} rounded-xl px-4 py-3 text-white placeholder-white/60 focus:outline-none focus:ring-2 ${emailError ? 'focus:ring-red-400/40 focus:border-red-400/40' : 'focus:ring-blue-400/40 focus:border-blue-400/40'} transition-all duration-300`}
              placeholder="Enter email"
              required
              autoComplete="username"
            />
            {emailError && (
              <p className="text-red-300 text-sm mt-1 flex items-center gap-1">
                <span>⚠️</span>
                {emailError}
              </p>
            )}
          </div>
          <div className="space-y-2">
            <label htmlFor="password" className="block text-white/90 text-sm font-medium drop-shadow">
              Password
            </label>
            <div className="relative group">
              <input
                id="password"
                type={showPassword ? "text" : "password"}
                value={password}
                onChange={e => {
                  setPassword(e.target.value);
                  if (passwordError) setPasswordError("");
                  // Also check confirm password when password changes
                  if (confirmPassword && confirmPasswordError) {
                    const confirmErr = validateConfirmPassword(e.target.value, confirmPassword);
                    setConfirmPasswordError(confirmErr);
                  }
                }}
                onBlur={e => {
                  const err = validatePassword(e.target.value);
                  setPasswordError(err);
                }}
                className={`w-full bg-transparent border ${passwordError ? 'border-red-400' : 'border-white/20'} rounded-xl px-4 py-3 pr-12 text-white placeholder-white/60 focus:outline-none focus:ring-2 ${passwordError ? 'focus:ring-red-400/40 focus:border-red-400/40' : 'focus:ring-blue-400/40 focus:border-blue-400/40'} transition-all duration-300`}
                placeholder="Enter password"
                required
                autoComplete="new-password"
              />
              <button
                type="button"
                onClick={() => setShowPassword(!showPassword)}
                className="absolute inset-y-0 right-0 flex items-center pr-3 text-white/80 hover:text-blue-400 transition-colors duration-200"
                tabIndex={-1}
                aria-label="Show/Hide password"
              >
                {showPassword ? <FaLockOpen className="w-3 h-3" /> : <FaLock className="w-3 h-3" />}
              </button>
            </div>
            {passwordError && (
              <p className="text-red-300 text-sm mt-1 flex items-center gap-1">
                <span>⚠️</span>
                {passwordError}
              </p>
            )}
          </div>
          <div className="space-y-2">
            <label htmlFor="confirmPassword" className="block text-white/90 text-sm font-medium drop-shadow">
              Confirm Password
            </label>
            <div className="relative group">
              <input
                id="confirmPassword"
                type={showConfirmPassword ? "text" : "password"}
                value={confirmPassword}
                onChange={e => {
                  setConfirmPassword(e.target.value);
                  if (confirmPasswordError) setConfirmPasswordError("");
                }}
                onBlur={e => {
                  const err = validateConfirmPassword(password, e.target.value);
                  setConfirmPasswordError(err);
                }}
                className={`w-full bg-transparent border ${confirmPasswordError ? 'border-red-400' : 'border-white/20'} rounded-xl px-4 py-3 pr-12 text-white placeholder-white/60 focus:outline-none focus:ring-2 ${confirmPasswordError ? 'focus:ring-red-400/40 focus:border-red-400/40' : 'focus:ring-blue-400/40 focus:border-blue-400/40'} transition-all duration-300`}
                placeholder="Confirm your password"
                required
                autoComplete="new-password"
              />
              <button
                type="button"
                onClick={() => setShowConfirmPassword(!showConfirmPassword)}
                className="absolute inset-y-0 right-0 flex items-center pr-3 text-white/80 hover:text-blue-400 transition-colors duration-200"
                tabIndex={-1}
                aria-label="Show/Hide confirm password"
              >
                {showConfirmPassword ? <FaLockOpen className="w-3 h-3" /> : <FaLock className="w-3 h-3" />}
              </button>
            </div>
            {confirmPasswordError && (
              <p className="text-red-300 text-sm mt-1 flex items-center gap-1">
                <span>⚠️</span>
                {confirmPasswordError}
              </p>
            )}
          </div>
        </div>
        <div className="space-y-2 mt-4">
          <label htmlFor="phone" className="block text-white/90 text-sm font-medium drop-shadow">
            Phone
          </label>
          <input
            id="phone"
            type="tel"
            value={phone}
            onChange={e => {
              setPhone(e.target.value);
              if (phoneError) setPhoneError("");
            }}
            onBlur={e => {
              const err = validatePhone(e.target.value);
              setPhoneError(err);
            }}
            className={`w-full bg-transparent border ${phoneError ? 'border-red-400' : 'border-white/20'} rounded-xl px-4 py-3 text-white placeholder-white/60 focus:outline-none focus:ring-2 ${phoneError ? 'focus:ring-red-400/40 focus:border-red-400/40' : 'focus:ring-blue-400/40 focus:border-blue-400/40'} transition-all duration-300`}
            placeholder="Enter phone number (e.g., 0912345678)"
            required
          />
          {phoneError && (
            <p className="text-red-300 text-sm mt-1 flex items-center gap-1">
              <span>⚠️</span>
              {phoneError}
            </p>
          )}
        </div>
        {error && (
          <div className="bg-red-500/20 backdrop-blur-sm border border-red-400/30 text-red-100 rounded-xl px-4 py-3 text-sm flex items-center gap-2">
            <span>⚠️</span>
            {error}
          </div>
        )}
        <button
          type="submit"
          disabled={loading}
          className="w-full bg-white border border-white/20 text-black font-semibold py-3 mt-2 rounded-xl shadow-lg hover:bg-white/70 hover:text-black-300 disabled:opacity-70 disabled:cursor-not-allowed relative overflow-hidden group transition-all duration-200"
        >
          <span className="relative flex items-center justify-center gap-2">
            {loading ? (
              <>
                <div className="w-5 h-5 border-2 border-white/30 border-t-white rounded-full animate-spin"></div>
                Signing up...
              </>
            ) : (
              <>Sign Up</>
            )}
          </span>
        </button>
        <div className="text-center mt-3">
          <span className="text-white/80 text-sm">Already have an account? </span>
          <Link 
            href="/login" 
            className="text-blue-300 underline hover:text-blue-500 transition-colors text-sm"
          >
            Sign in
          </Link>
        </div>
      </form>
    </div>
  );
}
