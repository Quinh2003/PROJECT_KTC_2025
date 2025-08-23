import { useState } from "react";
import Link from "next/link";
import { registerUserApi } from "../../server/user.api";
import { loginApi } from "../../server/auth.api";
import { FaLock, FaLockOpen } from "react-icons/fa";
import { AuthResponse } from "../../types/User";

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

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError("");
    setLoading(true);
    if (password !== confirmPassword) {
      setError("Passwords do not match!");
      setLoading(false);
      return;
    }
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
              onChange={e => setFullName(e.target.value)}
              className="w-full bg-transparent border border-white/20 rounded-xl px-4 py-3 text-white placeholder-white/60 focus:outline-none focus:ring-2 focus:ring-blue-400/40 focus:border-blue-400/40 transition-all duration-300"
              placeholder="Enter full name"
              required
            />
          </div>
          <div className="space-y-2">
            <label htmlFor="email" className="block text-white/90 text-sm font-medium drop-shadow">
              Email
            </label>
            <input
              id="email"
              type="email"
              value={email}
              onChange={e => setEmail(e.target.value)}
              className="w-full bg-transparent border border-white/20 rounded-xl px-4 py-3 text-white placeholder-white/60 focus:outline-none focus:ring-2 focus:ring-blue-400/40 focus:border-blue-400/40 transition-all duration-300"
              placeholder="Enter email"
              required
              autoComplete="username"
            />
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
                onChange={e => setPassword(e.target.value)}
                className="w-full bg-transparent border border-white/20 rounded-xl px-4 py-3 pr-12 text-white placeholder-white/60 focus:outline-none focus:ring-2 focus:ring-blue-400/40 focus:border-blue-400/40 transition-all duration-300"
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
                onChange={e => setConfirmPassword(e.target.value)}
                className="w-full bg-transparent border border-white/20 rounded-xl px-4 py-3 pr-12 text-white placeholder-white/60 focus:outline-none focus:ring-2 focus:ring-blue-400/40 focus:border-blue-400/40 transition-all duration-300"
                placeholder="Enter password"
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
            onChange={e => setPhone(e.target.value)}
            className="w-full bg-transparent border border-white/20 rounded-xl px-4 py-3 text-white placeholder-white/60 focus:outline-none focus:ring-2 focus:ring-blue-400/40 focus:border-blue-400/40 transition-all duration-300"
            placeholder="Enter phone number"
            required
          />
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
