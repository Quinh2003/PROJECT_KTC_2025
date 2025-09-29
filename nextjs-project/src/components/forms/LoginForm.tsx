"use client";

import { useState } from "react";
import { AuthResponse } from "../../types/User";
import { setTokenCookie, setRefreshTokenCookie } from "../../lib/auth";
import Link from "next/dist/client/link";
import { useRouter } from "next/navigation";
import { loginApi, googleLoginApi } from "../../server/auth.api";
import { FaLock, FaLockOpen } from "react-icons/fa";
import ForgotPasswordForm from "./ForgotPasswordForm";
import { getAuth, signInWithPopup, GoogleAuthProvider } from "firebase/auth";
import { app } from "../../lib/firebase";
import TwoFactorForm from "./TwoFactorForm";

interface LoginFormProps {
  onLogin: (response: AuthResponse) => void;
}

export default function LoginForm({ onLogin }: LoginFormProps) {
  const router = useRouter();
  
  // Xử lý đăng nhập bằng Google
  const handleGoogleLogin = async () => {
    const auth = getAuth(app);
    const provider = new GoogleAuthProvider();
    try {
      // Đăng nhập Google bằng Firebase Auth
      const result = await signInWithPopup(auth, provider);
      // Lấy accessToken Google để gửi lên backend
      const credential = GoogleAuthProvider.credentialFromResult(result);
      const accessToken = credential?.accessToken;
      console.log("Google accessToken:", accessToken);
      if (!accessToken) {
        alert("Unable to get access token from Google!");
        return;
      }
      // Gửi accessToken này lên backend để xác thực Google login và nhận OTP
      const res = await googleLoginApi(accessToken);
      const data = await res.json();
      console.log("Google login response:", data);
      console.log("totpEnabled type:", typeof data.user?.totpEnabled, "value:", data.user?.totpEnabled);
      if (data.user?.role?.toLowerCase() === "customer") {
        if (!data.user?.totpEnabled) {
          // Nếu chưa xác thực OTP, hiển thị form nhập OTP
          setPending2FA(true);
          setPendingUser(data.user);
          return;
        }
        // Nếu đã xác thực OTP, đăng nhập luôn
        setTokenCookie(data.token);
        setRefreshTokenCookie(data.refreshToken);
        if (onLogin) onLogin(data);
      } else {
        setError("This application is for customers only. Please use the employee application.");
      }
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    } catch (error) {
      setError("Google login failed!");
    }
  };
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [error, setError] = useState("");
  const [emailError, setEmailError] = useState("");
  const [passwordError, setPasswordError] = useState("");
  const [loading, setLoading] = useState(false);
  const [showPassword, setShowPassword] = useState(false);
  const [showForgot, setShowForgot] = useState(false);
  const [pending2FA, setPending2FA] = useState(false);
  const [pendingUser, setPendingUser] = useState<AuthResponse["user"] | null>(null);

  // Validation functions
  const validateEmail = (email: string): string => {
    if (!email) return "Email is required";
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) return "Please enter a valid email";
    return "";
  };

  const validatePassword = (password: string): string => {
    if (!password) return "Password is required";
    if (password.length < 6) return "Password must be at least 6 characters";
    return "";
  };

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError("");
    setEmailError("");
    setPasswordError("");
    
    // Validate form
    const emailErr = validateEmail(email);
    const passwordErr = validatePassword(password);
    
    if (emailErr) {
      setEmailError(emailErr);
      return;
    }
    
    if (passwordErr) {
      setPasswordError(passwordErr);
      return;
    }
    
    setLoading(true);
    try {
      const res = await loginApi(email, password);
      if (!res.ok) {
        setError("Invalid email or password!");
        return;
      }
      const data = await res.json();
      const userRole = data.user?.role?.toLowerCase();
      if (userRole !== "customer") {
        setError("This application is for customers only!");
        return;
      }
      if (data.user?.totpEnabled) {
        setPending2FA(true);
        setPendingUser(data.user);
        return;
      }
      if (typeof window !== "undefined") {
        setTokenCookie(data.token);
        setRefreshTokenCookie(data.refreshToken);
        localStorage.setItem("user", JSON.stringify(data.user));
        const accountsStr = localStorage.getItem("accounts");
        const accounts = accountsStr ? (() => { try { return JSON.parse(accountsStr); } catch { return {}; } })() : {};
        accounts[email] = password;
        localStorage.setItem("accounts", JSON.stringify(accounts));
      }
      onLogin(data);
    } catch {
      setError("Unable to connect to server!");
    } finally {
      setLoading(false);
    }
  };

  if (pending2FA && pendingUser) {
    return <TwoFactorForm email={pendingUser.email} onSuccess={(token) => {
      setPending2FA(false);
      if (typeof window !== "undefined") {
        setTokenCookie(token);
        localStorage.setItem("user", JSON.stringify(pendingUser));
      }
      if (onLogin) onLogin({ user: pendingUser, token });
      router.push("/account");
    }} />;
  }

  if (showForgot) {
    return <ForgotPasswordForm onBack={() => setShowForgot(false)} />;
  }

  return (
    <div className="w-full bg-white/10 backdrop-blur-xl rounded-3xl border border-white/20 shadow-2xl px-8 py-10 space-y-6">
      <div className="text-center mb-8">
        <h2 className="text-2xl font-semibold text-white drop-shadow-lg">
          Welcome to FastRoute
        </h2>
        <p className="text-white/70 text-sm mt-2">
          Log in to continue using our services
        </p>
      </div>

      <form onSubmit={handleSubmit} className="space-y-6" autoComplete="off">
        <div className="space-y-2">
          <label htmlFor="email" className="block text-white/90 text-sm font-medium drop-shadow">
            Email
          </label>
          <input
            id="email"
            name="email"
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
            className={`w-full bg-transparent border ${emailError ? 'border-red-400' : 'border-white/20'} rounded-xl px-4 py-3 text-white text-base placeholder-white/60 focus:outline-none focus:ring-2 ${emailError ? 'focus:ring-red-400/40 focus:border-red-400/40' : 'focus:ring-blue-400/40 focus:border-blue-400/40'} transition-all duration-300`}
            placeholder="Enter email"
            required
            autoComplete="email"
          />
          {emailError && (
            <p className="text-red-300 text-sm mt-1 flex items-center gap-1">
              <span>⚠️</span>
              {emailError}
            </p>
          )}
        </div>

        <div className="space-y-2 -mt-1 my-6">
          <div className="flex items-center justify-between">
            <label htmlFor="password" className="block text-white/90 text-sm font-medium drop-shadow">
              Password
            </label>
            <button
              type="button"
              className="!text-black hover:!text-white/50 text-xs font-medium ml-auto transition-all duration-200"
              style={{ 
                fontSize: '14px'
              }}
              onClick={() => setShowForgot(true)}
            >
              Forgot password?
            </button>
          </div>
          <div className="relative group">
            <input
              id="password"
              name="password"
              type={showPassword ? "text" : "password"}
              value={password}
              onChange={e => {
                setPassword(e.target.value);
                if (passwordError) setPasswordError("");
              }}
              onBlur={e => {
                const err = validatePassword(e.target.value);
                setPasswordError(err);
              }}
              className={`w-full bg-transparent border ${passwordError ? 'border-red-400' : 'border-white/20'} rounded-xl px-4 py-3 pr-12 text-white placeholder-white/60 focus:outline-none focus:ring-2 ${passwordError ? 'focus:ring-red-400/40 focus:border-red-400/40' : 'focus:ring-blue-400/40 focus:border-blue-400/40'} transition-all duration-300`}
              placeholder="Enter password"
              required
              autoComplete="current-password"
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

        {error && (
          <div className="bg-red-500/20 backdrop-blur-sm border border-red-400/30 text-red-100 rounded-xl px-4 py-3 text-sm flex items-center gap-2">
            <span>⚠️</span>
            {error}
          </div>
        )}

        <button
          type="submit"
          disabled={loading}
          className="w-full bg-white text-black font-bold py-3 mt-3 rounded-xl shadow-lg hover:bg-white/70 hover:text-black/70 disabled:opacity-70 disabled:cursor-not-allowed relative overflow-hidden group transition-all duration-200 border border-white/30"
        >
          <span className="relative flex items-center justify-center gap-2">
            {loading ? (
              <>
                <div className="w-5 h-5 border-2 border-white/70 border-t-black-600 rounded-full animate-spin"></div>
                Logging in...
              </>
            ) : (
              <>Log In</>
            )}
          </span>
        </button>

        <div className="flex items-center">
          <div className="flex-grow h-px bg-white/30" />
          <span className="mx-3 text-white/60 text-sm">OR</span>
          <div className="flex-grow h-px bg-white/30" />
        </div>

        <button
          type="button"
          onClick={handleGoogleLogin}
          className="w-full bg-white text-black font-bold py-3 mt-2 rounded-xl shadow-lg border border-white/30 flex items-center justify-center gap-2 hover:bg-gray-100"
        >
          <svg width="20" height="20" viewBox="0 0 48 48" fill="none" xmlns="http://www.w3.org/2000/svg">
            <g clipPath="url(#clip0_17_40)">
              <path d="M47.5 24.5C47.5 22.7 47.3 21 47 19.3H24.5V28.7H37.6C36.9 32.1 34.3 34.7 30.9 36.2V41.1H38.2C43.1 37.1 47.5 31.2 47.5 24.5Z" fill="#4285F4"/>
              <path d="M24.5 48C31.1 48 36.7 45.8 40.7 41.1L30.9 36.2C28.7 37.5 26.1 38.3 24.5 38.3C18.1 38.3 12.7 34.1 10.7 28.7H2.2V33.7C6.2 41.1 14.1 48 24.5 48Z" fill="#34A853"/>
              <path d="M10.7 28.7C10.2 27.4 10 26.1 10 24.7C10 23.3 10.2 22 10.7 20.7V15.7H2.2C0.7 18.5 0 21.5 0 24.7C0 27.9 0.7 30.9 2.2 33.7L10.7 28.7Z" fill="#FBBC05"/>
              <path d="M24.5 10.7C27.2 10.7 29.6 11.6 31.5 13.4L38.3 6.6C34.7 3.2 29.9 0.7 24.5 0.7C14.1 0.7 6.2 7.6 2.2 15.7L10.7 20.7C12.7 15.3 18.1 10.7 24.5 10.7Z" fill="#EA4335"/>
            </g>
            <defs>
              <clipPath id="clip0_17_40">
                <rect width="48" height="48" fill="white"/>
              </clipPath>
            </defs>
          </svg>
          Continue with Google
        </button>
      </form>
      
      <div className="text-center mt-5 pt-2 border-white/20">
        <span className="text-white/80 text-sm">Don&apos;t have an account? </span>
        <Link 
          href="/register" 
          className="text-blue-300 hover:text-blue-200 text-sm font-medium underline decoration-dotted hover:decoration-solid transition-all duration-200"
        >
          Register
        </Link>
      </div>
    </div>
  );
}
