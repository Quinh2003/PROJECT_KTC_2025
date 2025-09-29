import { useState } from "react";
import { useTranslation } from 'react-i18next';
import type { User } from "../types/User";
import LanguageSwitcher from './LanguageSwitcher';

// Validation functions - sử dụng i18n
const validateEmail = (email: string, t: any) => {
  if (!email.trim()) return t('validation.required');
  const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
  if (!emailRegex.test(email)) return t('validation.invalidEmail');
  return "";
};

const validatePassword = (password: string, t: any) => {
  if (!password.trim()) return t('validation.required');
  return "";
};

export default function LoginForm({ onLogin }: { onLogin: (user: User) => void }) {
  const { t } = useTranslation();
  const [email, setEmail] = useState("");
  const [password, setPassword] = useState("");
  const [error, setError] = useState("");
  const [showPassword, setShowPassword] = useState(false);
  const [loading, setLoading] = useState(false);
  const [emailError, setEmailError] = useState("");
  const [passwordError, setPasswordError] = useState("");

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError("");
    setEmailError("");
    setPasswordError("");
    
    // Validate form
    const emailErr = validateEmail(email, t);
    const passwordErr = validatePassword(password, t);
    
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
      const res = await fetch("http://localhost:8080/api/auth/login", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ email, password }),
      });
      if (!res.ok) {
        setError(t('auth.login.invalidCredentials'));
        setLoading(false);
        return;
      }
      const data = await res.json();
      // Lưu token và user vào localStorage
      localStorage.setItem("token", data.token);
      localStorage.setItem("user", JSON.stringify(data.user));
      onLogin(data.user);
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    } catch (err) {
      setError(t('notifications.networkError'));
    } finally {
      setLoading(false);
    }
  };

  return (
    <div
      className="min-h-screen w-full flex items-center justify-center bg-cover bg-center relative overflow-hidden"
      style={{
        backgroundImage: "url('/login.jpg')",
      }}
    >
      {/* Animated Background Overlay */}
      <div className="absolute inset-0 bg-gradient-to-br from-blue-900/30 via-purple-900/20 to-indigo-900/30 backdrop-blur-sm z-0"></div>
      
      {/* Language Switcher - positioned at top right of screen */}
      <div className="absolute top-6 right-6 z-20 drop-shadow-lg">
        <LanguageSwitcher />
      </div>

      {/* Floating Animation Elements */}
      <div className="absolute inset-0 overflow-hidden pointer-events-none">
        <div className="absolute top-1/4 left-1/4 w-72 h-72 bg-blue-400/10 rounded-full blur-3xl animate-pulse"></div>
        <div className="absolute bottom-1/4 right-1/4 w-96 h-96 bg-purple-400/10 rounded-full blur-3xl animate-pulse delay-700"></div>
        <div className="absolute top-3/4 left-3/4 w-64 h-64 bg-indigo-400/10 rounded-full blur-3xl animate-pulse delay-1000"></div>
      </div>

      <div className="relative z-10 flex flex-col items-center w-full max-w-md px-6">

        {/* Logo/Brand Section */}
        <div className="mb-8 text-center">
          <h1 className="text-3xl font-bold text-white drop-shadow-lg">
            Fast Route
          </h1>
          <p className="text-white/70 text-sm mt-2">
            {t('common.welcome')}
          </p>
        </div>

        <form
          onSubmit={handleSubmit}
          className="w-full bg-white/10 backdrop-blur-xl rounded-3xl border border-white/20 shadow-2xl px-8 py-10 space-y-6 transform transition-all duration-500 "
        >
          <div className="text-center mb-8">
            <h2 className="text-2xl font-semibold text-white drop-shadow-lg">
              {t('auth.login.title')}
            </h2>
            <p className="text-white/70 text-sm mt-2">
              {t('auth.login.subtitle')}
            </p>
          </div>
          {/* Email Input */}
          <div className="space-y-2">
            <label htmlFor="email" className="block text-white/90 text-sm font-medium drop-shadow">
              {t('auth.login.email')}
            </label>
            <div className="relative group">
              <div className="absolute inset-0 bg-gradient-to-r from-blue-500/20 to-purple-500/20 rounded-xl blur opacity-0 group-hover:opacity-100 transition-opacity duration-300"></div>
              <input
                id="email"
                type="email"
                value={email}
                onChange={e => {
                  setEmail(e.target.value);
                  if (emailError) setEmailError("");
                }}
                onBlur={e => {
                  const err = validateEmail(e.target.value, t);
                  setEmailError(err);
                }}
                className={`relative w-full bg-white/10 backdrop-blur-sm border ${emailError ? 'border-red-400' : 'border-white/30'} rounded-xl px-4 py-3 text-white placeholder-white/60 focus:outline-none focus:ring-2 ${emailError ? 'focus:ring-red-400/50 focus:border-red-400/50' : 'focus:ring-blue-400/50 focus:border-blue-400/50'} transition-all duration-300 hover:bg-white/15`}
                placeholder={t('auth.login.emailPlaceholder', 'Enter your email address')}
                required
                autoComplete="username"
              />
              <div className="absolute inset-y-0 right-0 flex items-center pr-3">
                <span className="text-white/40">✉️</span>
              </div>
            </div>
            {emailError && (
              <p className="text-red-300 text-sm mt-1 flex items-center gap-1">
                <span>⚠️</span>
                {emailError}
              </p>
            )}
          </div>

          {/* Password Input */}
          <div className="space-y-2">
            <label htmlFor="password" className="block text-white/90 text-sm font-medium drop-shadow">
              {t('auth.login.password')}
            </label>
            <div className="relative group">
              <div className="absolute inset-0 bg-gradient-to-r from-purple-500/20 to-blue-500/20 rounded-xl blur opacity-0 group-hover:opacity-100 transition-opacity duration-300"></div>
              <input
                id="password"
                type={showPassword ? "text" : "password"}
                value={password}
                onChange={e => {
                  setPassword(e.target.value);
                  if (passwordError) setPasswordError("");
                }}
                onBlur={e => {
                  const err = validatePassword(e.target.value, t);
                  setPasswordError(err);
                }}
                className={`relative w-full bg-white/10 backdrop-blur-sm border ${passwordError ? 'border-red-400' : 'border-white/30'} rounded-xl px-4 py-3 pr-12 text-white placeholder-white/60 focus:outline-none focus:ring-2 ${passwordError ? 'focus:ring-red-400/50 focus:border-red-400/50' : 'focus:ring-blue-400/50 focus:border-blue-400/50'} transition-all duration-300 hover:bg-white/15`}
                placeholder={t('auth.login.passwordPlaceholder', 'Enter your password')}
                required
                autoComplete="current-password"
              />
              <button
                type="button"
                onClick={() => setShowPassword(!showPassword)}
                className="absolute inset-y-0 right-0 flex items-center pr-3 text-white/60 hover:text-white transition-colors duration-200"
                tabIndex={-1}
                aria-label="Toggle password visibility"
              >
                {showPassword ? "🙈" : "🐵"}
              </button>
            </div>
            {passwordError && (
              <p className="text-red-300 text-sm mt-1 flex items-center gap-1">
                <span>⚠️</span>
                {passwordError}
              </p>
            )}
          </div>
          {/* Error Message */}
          {error && (
            <div className="bg-red-500/20 backdrop-blur-sm border border-red-400/30 text-red-100 rounded-xl px-4 py-3 text-sm flex items-center gap-2 animate-shake">
              <span>⚠️</span>
              {error}
            </div>
          )}

          {/* Submit Button */}
          <button
            type="submit"
            disabled={loading}
            className="w-full bg-gradient-to-r from-blue-500 to-purple-600 hover:from-blue-600 hover:to-purple-700 text-white font-semibold py-4 rounded-xl shadow-lg hover:shadow-xl disabled:opacity-70 disabled:cursor-not-allowed disabled:transform-none relative overflow-hidden group"
          >
            <div className="absolute inset-0 bg-gradient-to-r from-white/0 via-white/20 to-white/0 transform -skew-x-12 translate-x-[-100%] group-hover:translate-x-[100%] transition-transform duration-1000"></div>
            <span className="relative flex items-center justify-center gap-2">
              {loading ? (
                <>
                  <div className="w-5 h-5 border-2 border-white/30 border-t-white rounded-full animate-spin"></div>
                  {t('common.loading')}
                </>
              ) : (
                <>
                  {t('auth.login.loginButton')}
                </>
              )}
            </span>
          </button>

          {/* Additional Links */}
          <div className="text-center space-y-2 pt-4">
            <p className="text-white/60 text-sm">
              {t('auth.login.needHelp', 'Need help accessing your account?')}
            </p>
            <button
              type="button"
              className="text-blue-300 hover:text-blue-200 text-sm font-medium transition-colors duration-200 underline decoration-dotted"
            >
              {t('auth.login.contactAdmin', 'Contact Administrator')}
            </button>
          </div>
        </form>

        {/* Footer */}
        <div className="mt-8 text-center">
          <p className="text-white/50 text-xs">
            © 2025 Fast Route Logistics. All rights reserved.
          </p>
        </div>
      </div>
    </div>
  );
}
