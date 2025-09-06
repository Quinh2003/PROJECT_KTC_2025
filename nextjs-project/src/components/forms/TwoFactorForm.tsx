import React, { useState } from "react";

interface TwoFactorFormProps {
  email: string;
  onSuccess: (token: string) => void;
}

const TwoFactorForm: React.FC<TwoFactorFormProps> = ({ email, onSuccess }) => {
  const [otp, setOtp] = useState("");
  const [error, setError] = useState("");
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    setError("");
    try {
      const res = await fetch("http://localhost:8080/api/auth/totp/verify", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ email, code: otp })
      });
      const result = await res.json();
      if (result.valid === true && result.token) {
        onSuccess(result.token as string);
      } else {
        setError("Mã OTP không đúng hoặc đã hết hạn!");
      }
    } catch (err) {
      setError("Lỗi xác thực OTP!");
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="min-h-[60vh] flex flex-col items-center justify-center">
      <form onSubmit={handleSubmit} className="flex flex-col items-center gap-6 bg-white/20 rounded-2xl p-8 shadow-2xl border border-blue-400/30 w-full max-w-md">
        <div className="flex items-center gap-2 mb-2">
          <svg width="32" height="32" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="text-blue-400"><circle cx="16" cy="16" r="14"/><path d="M16 10v6l5 3"/></svg>
          <span className="text-blue-900 text-xl font-bold">Nhập mã OTP từ app Authenticator</span>
        </div>
        <p className="text-blue-900/80 text-base mb-2 text-center">Mở ứng dụng Google Authenticator hoặc Microsoft Authenticator, nhập 6 số OTP hiển thị để xác thực bảo mật 2 lớp.</p>
        <input
          id="otp"
          type="text"
          value={otp}
          onChange={e => setOtp(e.target.value)}
          maxLength={6}
          required
          autoFocus
          pattern="[0-9]{6}"
          inputMode="numeric"
          className="w-48 text-center text-2xl tracking-widest px-6 py-4 rounded-xl border-2 border-blue-500 bg-white/90 text-blue-900 font-bold focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-all duration-200 shadow-lg"
          placeholder="000000"
        />
        <button type="submit" disabled={loading} className="w-48 bg-gradient-to-r from-blue-500 to-blue-600 text-white py-3 rounded-xl font-bold shadow-xl hover:from-blue-600 hover:to-blue-700 transition text-xl">
          {loading ? "Đang xác thực..." : "Xác thực OTP"}
        </button>
        {error && <div className="text-red-400 text-center font-semibold mt-2">{error}</div>}
      </form>
    </div>
  );
};

export default TwoFactorForm;
