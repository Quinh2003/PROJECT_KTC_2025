import { forgotPasswordApi } from "../../server/user.api";
import { useState } from "react";
import Link from "next/link";

export default function ForgotPasswordForm({ onBack }: { onBack?: () => void }) {
  const [email, setEmail] = useState("");
  const [message, setMessage] = useState("");
  const [error, setError] = useState("");
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setError("");
    setMessage("");
    setLoading(true);
    try {
      const res = await forgotPasswordApi(email);
      if (!res.ok) {
        setError("Email không tồn tại hoặc lỗi server!");
        return;
      }
      setMessage("Yêu cầu đặt lại mật khẩu đã được gửi. Vui lòng kiểm tra email!");
    } catch {
      setError("Không thể kết nối server!");
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="w-full bg-white/10 backdrop-blur-xl rounded-3xl border border-white/20 shadow-2xl px-8 py-10 space-y-6">
      <div className="text-center mb-6">
        <h2 className="text-xl font-semibold text-white drop-shadow-lg">Forgot Password</h2>
        <p className="text-white/70 text-sm mt-2">Nhập email đã đăng ký để nhận hướng dẫn đặt lại mật khẩu.</p>
      </div>
      <form onSubmit={handleSubmit} className="space-y-4">
        <div>
          <label htmlFor="email" className="block text-white/90 text-sm font-medium mb-2">Email</label>
          <input
            id="email"
            type="email"
            value={email}
            onChange={e => setEmail(e.target.value)}
            className="w-full bg-transparent border border-white/20 rounded-xl px-4 py-3 text-white placeholder-white/60 focus:outline-none focus:ring-2 focus:ring-blue-400/40 focus:border-blue-400/40"
            placeholder="Enter your registered email"
            required
            autoComplete="email"
          />
        </div>
        {error && <div className="text-red-400 text-sm">{error}</div>}
        {message && <div className="text-green-400 text-sm">{message}</div>}
        <button
          type="submit"
          disabled={loading}
          className="w-full bg-white text-black font-bold py-3 rounded-xl shadow-lg hover:bg-white/70 hover:text-black/70 disabled:opacity-70 disabled:cursor-not-allowed"
        >
          {loading ? "Sending..." : "Send Request"}
        </button>
      </form>
      <div className="text-center mt-4">
        {onBack ? (
          <button
            type="button"
            className="text-blue-300 underline hover:text-blue-500 text-sm font-medium"
            onClick={onBack}
          >
            Back to Sign In
          </button>
        ) : (
          <Link href="/login" className="text-blue-300 underline hover:text-blue-500 text-sm font-medium">Back to Login</Link>
        )}
      </div>
    </div>
  );
}
