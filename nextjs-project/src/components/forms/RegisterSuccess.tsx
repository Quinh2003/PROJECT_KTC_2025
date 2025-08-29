import { useState } from "react";
import { QRCodeCanvas } from "qrcode.react";
import { useRouter } from "next/navigation";

interface RegisterSuccessProps {
  response: any;
  user: {
    email: string;
    fullName: string;
    phone: string;
  };
}

export default function RegisterSuccess({ response, user }: RegisterSuccessProps) {
  const router = useRouter();
  const [showQR, setShowQR] = useState(true);
  const qrUrl = response?.totpQrUrl;
  const [showOtpForm, setShowOtpForm] = useState(!!qrUrl);
  const [otp, setOtp] = useState("");
  const [otpError, setOtpError] = useState("");
  const [otpSuccess, setOtpSuccess] = useState(false);
  const [loading, setLoading] = useState(false);

  const handleOtpSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);
    setOtpError("");
    try {
      const res = await fetch("http://localhost:8080/api/auth/totp/verify", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ email: user.email, code: otp })
      });
      const result = await res.json();
      if (result.valid === true) {
        setOtpSuccess(true);
        setShowOtpForm(false);
        setTimeout(() => {
          router.push('/login');
        }, 1200);
      } else {
        setOtpError("Mã OTP không đúng hoặc đã hết hạn!");
      }
    } catch {
      setOtpError("Lỗi xác thực OTP!");
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="min-h-screen flex flex-col items-center justify-center bg-white/10 backdrop-blur-xl rounded-3xl border border-white/20 shadow-2xl px-8 py-10">
      <div className="w-full max-w-md mx-auto space-y-6">
        <h2 className="text-2xl font-semibold text-white drop-shadow-lg mb-4 text-center">Đăng ký thành công!</h2>
        {qrUrl && (
          <div className="flex flex-col items-center gap-4">
            <h3 className="text-lg text-white">Quét mã QR với app Authenticator:</h3>
            <QRCodeCanvas value={qrUrl ? qrUrl.replace(/\n/g, '').trim() : ''} size={180} />
            <p className="text-white/80 text-sm break-all">Hoặc mở link: <a href={qrUrl} target="_blank" className="text-blue-300 underline">{qrUrl}</a></p>
          </div>
        )}
        {showOtpForm && (
          <form onSubmit={handleOtpSubmit} className="mt-10 flex flex-col items-center gap-6 bg-white/20 rounded-2xl p-8 shadow-2xl border border-blue-400/30">
            <div className="flex items-center gap-2 mb-2">
              <svg width="32" height="32" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="text-blue-400"><circle cx="16" cy="16" r="14"/><path d="M16 10v6l5 3"/></svg>
              <span className="text-white text-xl font-bold">Nhập mã OTP từ app Authenticator</span>
            </div>
            <p className="text-white/80 text-base mb-2 text-center">Mở ứng dụng Google Authenticator hoặc Microsoft Authenticator, nhập 6 số OTP hiển thị để xác thực bảo mật 2 lớp.</p>
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
            {otpError && <div className="text-red-400 text-center font-semibold mt-2">{otpError}</div>}
          </form>
        )}
        {otpSuccess && <div className="text-green-400 font-semibold text-center text-lg">Xác thực OTP thành công! Bạn đã kích hoạt bảo mật 2 lớp.</div>}
        {!showQR && <p className="text-white/80 text-center">Bạn đã quét mã QR. Hãy đăng nhập để sử dụng!</p>}
      </div>
    </div>
  );
}
