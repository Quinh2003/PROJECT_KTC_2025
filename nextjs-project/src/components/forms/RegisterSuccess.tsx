import { useState } from "react";
import QRCode from "react-qr-code";
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
  // Chuẩn hóa chuỗi QR: loại bỏ mọi khoảng trắng và xuống dòng
  const rawQrUrl = response?.otpauthUrl || "";
  const cleanQrUrl = rawQrUrl.trim();
  // Hiển thị QR đầu tiên, chỉ khi bấm nút mới hiện OTP
  const [showQR, setShowQR] = useState(!!rawQrUrl);
  const [showOtpForm, setShowOtpForm] = useState(false);
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
        // Nếu backend trả về token/user sau khi xác thực OTP, lưu lại vào localStorage
        if (result.token) {
          localStorage.setItem("token", result.token);
          // Lưu token vào cookie để middleware có thể kiểm tra
          document.cookie = `token=${result.token}; path=/;`;
        }
        if (result.user) {
          localStorage.setItem("user", JSON.stringify(result.user));
        }
        // Không tự động chuyển trang, để người dùng tự thao tác
      } else {
        setOtpError("Invalid or expired OTP code!");
      }
    } catch {
      setOtpError("OTP verification error!");
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="w-full bg-white/10 backdrop-blur-xl rounded-3xl border border-white/20 shadow-2xl px-8 py-10 flex flex-col items-center justify-center min-h-[60vh]">
      <div className="w-full max-w-lg mx-auto space-y-8 flex flex-col items-center">
        <div className="flex flex-col items-center gap-2">
          <div className="bg-green-500/90 rounded-full p-4 shadow-lg mb-2">
            <svg width="48" height="48" viewBox="0 0 24 24" fill="none" stroke="white" strokeWidth="2.5" strokeLinecap="round" strokeLinejoin="round"><circle cx="12" cy="12" r="10"/><path d="M8 12l2.5 2.5L16 9"/></svg>
          </div>
          <h2 className="text-2xl font-bold text-white drop-shadow-lg text-center">Registration Successful!</h2>
          <p className="text-white/80 text-base text-center max-w-xs">Congratulations <span className="font-semibold text-blue-200">{user.fullName}</span>, your account has been created.</p>
        </div>
        {rawQrUrl && showQR && (
          <div className="flex flex-col items-center gap-4 mt-4">
            <h3 className="text-lg text-white font-semibold">Scan the QR code with your Authenticator app</h3>
              <div style={{background: '#fff', padding: '16px', borderRadius: '12px'}}>
                <QRCode value={cleanQrUrl} size={240} />
              </div>
            <p className="text-white/80 text-sm break-all">Or open this link:<a href={cleanQrUrl} target="_blank" className="text-blue-300 underline">{cleanQrUrl}</a></p>
            <button 
              onClick={() => setShowQR(false)}
              className="mt-4 px-6 py-2 bg-red-500/80 text-white rounded-lg hover:bg-red-600/80 transition-all duration-200"
            >
              Ẩn mã QR
            </button>
            <button
              onClick={() => { setShowQR(false); setShowOtpForm(true); }}
              className="mt-2 px-6 py-2 bg-blue-500/80 text-white rounded-lg hover:bg-blue-600/80 transition-all duration-200"
            >
              Tiếp tục
            </button>
          </div>
        )}
        {rawQrUrl && !showQR && !showOtpForm && (
          <div className="flex flex-col items-center gap-4 mt-4">
            <button 
              onClick={() => setShowQR(true)}
              className="px-6 py-3 bg-blue-500/80 text-white rounded-lg hover:bg-blue-600/80 transition-all duration-200 font-semibold"
            >
              Hiện lại mã QR
            </button>
            <button
              onClick={() => setShowOtpForm(true)}
              className="mt-2 px-6 py-2 bg-blue-500/80 text-white rounded-lg hover:bg-blue-600/80 transition-all duration-200"
            >
              Tiếp tục
            </button>
          </div>
        )}
        {showOtpForm && (
          <form onSubmit={handleOtpSubmit} className="mt-8 flex flex-col items-center gap-6 bg-white/20 rounded-2xl p-8 shadow-2xl border border-blue-400/30">
            <div className="flex items-center gap-2 mb-2">
              <svg width="32" height="32" fill="none" stroke="currentColor" strokeWidth="2" strokeLinecap="round" strokeLinejoin="round" className="text-blue-400"><circle cx="16" cy="16" r="14"/><path d="M16 10v6l5 3"/></svg>
              <span className="text-white text-xl font-bold">Enter OTP from Authenticator app</span>
            </div>
            <p className="text-white/80 text-base mb-2 text-center">Open Google Authenticator or Microsoft Authenticator and enter the 6-digit OTP to enable 2FA security.</p>
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
              {loading ? "Verifying..." : "Verify OTP"}
            </button>
            {otpError && <div className="text-red-400 text-center font-semibold mt-2">{otpError}</div>}
          </form>
        )}
        {otpSuccess && (
          <div className="flex flex-col items-center gap-4 mt-4">
            <div className="text-green-400 font-semibold text-center text-lg">OTP verified! Two-factor authentication is now enabled.</div>
            <button
              className="mt-8 w-full bg-white border border-white/20 text-black font-semibold py-3 rounded-xl shadow-lg hover:bg-white/70 hover:text-black-300 transition-all duration-200 text-lg"
              onClick={() => router.push('/login')}
            >
              Đăng nhập
            </button>
          </div>
        )}
      </div>
    </div>
  );
}
