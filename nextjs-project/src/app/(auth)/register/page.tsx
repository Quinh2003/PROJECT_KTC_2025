"use client";

import { useRouter } from "next/navigation";
import RegisterForm from "../../../components/forms/RegisterForm";
import { AuthResponse } from "../../../types/User";

export default function RegisterPage() {
  const router = useRouter();

  const handleRegister = (response: AuthResponse) => {
    // Store user data and token after successful registration
    if (typeof window !== "undefined") {
      localStorage.setItem("user", JSON.stringify(response.user));
      localStorage.setItem("token", response.token);
      // KHÔNG chuyển trang tự động, để RegisterForm xử lý luồng QR/OTP
    }
  };

  return (
    <div className="min-h-screen w-full flex items-center justify-center bg-cover bg-center relative overflow-hidden"
    style={{ backgroundImage: "url('/login.webp')" }}>
      <div className="absolute inset-0  backdrop-blur-xs z-0"></div>
      <div className="relative z-10 flex flex-col items-center w-full max-w-xl px-6">
        <RegisterForm onRegister={handleRegister} />
        
        <div className="mt-8 text-center">
          <p className="text-white/50 text-xs">© 2025 Fast Route Logistics. All rights reserved.</p>
        </div>
      </div>
    </div>
  );
}
