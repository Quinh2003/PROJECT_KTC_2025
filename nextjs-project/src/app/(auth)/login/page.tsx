"use client";

import { useRouter } from "next/navigation";
import LoginForm from "../../../components/forms/LoginForm";
import { AuthResponse } from "../../../types/User";

export default function LoginPage() {
  const router = useRouter();

  const handleLogin = (response: AuthResponse) => {
    if (typeof window !== "undefined") {
      localStorage.setItem("user", JSON.stringify(response.user));
      localStorage.setItem("token", response.token);
      const userRole = response.user.role?.toLowerCase();
      if (userRole === "customer") {
        router.push("/account");
      } else {
        alert("This application is for customers only. Please use the employee application.");
        localStorage.clear(); 
        return;
      }
    }
  };

  return (
    <div className="min-h-screen w-full flex items-center justify-center bg-cover bg-center relative overflow-hidden"
     style={{ backgroundImage: "url('/login.webp')" }}>
      <div className="absolute inset-0 backdrop-blur-xs z-0"></div>
      <div className="relative z-10 flex flex-col items-center w-full max-w-md px-6">
        <LoginForm onLogin={handleLogin} />
        
        <div className="mt-8 text-center">
          <p className="text-white/50 text-xs">© 2025 Fast Route Logistics. All rights reserved.</p>
        </div>
      </div>
    </div>
  );
}
