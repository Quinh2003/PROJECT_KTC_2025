"use client";

import { useRouter } from "next/navigation";
import LoginForm from "../../../components/forms/LoginForm";
import { AuthResponse } from "../../../types/User";
import Link from "next/link";

export default function LoginPage() {
  const router = useRouter();

  const handleLogin = (response: AuthResponse) => {
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
  };

  return (
    <div className="min-h-screen w-full flex items-center justify-center bg-cover bg-center relative overflow-hidden"
     style={{ backgroundImage: "url('/login.jpg')" }}>
      <div className="absolute inset-0 backdrop-blur-sm z-0"></div>
      <div className="relative z-10 flex flex-col items-center w-full max-w-md px-6">
        <LoginForm onLogin={handleLogin} />
        
        <div className="mt-6 text-center space-y-2 pt-4">
          <Link href="/register" className="text-blue-300 hover:text-blue-200 text-sm font-medium transition-colors duration-200 underline decoration-dotted">
            Don&apos;t have an account? Sign up
          </Link>
        </div>
        
        <div className="mt-8 text-center">
          <p className="text-white/50 text-xs">© 2025 Fast Route Logistics. All rights reserved.</p>
        </div>
      </div>
    </div>
  );
}
