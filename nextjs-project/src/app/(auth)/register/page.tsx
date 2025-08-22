"use client";

import { useRouter } from "next/navigation";
import RegisterForm from "../../../components/forms/RegisterForm";
import { AuthResponse } from "../../../types/User";
import Link from "next/link";

export default function RegisterPage() {
  const router = useRouter();

  const handleRegister = (response: AuthResponse) => {
    // Store user data and token after successful registration
    localStorage.setItem("user", JSON.stringify(response.user));
    localStorage.setItem("token", response.token);
    
    // Redirect directly to dashboard
    router.push("/account");
  };

  return (
    <div className="min-h-screen w-full flex items-center justify-center bg-cover bg-center relative overflow-hidden" style={{ backgroundImage: "url('/login.jpg')" }}>
      <div className="absolute inset-0 bg-gradient-to-br from-blue-900/50 via-purple-900/40 to-indigo-900/50 backdrop-blur-md z-0"></div>
      <div className="relative z-10 flex flex-col items-center w-full max-w-md px-6">
        <RegisterForm onRegister={handleRegister} />
        
        <div className="mt-6 text-center space-y-2 pt-4">
          <Link href="/login" className="text-blue-300 hover:text-blue-200 text-sm font-medium transition-colors duration-200 underline decoration-dotted">
            Already have an account? Sign in
          </Link>
        </div>
        
        <div className="mt-8 text-center">
          <p className="text-white/50 text-xs">© 2025 Fast Route Logistics. All rights reserved.</p>
        </div>
      </div>
    </div>
  );
}
