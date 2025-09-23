"use client";

import LoginForm from "@/components/forms/LoginForm";

export default function LoginPage() {
  const handleLogin = (response: { user: unknown; token: string }) => {
    if (typeof window !== "undefined") {
      localStorage.setItem("user", JSON.stringify(response.user));
      localStorage.setItem("token", response.token);
      window.location.href = "/account";
    }
  };

  return <LoginForm onLogin={handleLogin} />;
}
