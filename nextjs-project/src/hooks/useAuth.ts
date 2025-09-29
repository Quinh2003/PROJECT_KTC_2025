// hooks/useAuth.ts
"use client";

import { useState, useEffect, useCallback } from "react";
import { useRouter } from "next/navigation";
import { getAuthToken, isTokenExpired } from "@/utils/auth";

interface User {
  id: number;
  username: string;
  email: string;
  fullName?: string;
}

export function useAuth() {
  const router = useRouter();
  const [user, setUser] = useState<User | null>(null);
  const [loading, setLoading] = useState(true);
  const [isAuthenticated, setIsAuthenticated] = useState(false);

  const logout = useCallback(() => {
    localStorage.removeItem("token");
    localStorage.removeItem("user");
    setUser(null);
    setIsAuthenticated(false);
    router.push("/login");
  }, [router]);

  useEffect(() => {
    const checkAuth = () => {
      try {
        const token = getAuthToken();
        const userStr = localStorage.getItem("user");

        if (!token || !userStr) {
          setUser(null);
          setIsAuthenticated(false);
          setLoading(false);
          return;
        }

        if (isTokenExpired(token)) {
          logout();
          return;
        }

        const userData = JSON.parse(userStr);
        setUser(userData);
        setIsAuthenticated(true);
      } catch (error) {
        console.error("Auth check failed:", error);
        logout();
      } finally {
        setLoading(false);
      }
    };

    checkAuth();
  }, [router, logout]);

  const login = (token: string, userData: User) => {
    localStorage.setItem("token", token);
    localStorage.setItem("user", JSON.stringify(userData));
    setUser(userData);
    setIsAuthenticated(true);
  };

  return {
    user,
    loading,
    isAuthenticated,
    login,
    logout,
  };
}
