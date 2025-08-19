"use client";

import { useState, useEffect } from "react";
import { useForm } from "react-hook-form";
import { useAuth } from "../hooks/use-auth";
import { useRouter } from "next/navigation";

type LoginFormInputs = {
  email: string;
  password: string;
};

export default function LoginPage() {
  const [serverError, setServerError] = useState("");
  const { login, isAuthenticated, user } = useAuth();
  const router = useRouter();

  const {
    register,
    handleSubmit,
    setValue,
    formState: { errors, isSubmitting },
  } = useForm<LoginFormInputs>();

  // Duplicate getDashboardUrl removed to fix redeclaration error.

  const redirectToDashboard = (role: string) => {
    // Redirect đến ReactJS project port 3001
    const dashboardUrl = getDashboardUrl(role);
    window.location.href = dashboardUrl;
  };

  // Nếu đã đăng nhập thì chuyển hướng luôn, không render login page
  if (isAuthenticated && user) {
    redirectToDashboard(user.role);
    return null;
  }

  const onSubmit = async (data: LoginFormInputs) => {
    setServerError("");
    // Fake check tài khoản demo
    console.log(data)
    const found = demoAccounts.find(
      (acc) => acc.email === data.email && acc.password === data.password
    );
    if (found) {
      // Fake user object
      const fakeUser = {
        id: found.email,
        email: found.email,
        role: (found.role === "Admin"
          ? "ADMIN"
          : found.role === "Dispatcher"
          ? "DISPATCHER"
          : found.role === "Fleet Manager"
          ? "FLEET_MANAGER"
          : found.role === "Driver"
          ? "DRIVER"
          : found.role === "Operations"
          ? "OPERATIONS_MANAGER"
          : "ADMIN") as
          | "ADMIN"
          | "DISPATCHER"
          | "FLEET_MANAGER"
          | "DRIVER"
          | "OPERATIONS_MANAGER",
        name: found.role,
      };
      // Fake token
      const fakeToken = "FAKE_TOKEN";
      // Lưu vào localStorage để ReactJS nhận được trạng thái đăng nhập
      localStorage.setItem("jwt_token", fakeToken);
      localStorage.setItem("user", JSON.stringify(fakeUser));
      login(fakeToken, fakeUser);
      redirectToDashboard(fakeUser.role);
    } else {
      setServerError("Sai tài khoản hoặc mật khẩu demo!");
    }
  };

  const getDashboardUrl = (role: string): string => {
    const baseUrl = "http://localhost:5173"; // ReactJS project

    switch (role) {
      case "ADMIN":
        return `${baseUrl}/admin`;
      case "DISPATCHER":
        return `${baseUrl}/dispatcher`;
      case "FLEET_MANAGER":
        return `${baseUrl}/fleet`;
      case "DRIVER":
        return `${baseUrl}/driver`;
      case "OPERATIONS_MANAGER":
        return `${baseUrl}/operations`;
      default:
        return `${baseUrl}/dashboard`;
    }
  };

  // Demo accounts for testing
  const demoAccounts = [
    { role: "Admin", email: "admin@ktc.com", password: "123456" },
    { role: "Dispatcher", email: "dispatcher@ktc.com", password: "123456" },
    { role: "Fleet Manager", email: "fleet@ktc.com", password: "123456" },
    { role: "Driver", email: "driver@ktc.com", password: "123456" },
    { role: "Operations", email: "operations@ktc.com", password: "123456" },
  ];

  const fillDemoAccount = (email: string, password: string) => {
    // Dùng setValue của react-hook-form để cập nhật giá trị form
    setValue("email", email);
    setValue("password", password);
  };

  return (
    <div className="min-h-screen flex items-center justify-center bg-gradient-to-br from-blue-50 to-indigo-100 py-12 px-4">
      <div className="w-full max-w-md">
        {/* Logo và Header */}
        <div className="text-center mb-8">
          <div className="mx-auto h-20 w-20 bg-blue-600 rounded-full flex items-center justify-center mb-4 shadow-lg">
            <svg
              className="h-10 w-10 text-white"
              fill="none"
              viewBox="0 0 24 24"
              stroke="currentColor"
            >
              <path
                strokeLinecap="round"
                strokeLinejoin="round"
                strokeWidth={2}
                d="M13 10V3L4 14h7v7l9-11h-7z"
              />
            </svg>
          </div>
          <h1 className="text-3xl font-bold text-gray-900">
            KTC Logistics 2025
          </h1>
          <p className="text-gray-600 mt-2">
            Hệ thống quản lý logistics thông minh
          </p>
        </div>

        {/* Form đăng nhập */}
        <div className="bg-white rounded-xl shadow-xl p-8">
          <form onSubmit={handleSubmit(onSubmit)} className="space-y-6">
            <div>
              <label
                htmlFor="email"
                className="block text-sm font-medium text-gray-700 mb-2"
              >
                Email đăng nhập
              </label>
              <input
                id="email"
                type="email"
                {...register("email", {
                  required: "Vui lòng nhập email",
                  pattern: {
                    value: /^[^\s@]+@[^\s@]+\.[^\s@]+$/,
                    message: "Email không hợp lệ",
                  },
                })}
                className="w-full px-4 py-3 border border-gray-300 rounded-lg shadow-sm focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-colors"
                placeholder="your@email.com"
                disabled={isSubmitting}
              />
              {errors.email && (
                <p className="mt-2 text-sm text-red-600">
                  {errors.email.message}
                </p>
              )}
            </div>

            <div>
              <label
                htmlFor="password"
                className="block text-sm font-medium text-gray-700 mb-2"
              >
                Mật khẩu
              </label>
              <input
                id="password"
                type="password"
                {...register("password", {
                  required: "Vui lòng nhập mật khẩu",
                  minLength: {
                    value: 6,
                    message: "Mật khẩu phải có ít nhất 6 ký tự",
                  },
                })}
                className="w-full px-4 py-3 border border-gray-300 rounded-lg shadow-sm focus:outline-none focus:ring-2 focus:ring-blue-500 focus:border-blue-500 transition-colors"
                placeholder="••••••••"
                disabled={isSubmitting}
              />
              {errors.password && (
                <p className="mt-2 text-sm text-red-600">
                  {errors.password.message}
                </p>
              )}
            </div>

            {/* Error message */}
            {serverError && (
              <div className="bg-red-50 border-l-4 border-red-400 p-4 rounded-r-lg">
                <div className="flex">
                  <div className="flex-shrink-0">
                    <svg
                      className="h-5 w-5 text-red-400"
                      viewBox="0 0 20 20"
                      fill="currentColor"
                    >
                      <path
                        fillRule="evenodd"
                        d="M10 18a8 8 0 100-16 8 8 0 000 16zM8.707 7.293a1 1 0 00-1.414 1.414L8.586 10l-1.293 1.293a1 1 0 101.414 1.414L10 11.414l1.293 1.293a1 1 0 001.414-1.414L11.414 10l1.293-1.293a1 1 0 00-1.414-1.414L10 8.586 8.707 7.293z"
                        clipRule="evenodd"
                      />
                    </svg>
                  </div>
                  <div className="ml-3">
                    <p className="text-sm text-red-700">{serverError}</p>
                  </div>
                </div>
              </div>
            )}

            {/* Submit button */}
            <button
              type="submit"
              disabled={isSubmitting}
              className="w-full bg-blue-600 text-white py-3 px-4 rounded-lg font-medium hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-blue-500 focus:ring-offset-2 disabled:opacity-50 disabled:cursor-not-allowed transition-all duration-200 transform hover:scale-[1.02]"
            >
              {isSubmitting ? (
                <div className="flex items-center justify-center">
                  <svg
                    className="animate-spin -ml-1 mr-3 h-5 w-5 text-white"
                    xmlns="http://www.w3.org/2000/svg"
                    fill="none"
                    viewBox="0 0 24 24"
                  >
                    <circle
                      className="opacity-25"
                      cx="12"
                      cy="12"
                      r="10"
                      stroke="currentColor"
                      strokeWidth="4"
                    ></circle>
                    <path
                      className="opacity-75"
                      fill="currentColor"
                      d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
                    ></path>
                  </svg>
                  Đang đăng nhập...
                </div>
              ) : (
                "Đăng nhập"
              )}
            </button>
          </form>

          {/* Demo accounts section */}
          <div className="mt-8 border-t border-gray-200 pt-6">
            <div className="text-center text-sm text-gray-500 mb-4">
              <span className="bg-white px-2">Tài khoản demo</span>
            </div>
            <div className="grid grid-cols-1 gap-2">
              {demoAccounts.map((account, index) => (
                <button
                  key={index}
                  onClick={() =>
                    fillDemoAccount(account.email, account.password)
                  }
                  className="text-left px-3 py-2 bg-gray-50 hover:bg-gray-100 rounded-lg transition-colors text-xs"
                  type="button"
                >
                  <div className="font-medium text-gray-700">
                    {account.role}
                  </div>
                  <div className="text-gray-500">{account.email}</div>
                </button>
              ))}
            </div>
          </div>

          {/* Footer với roles */}
          <div className="mt-6 text-center">
            <div className="text-sm text-gray-500 mb-3">
              Dành cho các vai trò:
            </div>
            <div className="flex flex-wrap justify-center gap-2">
              <span className="px-3 py-1 bg-blue-100 text-blue-800 rounded-full text-xs font-medium">
                Admin
              </span>
              <span className="px-3 py-1 bg-green-100 text-green-800 rounded-full text-xs font-medium">
                Dispatcher
              </span>
              <span className="px-3 py-1 bg-purple-100 text-purple-800 rounded-full text-xs font-medium">
                Fleet Manager
              </span>
              <span className="px-3 py-1 bg-orange-100 text-orange-800 rounded-full text-xs font-medium">
                Driver
              </span>
              <span className="px-3 py-1 bg-red-100 text-red-800 rounded-full text-xs font-medium">
                Operations
              </span>
            </div>
          </div>
        </div>

        {/* Copyright */}
        <div className="text-center mt-8 text-sm text-gray-500">
          © 2025 KTC Logistics. All rights reserved.
        </div>
      </div>
    </div>
  );
}
