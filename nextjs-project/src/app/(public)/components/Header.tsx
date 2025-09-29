"use client";

import { useState } from "react";
import { useRouter } from "next/navigation";

interface HeaderProps {
  isLoggedIn: boolean;
  onLogin: () => void;
  onSignIn: () => void;
  onDashboard: () => void;
  onCreateOrder: () => void;
  onLogout: () => void;
  onGoHome?: () => void;
  onNavigateToServices?: () => void;
  onNavigateToEstimate?: () => void;
}

export default function Header({
  isLoggedIn,
  onLogin,
  onSignIn,
  onDashboard,
  onCreateOrder,
  onLogout,
  onGoHome,
  onNavigateToServices,
  onNavigateToEstimate,
}: HeaderProps) {
  const router = useRouter();
  const [isMobileMenuOpen, setIsMobileMenuOpen] = useState(false);

  const navigationItems = [
    { name: "Home", href: "/", onClick: onGoHome },
    {
      name: "Estimate Fee",
      href: "/?tab=tracking&subtab=estimate",
      onClick: onNavigateToEstimate,
    },
    {
      name: "Services",
      href: "/?tab=services",
      onClick: onNavigateToServices,
    },
    { name: "About Us", href: "/about" },
  ];

  return (
    <header className="fixed top-0 left-0 right-0 z-50 bg-white shadow-md">
      {/* Top Header */}
      <div className="bg-green-700">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex justify-between items-center h-16">
            {/* Logo */}
            <div className="flex items-center">
              <span className="text-xl font-bold text-white">Fast Route</span>
            </div>

            {/* Right Side Actions */}
            <div className="flex items-center gap-4">
              {/* User Actions */}
              {isLoggedIn ? (
                <div className="flex items-center gap-2">
                  <button
                    onClick={onDashboard}
                    className="text-white px-3 sm:px-4 py-2 rounded-lg hover:bg-green-800 text-sm sm:text-base"
                  >
                    Account
                  </button>
                  <button
                    onClick={onCreateOrder}
                    className="bg-white text-green-700 px-3 sm:px-4 py-2 rounded-lg hover:bg-green-100 text-sm sm:text-base"
                  >
                    Create Order
                  </button>
                  <button
                    onClick={onLogout}
                    className="border border-white text-white px-3 sm:px-4 py-2 rounded-lg hover:bg-green-800 text-sm sm:text-base"
                  >
                    Logout
                  </button>
                </div>
              ) : (
                <div className="flex items-center gap-2">
                  <button
                    onClick={onLogin}
                    className="bg-white text-green-700 px-3 sm:px-4 py-2 rounded-lg hover:bg-green-100 text-sm sm:text-base"
                  >
                    Login
                  </button>
                  <button
                    onClick={onSignIn}
                    className="border border-white text-white px-3 sm:px-4 py-2 rounded-lg hover:bg-green-800 text-sm sm:text-base"
                  >
                    Register
                  </button>
                </div>
              )}
            </div>
          </div>
        </div>
      </div>

      {/* Navigation Menu */}
      <div className="bg-white border-b">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex items-center justify-center h-14 w-full">
            {/* Desktop Navigation */}
            <nav className="hidden lg:flex items-center justify-center space-x-12 w-full max-w-4xl">
              {navigationItems.map((item) => (
                <div key={item.name} className="relative group">
                  <button
                    onClick={() => {
                      if (item.onClick) {
                        item.onClick();
                      } else {
                        router.push(item.href);
                      }
                    }}
                    className="flex items-center gap-1 text-gray-700 hover:text-white hover:bg-green-700 font-medium transition-all duration-200 px-4 h-14"
                  >
                    {item.name}
                  </button>
                </div>
              ))}
            </nav>

            {/* Mobile menu button */}
            <div className="lg:hidden absolute right-4">
              <button
                onClick={() => setIsMobileMenuOpen(!isMobileMenuOpen)}
                className="text-gray-700 hover:text-green-700 p-2"
              >
                <svg
                  className="w-6 h-6"
                  fill="none"
                  stroke="currentColor"
                  viewBox="0 0 24 24"
                >
                  <path
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    strokeWidth={2}
                    d="M4 6h16M4 12h16M4 18h16"
                  />
                </svg>
              </button>
            </div>
          </div>

          {/* Mobile Navigation */}
          {isMobileMenuOpen && (
            <div className="lg:hidden bg-white border-t">
              <div className="py-4 space-y-2">
                {navigationItems.map((item) => (
                  <button
                    key={item.name}
                    onClick={() => {
                      if (item.onClick) {
                        item.onClick();
                      } else {
                        router.push(item.href);
                      }
                      setIsMobileMenuOpen(false);
                    }}
                    className="block w-full text-left px-4 py-3 text-gray-700 hover:text-white hover:bg-green-700 transition-all duration-200"
                  >
                    {item.name}
                  </button>
                ))}
              </div>
            </div>
          )}
        </div>
      </div>
    </header>
  );
}
