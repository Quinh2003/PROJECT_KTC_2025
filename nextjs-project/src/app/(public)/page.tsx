"use client";

import { useState, useEffect } from "react";
import { getOrderByTrackingCodeApi } from "../../server/order.api";
import { useRouter, useSearchParams } from "next/navigation";

export default function PublicHome() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const [trackingCode, setTrackingCode] = useState("");
  const [trackingResult, setTrackingResult] = useState<{
    code: string;
    status: string;
    from: string;
    to: string;
    currentLocation: string;
    estimatedDelivery: string;
  } | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [isLoggedIn, setIsLoggedIn] = useState(false);

  useEffect(() => {
    if (typeof window !== "undefined") {
      // Check if user is logged in
      const token = localStorage.getItem("token");
      setIsLoggedIn(!!token);

      // Check if redirected from ReactJS login with token
      const urlToken = searchParams.get("token");
      const urlUser = searchParams.get("user");
      
      if (urlToken && urlUser) {
        try {
          // Store authentication data
          localStorage.setItem("token", urlToken);
          localStorage.setItem("user", decodeURIComponent(urlUser));
          setIsLoggedIn(true);
          // Clean URL and redirect to account dashboard
          window.history.replaceState({}, document.title, "/");
          router.push("/account");
        } catch (error) {
          console.error("Error processing authentication:", error);
        }
      }

      // Auto-fill tracking code if provided in URL
      const urlTrackingCode = searchParams.get("trackingCode");
      if (urlTrackingCode) {
        setTrackingCode(urlTrackingCode);
      }
    }
  }, [searchParams, router]);

  const handleTrackingSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!trackingCode.trim()) return;

    setIsLoading(true);
    try {
      const res = await getOrderByTrackingCodeApi(trackingCode);
      if (!res.ok) {
        setTrackingResult(null);
        setIsLoading(false);
        alert("Order not found!");
        return;
      }
      const order = await res.json();
      setTrackingResult({
        code: order.id,
        status: order.status?.name || "Unknown",
        from: order.store?.address || "",
        to: order.address?.address || "",
        currentLocation: order.status?.statusType || "",
        estimatedDelivery: order.estimatedDelivery || ""
      });
      setIsLoading(false);
    } catch (error) {
      console.error("Error tracking order:", error);
      setIsLoading(false);
      alert("An error occurred while tracking the order!");
    }
  };

  const handleLoginRedirect = () => {
    // Redirect to Next.js login page
    router.push("/login");
  };

  const handleCreateOrder = () => {
    // Check if user is logged in, if not redirect to login
    if (!isLoggedIn) {
      alert("You need to log in to create an order!");
      handleLoginRedirect();
    } else {
      // Redirect to create order page
      router.push("/account/orders/new");
    }
  };

  const handleDashboard = () => {
    router.push("/account");
  };

  const handleLogout = () => {
    if (typeof window !== "undefined") {
      localStorage.removeItem("token");
      localStorage.removeItem("user");
    }
    setIsLoggedIn(false);
    setTrackingResult(null);
  };

  return (
    <div className="min-h-screen bg-white">
      {/* Header */}
      <header className="bg-green-700 shadow-md">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="flex justify-between items-center h-16">
            <div className="flex items-center">
              <span className="ml-2 text-xl font-bold text-white">Fast Route</span>
            </div>
            <div className="flex items-center gap-4">
              {isLoggedIn ? (
                <>
                  <button
                    onClick={handleDashboard}
                    className="text-white px-4 py-2 rounded-lg hover:bg-green-800 transition-colors"
                  >
                    Account
                  </button>
                  <button
                    onClick={handleCreateOrder}
                    className="bg-white text-green-700 px-4 py-2 rounded-lg hover:bg-green-100 transition-colors"
                  >
                    Create Order
                  </button>
                  <button
                    onClick={handleLogout}
                    className="border border-gray-300 text-green-700 px-4 py-2 rounded-lg hover:bg-green-50 transition-colors"
                  >
                    Logout
                  </button>
                </>
              ) : (
                <>
                  <button
                    onClick={handleCreateOrder}
                    className="bg-white text-green-700 px-4 py-2 rounded-lg hover:bg-green-100 transition-colors"
                  >
                    Create Order
                  </button>
                  <button
                    onClick={handleLoginRedirect}
                    className="border border-white text-white px-4 py-2 rounded-lg hover:bg-green-800 transition-colors"
                  >
                    Login
                  </button>
                </>
              )}
            </div>
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-12">
        {/* Hero Section */}
        <div className="text-center mb-12">
          <h1 className="text-4xl font-bold text-green-700 mb-4">
            Fast and reliable delivery service
          </h1>
          <p className="text-xl text-gray-700 mb-8">
            Track your order or create a new one today
          </p>
        </div>

        {/* Tracking Section */}
        <div className="bg-white rounded-2xl shadow-lg p-8 mb-12">
          <div className="max-w-2xl mx-auto">
            <h2 className="text-2xl font-bold text-green-700 mb-6 text-center">
              Track your order
            </h2>
            <form onSubmit={handleTrackingSubmit} className="space-y-4">
              <div className="flex gap-4">
                <input
                  type="text"
                  value={trackingCode}
                  onChange={(e) => setTrackingCode(e.target.value)}
                  placeholder="Enter tracking code (e.g. FR001, FR002...)"
                  className="flex-1 px-4 py-3 border border-gray-300 rounded-lg focus:outline-none focus:ring-2 focus:ring-green-700 focus:border-green-700"
                />
                <button
                  type="submit"
                  disabled={isLoading}
                  className="bg-green-700 text-white px-8 py-3 rounded-lg hover:bg-green-800 transition-colors disabled:opacity-50"
                >
                  {isLoading ? "Searching..." : "Track"}
                </button>
              </div>
            </form>

            {/* Tracking Result */}
            {trackingResult && (
              <div className="mt-8 p-6 bg-green-50 border border-green-200 rounded-lg">
                <h3 className="text-lg font-semibold text-green-800 mb-4">
                  Order information: {trackingResult.code}
                </h3>
                <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                  <div>
                    <p className="text-sm text-gray-700">Status:</p>
                    <p className="font-semibold text-green-700">{trackingResult.status}</p>
                  </div>
                  <div>
                    <p className="text-sm text-gray-700">Estimated delivery:</p>
                    <p className="font-semibold text-green-700">{trackingResult.estimatedDelivery}</p>
                  </div>
                  <div>
                    <p className="text-sm text-gray-700">From:</p>
                    <p className="font-semibold text-green-700">{trackingResult.from}</p>
                  </div>
                  <div>
                    <p className="text-sm text-gray-700">To:</p>
                    <p className="font-semibold text-green-700">{trackingResult.to}</p>
                  </div>
                  <div className="md:col-span-2">
                    <p className="text-sm text-gray-700">Current location:</p>
                    <p className="font-semibold text-green-700">{trackingResult.currentLocation}</p>
                  </div>
                </div>
              </div>
            )}
          </div>
        </div>

        {/* Features Section */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-8 mb-12">
          <div className="text-center p-6 bg-white rounded-xl shadow-lg">
            <div className="w-16 h-16 bg-green-100 rounded-full flex items-center justify-center mx-auto mb-4">
              <span className="text-2xl">🚚</span>
            </div>
            <h3 className="text-xl font-semibold mb-2 text-green-700">Express delivery</h3>
            <p className="text-gray-700">Guaranteed delivery within 24 hours</p>
          </div>
          <div className="text-center p-6 bg-white rounded-xl shadow-lg">
            <div className="w-16 h-16 bg-green-100 rounded-full flex items-center justify-center mx-auto mb-4">
              <span className="text-2xl">📍</span>
            </div>
            <h3 className="text-xl font-semibold mb-2 text-green-700">Real-time tracking</h3>
            <p className="text-gray-700">Track your order live on the map</p>
          </div>
          <div className="text-center p-6 bg-white rounded-xl shadow-lg">
            <div className="w-16 h-16 bg-green-100 rounded-full flex items-center justify-center mx-auto mb-4">
              <span className="text-2xl">🔒</span>
            </div>
            <h3 className="text-xl font-semibold mb-2 text-green-700">Secure & safe</h3>
            <p className="text-gray-700">Ensuring your package is delivered to the right recipient</p>
          </div>
        </div>

        {/* Call to Action */}
        <div className="bg-gradient-to-r from-green-700 to-green-900 rounded-2xl p-8 text-center text-white">
          <h2 className="text-3xl font-bold mb-4">Start shipping today</h2>
          <p className="text-xl mb-6">Sign up to experience the best service</p>
          <div className="flex gap-4 justify-center">
            <button
              onClick={handleCreateOrder}
              className="bg-white text-green-700 px-8 py-3 rounded-lg font-semibold hover:bg-green-100 transition-colors"
            >
              Create order now
            </button>
            <button
              onClick={handleLoginRedirect}
              className="border-2 border-white text-white px-8 py-3 rounded-lg font-semibold hover:bg-green-800 hover:text-white transition-colors"
            >
              Login / Register
            </button>
          </div>
        </div>
      </main>

      {/* Footer */}
      <footer className="bg-black text-white py-12 mt-16">
        <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <div className="grid grid-cols-1 md:grid-cols-4 gap-8">
            <div>
              <h3 className="text-lg font-semibold mb-4 text-green-700">Fast Route</h3>
              <p className="text-gray-400">Fast and reliable delivery service</p>
            </div>
            <div>
              <h4 className="font-semibold mb-4 text-green-700">Services</h4>
              <ul className="space-y-2 text-gray-400">
                <li>Local delivery</li>
                <li>Interprovincial delivery</li>
                <li>International delivery</li>
              </ul>
            </div>
            <div>
              <h4 className="font-semibold mb-4 text-green-700">Support</h4>
              <ul className="space-y-2 text-gray-400">
                <li>Hotline: 1900-xxxx</li>
                <li>Email: support@fastroute.com</li>
                <li>FAQ</li>
              </ul>
            </div>
            <div>
              <h4 className="font-semibold mb-4 text-green-700">Follow us</h4>
              <ul className="space-y-2 text-gray-400">
                <li>Facebook</li>
                <li>Instagram</li>
                <li>LinkedIn</li>
              </ul>
            </div>
          </div>
          <div className="border-t border-gray-700 mt-8 pt-8 text-center text-gray-400">
            <p>&copy; 2025 Fast Route. All rights reserved.</p>
          </div>
        </div>
      </footer>
    </div>
  );
}
