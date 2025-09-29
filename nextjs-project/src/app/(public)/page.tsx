"use client";

import { useState, useEffect } from "react";
import { useRouter, useSearchParams } from "next/navigation";
import { getOrderTrackingApi } from "../../server/order.api";

const formatDate = (isoString: string) =>
  isoString
    ? new Date(isoString).toLocaleString("vi-VN", {
        year: "numeric",
        month: "2-digit",
        day: "2-digit",
        hour: "2-digit",
        minute: "2-digit",
      })
    : "";

export default function PublicHome() {
  const router = useRouter();
  const searchParams = useSearchParams();

  const [trackingCode, setTrackingCode] = useState("");
  const [trackingResult, setTrackingResult] = useState<{
    code: string;
    status: string;
    from: string;
    to: string;
    estimatedDelivery: string;
  } | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const [isLoggedIn, setIsLoggedIn] = useState(false);

  useEffect(() => {
    const token = localStorage.getItem("token");
    setIsLoggedIn(!!token);

    const urlToken = searchParams.get("token");
    const urlUser = searchParams.get("user");
    if (urlToken && urlUser) {
      localStorage.setItem("token", urlToken);
      localStorage.setItem("user", decodeURIComponent(urlUser));
      setIsLoggedIn(true);
      window.history.replaceState({}, document.title, "/");
      router.push("/account");
    }

    const urlTrackingCode = searchParams.get("trackingCode");
    if (urlTrackingCode) setTrackingCode(urlTrackingCode);
  }, [searchParams, router]);

  const handleTrackingSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!trackingCode.trim()) {
      setTrackingResult(null); // clear luôn nếu không nhập gì
      return;
    }
    setIsLoading(true);
    try {
      const res = await getOrderTrackingApi(trackingCode);
      if (!res.ok) {
        setTrackingResult(null);
        alert("Order not found!");
      } else {
        const order = await res.json();
        setTrackingResult({
          code: order.orderId,
          status: order.status || "Unknown",
          from: order.storeAddress || "",
          to: order.address || "",
          estimatedDelivery: order.estimatedDelivery || "",
        });
      }
    } catch (error) {
      console.error("Error tracking order:", error);
      setTrackingResult(null); // clear nếu lỗi
      alert("An error occurred while tracking the order!");
    } finally {
      setIsLoading(false);
    }
  };

  const handleLogin = () => router.push("/login");
  const handleDashboard = () => router.push("/account");
  const handleCreateOrder = () =>
    isLoggedIn ? router.push("/account/orders/new") : handleLogin();
  const handleLogout = () => {
    localStorage.removeItem("token");
    localStorage.removeItem("user");
    setIsLoggedIn(false);
    setTrackingResult(null);
  };

  return (
    <div className="flex flex-col min-h-screen w-full bg-gray-50">
      {/* Header */}
      <header className="fixed top-0 left-0 right-0 z-50 bg-green-700 shadow-md bg-opacity-90 backdrop-blur-md h-16 flex items-center px-4 sm:px-6 lg:px-8">
        <div className="flex justify-between items-center w-full">
          <span className="ml-2 text-xl font-bold text-white">Fast Route</span>
          <div className="flex items-center gap-4">
            {isLoggedIn ? (
              <>
                <button
                  onClick={handleDashboard}
                  className="text-white px-3 sm:px-4 py-2 rounded-lg hover:bg-green-800 text-sm sm:text-base"
                >
                  Account
                </button>
                <button
                  onClick={handleCreateOrder}
                  className="bg-white text-green-700 px-3 sm:px-4 py-2 rounded-lg hover:bg-green-100 text-sm sm:text-base"
                >
                  Create Order
                </button>
                <button
                  onClick={handleLogout}
                  className="border border-gray-300 text-green-700 px-3 sm:px-4 py-2 rounded-lg hover:bg-green-50 text-sm sm:text-base"
                >
                  Logout
                </button>
              </>
            ) : (
              <>
                <button
                  onClick={handleCreateOrder}
                  className="bg-white text-green-700 px-3 sm:px-4 py-2 rounded-lg hover:bg-green-100 text-sm sm:text-base"
                >
                  Create Order
                </button>
                <button
                  onClick={handleLogin}
                  className="border border-white text-white px-3 sm:px-4 py-2 rounded-lg hover:bg-green-800 text-sm sm:text-base"
                >
                  Login
                </button>
              </>
            )}
          </div>
        </div>
      </header>

      {/* Main Content */}
      <main className="flex-1 flex flex-col items-center justify-start mt-16 px-4 sm:px-6 lg:px-12 py-8 sm:py-12 bg-white">
        {/* Hero */}
        <div className="text-center mb-8 sm:mb-12 max-w-2xl">
          <h1 className="text-2xl sm:text-3xl lg:text-4xl font-bold text-green-700 mb-4">
            Fast and reliable delivery service
          </h1>
          <p className="text-base sm:text-lg lg:text-xl text-gray-700">
            Track your order or create a new one today
          </p>
        </div>

        {/* Tracking */}
        <div className="bg-white rounded-2xl shadow-lg p-6 sm:p-8 mb-12 border border-gray-200 w-full max-w-full sm:max-w-xl lg:max-w-3xl">
          <h2 className="text-lg sm:text-2xl font-bold text-green-700 mb-6 text-center">
            Track your order
          </h2>
          <form onSubmit={handleTrackingSubmit} className="space-y-4">
            <div className="flex flex-col sm:flex-row gap-4">
              <input
                type="text"
                value={trackingCode}
                onChange={(e) => {
                  const val = e.target.value;
                  setTrackingCode(val);
                  if (!val.trim()) setTrackingResult(null); // clear khi xoá hết
                }}
                placeholder="Enter tracking code (e.g. FR001, FR002...)"
                className="flex-1 px-4 py-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-green-700 text-sm sm:text-base"
              />
              <button
                type="submit"
                disabled={isLoading}
                className="bg-green-700 text-white px-6 sm:px-8 py-3 rounded-lg hover:bg-green-800 disabled:opacity-50 text-sm sm:text-base"
              >
                {isLoading ? "Searching..." : "Track"}
              </button>
            </div>
          </form>

          {trackingResult && (
            <div className="mt-8 p-6 bg-green-50 border border-green-200 rounded-lg text-sm sm:text-base">
              <h3 className="text-lg font-semibold text-green-800 mb-4">
                Order information: {trackingResult.code}
              </h3>
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <Info label="Status" value={trackingResult.status} />
                <Info
                  label="Estimated delivery"
                  value={formatDate(trackingResult.estimatedDelivery)}
                />
                <Info label="From" value={trackingResult.from} />
                <Info label="To" value={trackingResult.to} />
              </div>
            </div>
          )}
        </div>

        {/* Features */}
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-6 sm:gap-8 mb-12 w-full">
          <Feature
            icon="🚚"
            title="Express delivery"
            text="Guaranteed delivery within 24 hours"
          />
          <Feature
            icon="📍"
            title="Real-time tracking"
            text="Track your order live on the map"
          />
          <Feature
            icon="🔒"
            title="Secure & safe"
            text="Ensuring your package is delivered to the right recipient"
          />
        </div>

        {/* Call to Action */}
        <div className="bg-gradient-to-r from-green-700 to-green-900 rounded-2xl p-6 sm:p-8 text-center text-white w-full">
          <h2 className="text-2xl sm:text-3xl font-bold mb-4">
            Start shipping today
          </h2>
          <p className="text-lg sm:text-xl mb-6">
            Sign up to experience the best service
          </p>
          <div className="flex flex-col sm:flex-row gap-4 justify-center">
            <button
              onClick={handleCreateOrder}
              className="bg-white text-green-700 px-6 sm:px-8 py-3 rounded-lg font-semibold hover:bg-green-100 text-sm sm:text-base"
            >
              Create order now
            </button>
            <button
              onClick={handleLogin}
              className="border-2 border-white px-6 sm:px-8 py-3 rounded-lg font-semibold hover:bg-green-800 text-sm sm:text-base"
            >
              Login / Register
            </button>
          </div>
        </div>
      </main>

      {/* Footer */}
      <footer className="bg-black/80 text-white py-12 mt-16 rounded-t-2xl backdrop-blur-md">
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-8 max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
          <FooterCol
            title="Fast Route"
            items={["Fast and reliable delivery service"]}
            highlight
          />
          <FooterCol
            title="Services"
            items={[
              "Local delivery",
              "Interprovincial delivery",
              "International delivery",
            ]}
          />
          <FooterCol
            title="Support"
            items={[
              "Hotline: 1900-xxxx",
              "Email: support@fastroute.com",
              "FAQ",
            ]}
          />
          <FooterCol
            title="Follow us"
            items={["Facebook", "Instagram", "LinkedIn"]}
          />
        </div>
        <div className="border-t border-gray-700 mt-8 pt-8 text-center text-gray-400 text-xs sm:text-sm">
          &copy; 2025 Fast Route. All rights reserved.
        </div>
      </footer>
    </div>
  );
}

/* --- Small reusable components --- */
const Info = ({ label, value }: { label: string; value: string }) => (
  <div>
    <p className="text-xs sm:text-sm text-gray-700">{label}:</p>
    <p className="font-semibold text-green-700">{value}</p>
  </div>
);

const Feature = ({
  icon,
  title,
  text,
}: {
  icon: string;
  title: string;
  text: string;
}) => (
  <div className="text-center p-6 bg-white rounded-xl shadow-lg">
    <div className="w-14 h-14 sm:w-16 sm:h-16 bg-green-100 rounded-full flex items-center justify-center mx-auto mb-4 text-xl sm:text-2xl">
      {icon}
    </div>
    <h3 className="text-lg sm:text-xl font-semibold mb-2 text-green-700">
      {title}
    </h3>
    <p className="text-gray-700 text-sm sm:text-base">{text}</p>
  </div>
);

const FooterCol = ({
  title,
  items,
  highlight = false,
}: {
  title: string;
  items: string[];
  highlight?: boolean;
}) => (
  <div>
    <h4
      className={`font-semibold mb-4 ${
        highlight ? "text-base sm:text-lg text-green-700" : "text-green-700"
      }`}
    >
      {title}
    </h4>
    <ul className="space-y-2 text-gray-400 text-sm sm:text-base">
      {items.map((item, idx) => (
        <li key={idx}>{item}</li>
      ))}
    </ul>
  </div>
);
