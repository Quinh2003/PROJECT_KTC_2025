"use client";

import { useState, useEffect, useRef } from "react";
import { useRouter, useSearchParams } from "next/navigation";
import TrackingTabs from "./components/TrackingTabs.tsx";
import Footer from "./components/Footer";
import Header from "./components/Header";
import Slider from "./components/Slider";

export default function PublicHome() {
  const router = useRouter();
  const searchParams = useSearchParams();
  const trackingTabsRef = useRef<HTMLDivElement>(null);

  const [isLoggedIn, setIsLoggedIn] = useState(false);

  // Get tab navigation from URL parameters
  const tabParam = searchParams.get("tab");
  const subTabParam = searchParams.get("subtab");

  const getInitialTab = (): "tracking" | "services" => {
    if (tabParam === "services") return "services";
    return "tracking";
  };

  const getInitialSubTab = (): "trackingOrder" | "estimateFee" => {
    if (subTabParam === "estimate") return "estimateFee";
    return "trackingOrder";
  };

  // Function to scroll to TrackingTabs section
  const scrollToTrackingTabs = () => {
    setTimeout(() => {
      if (trackingTabsRef.current) {
        trackingTabsRef.current.scrollIntoView({
          behavior: "smooth",
          block: "start",
        });
      }
    }, 100); // Small delay to ensure DOM is updated
  };

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

    // Auto scroll to TrackingTabs if tab or subtab parameters are present
    if (tabParam || subTabParam) {
      scrollToTrackingTabs();
    }
  }, [searchParams, router, tabParam, subTabParam]);

  const handleLogin = () => router.push("/login");
  const handleSignIn = () => router.push("/register");
  const handleDashboard = () => router.push("/account");
  const handleCreateOrder = () =>
    isLoggedIn ? router.push("/account/orders/new") : handleLogin();
  const handleGoHome = () => router.push("/");
  const handleNavigateToServices = () => {
    router.push("/?tab=services");
    scrollToTrackingTabs();
  };
  const handleNavigateToEstimate = () => {
    router.push("/?tab=tracking&subtab=estimate");
    scrollToTrackingTabs();
  };
  const handleLogout = () => {
    localStorage.removeItem("token");
    localStorage.removeItem("user");
    setIsLoggedIn(false);
  };

  return (
    <div className="flex flex-col min-h-screen w-full bg-gray-50">
      {/* Header */}
      <Header
        isLoggedIn={isLoggedIn}
        onLogin={handleLogin}
        onSignIn={handleSignIn}
        onDashboard={handleDashboard}
        onCreateOrder={handleCreateOrder}
        onGoHome={handleGoHome}
        onLogout={handleLogout}
        onNavigateToServices={handleNavigateToServices}
        onNavigateToEstimate={handleNavigateToEstimate}
      />

      {/* Slider Hero Section - Right under header */}
      <div className="mt-30">
        <Slider onCreateOrder={handleCreateOrder} onLogin={handleLogin} />
      </div>

      {/* Main Content */}
      <main className="flex-1 bg-white">
        {/* Content Section */}
        <div className="flex flex-col items-center justify-start px-4 sm:px-6 lg:px-12 py-8 sm:py-12">
          {/* Hero */}
          <div className="text-center mb-8 sm:mb-12 max-w-2xl">
            <h1 className="text-2xl sm:text-3xl lg:text-4xl font-bold text-green-700 mb-4">
              Fast and reliable delivery service
            </h1>
            <p className="text-base sm:text-lg lg:text-xl text-gray-700">
              Track your order or create a new one today
            </p>
          </div>

          {/* Tracking Tabs */}
          <div ref={trackingTabsRef} className="mb-12">
            <TrackingTabs
              initialTrackingCode={searchParams.get("trackingCode") || ""}
              initialTab={getInitialTab()}
              initialSubTab={getInitialSubTab()}
            />
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
        </div>
      </main>

      {/* Footer */}
      <Footer />
    </div>
  );
}

/* --- Small reusable components --- */
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
