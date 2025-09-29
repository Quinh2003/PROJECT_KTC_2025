"use client";

import { useRouter } from "next/navigation";
import Header from "../components/Header";
import Footer from "../components/Footer";

export default function AboutUs() {
  const router = useRouter();

  const handleGoHome = () => {
    router.push("/");
  };

  const handleNavigateToServices = () => {
    router.push("/?tab=services");
  };

  const handleNavigateToEstimate = () => {
    router.push("/?tab=tracking&subtab=estimate");
  };

  const handleLogin = () => {
    router.push("/login");
  };

  const handleSignIn = () => {
    router.push("/register");
  };

  const handleDashboard = () => {
    router.push("/dashboard");
  };

  const handleCreateOrder = () => {
    router.push("/dashboard/orders/create");
  };

  const handleLogout = () => {
    localStorage.removeItem("user");
    localStorage.removeItem("token");
    router.push("/");
  };

  return (
    <div className="min-h-screen flex flex-col bg-gray-50">
      <Header
        isLoggedIn={false}
        onLogin={handleLogin}
        onSignIn={handleSignIn}
        onDashboard={handleDashboard}
        onCreateOrder={handleCreateOrder}
        onLogout={handleLogout}
        onGoHome={handleGoHome}
        onNavigateToServices={handleNavigateToServices}
        onNavigateToEstimate={handleNavigateToEstimate}
      />

      {/* Main Content */}
      <main className="flex-1 pt-32 pb-16">
        <div className="max-w-4xl mx-auto px-4 sm:px-6 lg:px-8">
          {/* Header Section */}
          <div className="text-center mb-16">
            <h1 className="text-4xl sm:text-5xl font-bold text-gray-900 mb-6">
              About <span className="text-green-700">FastRoute</span>
            </h1>
            <p className="text-xl text-gray-600 leading-relaxed max-w-3xl mx-auto">
              Your trusted partner for all shipping and delivery needs
            </p>
          </div>

          {/* Hero Image */}
          <div className="mb-16">
            <div className="bg-gradient-to-r from-green-700 to-green-500 rounded-2xl h-96 flex items-center justify-center shadow-lg">
              <div className="text-center text-white">
                <div className="text-8xl mb-4">🚚</div>
                <h2 className="text-3xl font-bold mb-2">FastRoute</h2>
                <p className="text-xl opacity-90">Connect - Fast - Reliable</p>
              </div>
            </div>
          </div>

          {/* Story Section */}
          <div className="prose prose-lg max-w-none mb-16">
            <div className="bg-white rounded-2xl p-8 sm:p-12 shadow-lg">
              <h2 className="text-3xl font-bold text-gray-900 mb-8 text-center">
                Our Story
              </h2>

              <div className="space-y-6 text-gray-700 leading-relaxed">
                <p className="text-lg">
                  <strong className="text-green-700">FastRoute</strong> was born
                  from the ambition to revolutionize the transportation industry
                  in Vietnam. With the rapid growth of e-commerce and the
                  increasing demand for deliveries, we realized the market
                  needed a smarter, faster, and more reliable shipping solution.
                </p>

                <p>
                  Founded by a team of experts with years of experience in
                  logistics and technology, FastRoute is not just a regular
                  shipping company. We are pioneers in applying advanced
                  technologies such as AI, Machine Learning, and IoT to optimize
                  routes, minimize delivery time, and enhance customer
                  experience.
                </p>

                <p>
                  <strong>Our Vision</strong> is to become the leading shipping
                  platform in Vietnam, connecting every corner of the country
                  through an intelligent logistics network. We are committed to
                  providing the fastest, safest, and most affordable delivery
                  service for everyone, from individuals to large enterprises.
                </p>

                <p>
                  <strong>Our Mission</strong> is to make sending and receiving
                  goods as simple as a single click. With the slogan
                  &ldquo;Connect – Fast – Reliable,&rdquo; we continuously
                  innovate and improve to meet every expectation of our
                  customers.
                </p>
              </div>
            </div>
          </div>

          {/* Features Section */}
          <div className="mb-16">
            <h2 className="text-3xl font-bold text-center text-gray-900 mb-12">
              Why Choose FastRoute?
            </h2>

            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
              <div className="bg-white rounded-xl p-6 shadow-lg text-center">
                <div className="text-4xl mb-4">⚡</div>
                <h3 className="text-xl font-semibold text-gray-900 mb-3">
                  Super-fast Delivery
                </h3>
                <p className="text-gray-600">
                  With AI-powered route optimization, we ensure the fastest
                  delivery possible.
                </p>
              </div>

              <div className="bg-white rounded-xl p-6 shadow-lg text-center">
                <div className="text-4xl mb-4">🛡️</div>
                <h3 className="text-xl font-semibold text-gray-900 mb-3">
                  Absolute Safety
                </h3>
                <p className="text-gray-600">
                  Our 5-layer security system and comprehensive insurance ensure
                  your goods are always fully protected.
                </p>
              </div>

              <div className="bg-white rounded-xl p-6 shadow-lg text-center">
                <div className="text-4xl mb-4">💰</div>
                <h3 className="text-xl font-semibold text-gray-900 mb-3">
                  Affordable Pricing
                </h3>
                <p className="text-gray-600">
                  Transparent, competitive pricing structure with exclusive
                  benefits for loyal customers.
                </p>
              </div>

              <div className="bg-white rounded-xl p-6 shadow-lg text-center">
                <div className="text-4xl mb-4">📱</div>
                <h3 className="text-xl font-semibold text-gray-900 mb-3">
                  Modern Technology
                </h3>
                <p className="text-gray-600">
                  User-friendly mobile and web platforms with real-time order
                  tracking 24/7.
                </p>
              </div>

              <div className="bg-white rounded-xl p-6 shadow-lg text-center">
                <div className="text-4xl mb-4">🌍</div>
                <h3 className="text-xl font-semibold text-gray-900 mb-3">
                  Nationwide Coverage
                </h3>
                <p className="text-gray-600">
                  Delivery network covering the entire country with over 1,000
                  service points.
                </p>
              </div>

              <div className="bg-white rounded-xl p-6 shadow-lg text-center">
                <div className="text-4xl mb-4">🎯</div>
                <h3 className="text-xl font-semibold text-gray-900 mb-3">
                  Quality Commitment
                </h3>
                <p className="text-gray-600">
                  24/7 customer support team committed to resolving any issue
                  within 2 hours.
                </p>
              </div>
            </div>
          </div>

          {/* Stats Section */}
          <div className="bg-green-700 rounded-2xl p-8 sm:p-12 text-white mb-16">
            <h2 className="text-3xl font-bold text-center mb-12">
              FastRoute in Numbers
            </h2>

            <div className="grid grid-cols-2 lg:grid-cols-4 gap-8 text-center">
              <div>
                <div className="text-4xl font-bold mb-2">50K+</div>
                <div className="text-green-100">Trusted Customers</div>
              </div>
              <div>
                <div className="text-4xl font-bold mb-2">1M+</div>
                <div className="text-green-100">Orders Delivered</div>
              </div>
              <div>
                <div className="text-4xl font-bold mb-2">63/63</div>
                <div className="text-green-100">Provinces/Cities</div>
              </div>
              <div>
                <div className="text-4xl font-bold mb-2">99.8%</div>
                <div className="text-green-100">Successful Delivery Rate</div>
              </div>
            </div>
          </div>

          {/* Team Section */}
          <div className="text-center mb-16">
            <h2 className="text-3xl font-bold text-gray-900 mb-8">
              Our Team
            </h2>
            <p className="text-lg text-gray-600 max-w-3xl mx-auto mb-12">
              FastRoute is built by a team of experienced professionals, always
              putting customers at the center of every decision.
            </p>

            <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
              <div className="bg-white rounded-xl p-6 shadow-lg">
                <div className="w-20 h-20 bg-green-100 rounded-full mx-auto mb-4 flex items-center justify-center">
                  <span className="text-2xl">👨‍💼</span>
                </div>
                <h3 className="text-xl font-semibold text-gray-900 mb-2">
                  Leadership Team
                </h3>
                <p className="text-gray-600">
                  Our leaders have over 15 years of experience in logistics and
                  technology.
                </p>
              </div>

              <div className="bg-white rounded-xl p-6 shadow-lg">
                <div className="w-20 h-20 bg-green-100 rounded-full mx-auto mb-4 flex items-center justify-center">
                  <span className="text-2xl">👨‍💻</span>
                </div>
                <h3 className="text-xl font-semibold text-gray-900 mb-2">
                  Tech Team
                </h3>
                <p className="text-gray-600">
                  A talented technical team, constantly innovating to bring the
                  best experience.
                </p>
              </div>

              <div className="bg-white rounded-xl p-6 shadow-lg">
                <div className="w-20 h-20 bg-green-100 rounded-full mx-auto mb-4 flex items-center justify-center">
                  <span className="text-2xl">🚚</span>
                </div>
                <h3 className="text-xl font-semibold text-gray-900 mb-2">
                  Operations Team
                </h3>
                <p className="text-gray-600">
                  A professional operations team ensuring every order is handled
                  optimally.
                </p>
              </div>
            </div>
          </div>

          {/* CTA Section */}
          <div className="bg-gradient-to-r from-green-600 to-green-400 rounded-2xl p-8 sm:p-12 text-center text-white">
            <h2 className="text-3xl font-bold mb-4">
              Ready to experience FastRoute?
            </h2>
            <p className="text-xl mb-8 opacity-90">
              Let us accompany you on your journey to business growth
            </p>
            <div className="flex flex-col sm:flex-row gap-4 justify-center">
              <button
                onClick={handleNavigateToEstimate}
                className="bg-white text-green-600 px-8 py-3 rounded-lg font-semibold hover:bg-gray-100 transition-colors"
              >
                Estimate Shipping Fee
              </button>
              <button
                onClick={handleLogin}
                className="border-2 border-white text-white px-8 py-3 rounded-lg font-semibold hover:bg-white hover:text-green-600 transition-colors"
              >
                Sign Up Now
              </button>
            </div>
          </div>
        </div>
      </main>

      <Footer />
    </div>
  );
}
