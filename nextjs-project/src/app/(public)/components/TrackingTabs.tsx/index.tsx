"use client";

import { useState, useEffect } from "react";
import TrackingOrder from "./TrackingOrder";
import EstimateFee from "./EstimateFee";

interface TrackingTabsProps {
  initialTrackingCode?: string;
  initialTab?: "tracking" | "services";
  initialSubTab?: "trackingOrder" | "estimateFee";
}

export default function TrackingTabs({
  initialTrackingCode = "",
  initialTab = "tracking",
  initialSubTab = "trackingOrder",
}: TrackingTabsProps) {
  const [activeMainTab, setActiveMainTab] = useState<"tracking" | "services">(
    initialTab
  );
  const [activeSubTab, setActiveSubTab] = useState<
    "trackingOrder" | "estimateFee"
  >(initialSubTab);

  // Update tabs when props change
  useEffect(() => {
    setActiveMainTab(initialTab);
    setActiveSubTab(initialSubTab);
  }, [initialTab, initialSubTab]);

  const mainTabs = [
    {
      id: "tracking" as const,
      label: "Tracking",
      active: activeMainTab === "tracking",
    },
    {
      id: "services" as const,
      label: "Services",
      active: activeMainTab === "services",
    },
  ];

  const subTabs = [
    { id: "trackingOrder" as const, label: "Track Package", icon: "📦" },
    { id: "estimateFee" as const, label: "Estimate Fee", icon: "💰" },
  ];

  return (
    <div className="w-full max-w-6xl mx-auto">
      {/* Main Tabs */}
      <div className="flex bg-white rounded-t-xl shadow-sm border-b">
        {mainTabs.map((tab) => (
          <button
            key={tab.id}
            onClick={() => setActiveMainTab(tab.id)}
            className={`flex-1 px-8 py-4 text-lg font-semibold transition-all duration-200 ${
              tab.active
                ? "bg-green-700 text-white rounded-t-xl"
                : "text-gray-600 hover:text-green-700 hover:bg-gray-50"
            }`}
          >
            {tab.label}
          </button>
        ))}
      </div>

      {/* Content Area */}
      <div className="bg-white rounded-b-xl shadow-lg">
        {activeMainTab === "tracking" && (
          <div>
            {/* Sub Tabs for Tracking */}
            <div className="flex border-b bg-gray-50">
              {subTabs.map((tab) => (
                <button
                  key={tab.id}
                  onClick={() => setActiveSubTab(tab.id)}
                  className={`flex items-center gap-2 px-6 py-3 text-sm font-medium transition-all duration-200 ${
                    activeSubTab === tab.id
                      ? "bg-white text-green-700 border-b-2 border-green-700 -mb-px"
                      : "text-gray-600 hover:text-green-700 hover:bg-gray-100"
                  }`}
                >
                  <span>{tab.icon}</span>
                  {tab.label}
                </button>
              ))}
            </div>

            {/* Sub Tab Content */}
            <div className="p-6 w-full">
              {activeSubTab === "trackingOrder" && (
                <TrackingOrder initialTrackingCode={initialTrackingCode} />
              )}
              {activeSubTab === "estimateFee" && <EstimateFee />}
            </div>
          </div>
        )}

        {activeMainTab === "services" && (
          <div className="p-8 w-full">
            <div className="text-center py-16 w-full max-w-4xl mx-auto">
              <h3 className="text-2xl font-bold text-gray-700 mb-4">
                Shipping Services
              </h3>
              <p className="text-gray-600 mb-8">
                Discover our diverse shipping services
              </p>

              {/* Service Grid */}
              <div className="grid grid-cols-1 md:grid-cols-2 gap-8 w-full max-w-4xl mx-auto">
                <ServiceCard
                  icon="🚚"
                  title="Express Delivery"
                  description="Fast shipping service for all types of goods and documents with quick delivery times."
                />
                <ServiceCard
                  icon="⚡"
                  title="Priority Express"
                  description="Ultra-fast shipping service with the highest priority for urgent goods and documents."
                />
                <ServiceCard
                  icon="📋"
                  title="Standard Delivery"
                  description="Standard shipping service for goods and documents with reasonable pricing and delivery times."
                />
                <ServiceCard
                  icon="📦"
                  title="Heavy Cargo"
                  description="Specialized shipping service for heavy items weighing 15kg and above at competitive rates."
                />
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

// Service Card Component
const ServiceCard = ({
  icon,
  title,
  description,
}: {
  icon: string;
  title: string;
  description: string;
}) => (
  <div className="bg-white border border-gray-200 rounded-xl p-6 hover:shadow-lg transition-shadow duration-200">
    <div className="text-4xl mb-4">{icon}</div>
    <h4 className="text-lg font-semibold text-gray-800 mb-3">{title}</h4>
    <p className="text-gray-600 text-sm leading-relaxed">{description}</p>
    <button className="mt-4 text-green-700 font-medium hover:text-green-800 transition-colors">
      Details →
    </button>
  </div>
);
