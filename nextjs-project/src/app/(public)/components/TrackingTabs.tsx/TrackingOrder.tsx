"use client";

import { useState } from "react";
import { getOrderTrackingApi } from "../../../../server/order.api";

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

interface TrackingResult {
  code: string;
  status: string;
  from: string;
  to: string;
  estimatedDelivery: string;
}

interface TrackingOrderProps {
  initialTrackingCode?: string;
}

export default function TrackingOrder({
  initialTrackingCode = "",
}: TrackingOrderProps) {
  const [trackingCode, setTrackingCode] = useState(initialTrackingCode);
  const [trackingResult, setTrackingResult] = useState<TrackingResult | null>(
    null
  );
  const [isLoading, setIsLoading] = useState(false);

  const handleTrackingSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!trackingCode.trim()) {
      setTrackingResult(null);
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
      setTrackingResult(null);
      alert("An error occurred while tracking the order!");
    } finally {
      setIsLoading(false);
    }
  };

  return (
    <div className="bg-white sm:p-8 w-4xl">
      <div className="w-full max-w-4xl mx-auto">
        <form onSubmit={handleTrackingSubmit} className="space-y-4">
          <div className="flex flex-col sm:flex-row gap-4">
            <input
              type="text"
              value={trackingCode}
              onChange={(e) => {
                const val = e.target.value;
                setTrackingCode(val);
                if (!val.trim()) setTrackingResult(null);
              }}
              placeholder="Enter tracking code (e.g., FR001, FR002...)"
              className="flex-1 px-4 py-3 border border-gray-300 rounded-lg focus:ring-2 focus:ring-green-700 focus:border-green-700 text-sm sm:text-base"
            />
            <button
              type="submit"
              disabled={isLoading}
              className="bg-green-700 text-white px-6 sm:px-8 py-3 rounded-lg hover:bg-green-800 disabled:opacity-50 transition-colors text-sm sm:text-base font-semibold"
            >
              {isLoading ? "Searching..." : "Track"}
            </button>
          </div>
        </form>

        {trackingResult && (
          <div className="mt-8 p-6 bg-green-50 border border-green-200 rounded-lg text-sm sm:text-base">
            <h3 className="text-lg font-semibold text-green-800 mb-4">
              Order Information: {trackingResult.code}
            </h3>
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <InfoItem label="Status" value={trackingResult.status} />
              <InfoItem
                label="Estimated Delivery"
                value={formatDate(trackingResult.estimatedDelivery)}
              />
              <InfoItem label="From" value={trackingResult.from} />
              <InfoItem label="To" value={trackingResult.to} />
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

// Small component to display information
const InfoItem = ({ label, value }: { label: string; value: string }) => (
  <div>
    <p className="text-xs sm:text-sm text-gray-700 font-medium">{label}:</p>
    <p className="font-semibold text-green-700 mt-1">{value}</p>
  </div>
);
