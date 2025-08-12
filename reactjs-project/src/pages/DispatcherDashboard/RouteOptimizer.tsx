import React, { useState } from "react";
import { GoogleMap, Polyline, Marker } from "@react-google-maps/api";

// Hàm gọi API tối ưu hóa lộ trình
async function optimizeRoute(data: any) {
  const res = await fetch("http://localhost:8080/api/route/optimize", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(data),
  });
  if (!res.ok) throw new Error("Tối ưu hóa lộ trình thất bại");
  return res.json();
}

const containerStyle = {
  width: "100%",
  height: "400px",
};

export default function RouteOptimizer({ isLoaded, loadError }: { isLoaded: boolean, loadError: any }) {
  const [result, setResult] = useState<any>(null);
  const [loading, setLoading] = useState(false);

  // Dữ liệu mẫu, bạn có thể thay bằng dữ liệu thực tế
  const data = {
    origin: { lat: 10.762622, lng: 106.660172, address: "A" },
    destination: { lat: 10.762622, lng: 106.660172, address: "A" },
    locations: [
      { lat: 10.776889, lng: 106.700806, address: "B" },
      { lat: 10.762913, lng: 106.682171, address: "C" },
    ],
  };



  const handleOptimize = async () => {
    setLoading(true);
    try {
      const res = await optimizeRoute(data);
      setResult(res);
    } catch (err) {
      alert("Có lỗi khi tối ưu lộ trình");
    }
    setLoading(false);
  };

  // Lấy danh sách điểm để vẽ Polyline
  const path = result?.steps?.map((step: any) => ({
    lat: step.lat,
    lng: step.lng,
  })) || [];

  if (loadError) {
    return <div className="text-red-500">Error loading Google Maps</div>;
  }
  return (
    <div className="p-6 max-w-3xl mx-auto">
      <button
        onClick={handleOptimize}
        className="px-4 py-2 bg-blue-600 text-white rounded shadow hover:bg-blue-700 transition"
        disabled={loading}
      >
        {loading ? "Đang tối ưu..." : "Tối ưu hóa lộ trình"}
      </button>

      {/* Hiển thị bản đồ */}
      {isLoaded && result?.steps?.length > 0 && (
        <div className="my-6 rounded-xl overflow-hidden shadow-lg">
          <GoogleMap
            mapContainerStyle={containerStyle}
            center={path[0]}
            zoom={13}
          >
            <Polyline
              path={path}
              options={{
                strokeColor: "#1976d2",
                strokeOpacity: 0.8,
                strokeWeight: 5,
              }}
            />
            {path.map((point: any, idx: number) => (
              <Marker
                key={idx}
                position={point}
                label={`${idx + 1}`}
              />
            ))}
          </GoogleMap>
        </div>
      )}

      {/* Hiển thị danh sách lộ trình */}
      {result && (
        <div className="mt-4 bg-white/70 backdrop-blur rounded-xl p-4 shadow">
          <div className="font-semibold mb-2">
            Tổng quãng đường: <span className="text-blue-700">{Math.round(result.totalDistance / 1000)} km</span>
            &nbsp;|&nbsp;
            Tổng thời gian: <span className="text-blue-700">{Math.round(result.totalDuration / 60)} phút</span>
          </div>
          <ol className="list-decimal ml-6">
            {result.steps.map((step: any, idx: number) => (
              <li key={idx}>
                {step.address} ({step.lat}, {step.lng})
              </li>
            ))}
          </ol>
        </div>
      )}
    </div>
  );
}