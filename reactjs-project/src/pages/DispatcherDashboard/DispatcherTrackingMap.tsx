import { useEffect, useState } from "react";
import { getAuthHeaders } from "../../services/operationsAPI";
import { GoogleMap, Marker } from "@react-google-maps/api";

const containerStyle = {
  width: "100%",
  height: "350px",
  borderRadius: "0.75rem",
};

const center = {
  lat: 10.762622, // Ho Chi Minh City
  lng: 106.660172,
};

export interface TrackingPoint {
  id: number;
  latitude: number;
  longitude: number;
  vehicleId: number;
  status: string;
  timestamp: string;
}

export default function DispatcherTrackingMap({ isLoaded, loadError }: { isLoaded: boolean; loadError: any }) {
  const [tracking, setTracking] = useState<TrackingPoint[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (!isLoaded) return;
    setLoading(true);
    fetch("http://localhost:8080/api/tracking/active-vehicles", {
      headers: getAuthHeaders(),
    })
      .then((res) => {
        if (!res.ok) throw new Error("Failed to fetch tracking data");
        return res.json();
      })
      .then((data) => {
        // Map API response to TrackingPoint[]
        setTracking(
          data.map((item: any) => ({
            id: item.vehicleId,
            latitude: item.latitude,
            longitude: item.longitude,
            vehicleId: item.vehicleId,
            status: item.status || "",
            timestamp: item.updatedAt || "",
          }))
        );
        setLoading(false);
      })
      .catch((err) => {
        setError(err.message);
        setLoading(false);
      });
  }, [isLoaded]);

  if (loadError) {
    return <div className="text-red-500">Error loading Google Maps</div>;
  }
  if (loading) {
    return <span className="text-gray-400">Loading tracking data...</span>;
  }
  if (error) {
    return <div className="text-red-500">{error}</div>;
  }

  return (
    <div className="bg-white rounded-xl shadow p-4 h-full min-h-[300px] flex items-center justify-center">
      {isLoaded ? (
        <GoogleMap mapContainerStyle={containerStyle} center={center} zoom={12}>
          {tracking.map((point) => (
            <Marker
              key={point.id}
              position={{ lat: point.latitude, lng: point.longitude }}
              label={point.vehicleId.toString()}
            />
          ))}
        </GoogleMap>
      ) : (
        <span className="text-gray-400">Loading map...</span>
      )}
    </div>
  );
<<<<<<< HEAD
}
=======
}
>>>>>>> 042a7c16d89d185c6e74a32de79f098e8a6971b5
