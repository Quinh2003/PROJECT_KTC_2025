import { GoogleMap } from '@react-google-maps/api';

const containerStyle = {
  width: '100%',
  height: '350px',
  borderRadius: '0.75rem',
};

const center = {
  lat: 10.762622, // Ho Chi Minh City
  lng: 106.660172,
};

export default function RouteMap({ isLoaded, loadError }: { isLoaded: boolean, loadError: any }) {
  if (loadError) {
    return <div className="text-red-500">Error loading Google Maps</div>;
  }
  return (
    <div className="bg-white rounded-xl shadow p-4 h-full min-h-[300px] flex items-center justify-center">
      {isLoaded ? (
        <GoogleMap
          mapContainerStyle={containerStyle}
          center={center}
          zoom={12}
        >
          {/* You can add Marker, Polyline, etc. here */}
        </GoogleMap>
      ) : (
        <span className="text-gray-400">Loading map...</span>
      )}
    </div>
  );
}