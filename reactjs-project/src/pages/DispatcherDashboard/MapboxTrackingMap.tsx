import { useEffect, useRef, useState } from 'react';
import mapboxgl from 'mapbox-gl';
import VehicleList from "../../components/VehicleList";
import useSimpleTracking from "../../hooks/useSimpleTracking";
import 'mapbox-gl/dist/mapbox-gl.css';

export default function MapboxTrackingMap() {
  const mapContainer = useRef<HTMLDivElement>(null);
  const map = useRef<mapboxgl.Map | null>(null);
  const markers = useRef<mapboxgl.Marker[]>([]);
  const [isLoaded, setIsLoaded] = useState(false);
  const [selectedVehicle, setSelectedVehicle] = useState<number | null>(null);

  const MAPBOX_TOKEN = import.meta.env.VITE_MAPBOX_ACCESS_TOKEN || "pk.eyJ1IjoieHVhbmh1eTEiLCJhIjoiY21lN3liN21tMDlzaTJtbXF3MjU0Z2JzaSJ9.vmH3qH_f7qf1ewBC_pJoSg";

  // Get tracking data
  const { tracking, error } = useSimpleTracking();

  // Initialize map
  useEffect(() => {
    if (map.current) return; // Map already initialized

    if (!mapContainer.current) {
      console.error('MapboxTrackingMap: Map container not found');
      return;
    }

    try {
      mapboxgl.accessToken = MAPBOX_TOKEN;

      map.current = new mapboxgl.Map({
        container: mapContainer.current,
        style: 'mapbox://styles/mapbox/satellite-streets-v12',
        center: [106.660172, 10.762622], // Ho Chi Minh City
        zoom: 12
      });

      map.current.on('load', () => {
        setIsLoaded(true);
      });

      map.current.on('error', (error) => {
        console.error('MapboxTrackingMap: Map error:', error);
      });

    } catch (error) {
      console.error('MapboxTrackingMap: Initialization error:', error);
    }

    return () => {
      // Cleanup markers
      markers.current.forEach(marker => marker.remove());
      markers.current = [];
      
      // Cleanup map
      if (map.current) {
        map.current.remove();
        map.current = null;
      }
      setIsLoaded(false);
    };
  }, []);

  // Add markers when map is loaded and tracking data is available
  useEffect(() => {
    if (!map.current || !isLoaded || !tracking || tracking.length === 0) {
      return;
    }

    // Clear existing markers
    markers.current.forEach(marker => marker.remove());
    markers.current = [];

    // Add new markers
    tracking.forEach((vehicle) => {
      try {
        // Create custom truck icon element
        const truckIcon = document.createElement('div');
        
        // Base Tailwind classes  
        const baseClasses = 'w-8 h-8 flex items-center justify-center rounded-full bg-white shadow-lg cursor-pointer text-base transition-all duration-300 hover:shadow-xl';
        
        // Selected or normal state with thick border using outline
        const outlineClass = selectedVehicle === vehicle.id 
          ? 'outline outline-4 outline-red-500' 
          : 'outline outline-2 outline-blue-500';
        
        truckIcon.className = `${baseClasses} ${outlineClass}`;
        truckIcon.innerHTML = '🚚';

        const marker = new mapboxgl.Marker({ 
          element: truckIcon
        })
          .setLngLat([vehicle.longitude, vehicle.latitude])
          .addTo(map.current!);

        // Add tooltip with vehicle info
        truckIcon.title = `Vehicle ${vehicle.vehicleId} - ${vehicle.status}`;

        // Add click handler for marker
        truckIcon.addEventListener('click', () => {
          setSelectedVehicle(vehicle.id);
          flyToVehicle(vehicle.id);
        });

        markers.current.push(marker);
      } catch (error) {
        console.error('MapboxTrackingMap: Error adding marker for vehicle', vehicle.id, error);
      }
    });

  }, [map.current, isLoaded, tracking, selectedVehicle]);

  // Fly to vehicle function
  const flyToVehicle = (vehicleId: number) => {
    if (!map.current || !tracking) return;
    
    const vehicle = tracking.find(v => v.id === vehicleId);
    if (!vehicle) return;

    map.current.flyTo({
      center: [vehicle.longitude, vehicle.latitude],
      zoom: 15,
      duration: 1000
    });
    
    setSelectedVehicle(vehicleId);
  };

  if (error) {
    return (
      <div className="bg-white rounded-xl shadow p-4 h-full min-h-[300px] flex items-center justify-center">
        <div className="text-red-500">{error}</div>
      </div>
    );
  }

  return (
    <div className="bg-white rounded-xl shadow-lg p-4 h-full min-h-[300px] w-full flex flex-col">
      <div 
        ref={mapContainer} 
        className="flex-1 min-h-[250px] h-[350px] w-full rounded-lg border-2 border-blue-500 relative overflow-hidden"
        style={{
          minHeight: '250px',
          height: '350px',
          width: '100%',
          position: 'relative',
          border: '2px solid blue' // Debug border
        }}
      >
        {!isLoaded && (
          <div className="absolute inset-0 bg-gray-100 flex items-center justify-center z-10 pointer-events-none">
            <div className="text-gray-600">
              <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto mb-2"></div>
              Loading map...
            </div>
          </div>
        )}
      </div>
      
      <VehicleList 
        tracking={tracking || []}
        selectedVehicle={selectedVehicle}
        onVehicleClick={(point) => flyToVehicle(point.id)}
      />
    </div>
  );
}
