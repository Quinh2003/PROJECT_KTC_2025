import { useEffect, useRef, useState } from 'react';
import mapboxgl from 'mapbox-gl';
import VehicleList from "../../components/VehicleList";
import useSimpleTracking from "../../hooks/useSimpleTracking";
import 'mapbox-gl/dist/mapbox-gl.css';

export default function MapboxTrackingMap() {
  // State cho lộ trình
  const [start, setStart] = useState<[number, number] | null>(null);
  const [end, setEnd] = useState<[number, number] | null>(null);
  const [route, setRoute] = useState<any>(null);
  const [waypoints, setWaypoints] = useState<[number, number][]>([]);
  const [truckPos, setTruckPos] = useState<[number, number] | null>(null);
  // Hàm lấy lộ trình từ Mapbox Directions API
  async function handleGetRoute(e?: React.FormEvent) {
    if (e) e.preventDefault();
    // Nếu có tracking, lấy vị trí xe đầu tiên làm điểm đầu
    let startPos: [number, number] | null = start;
    if ((!start || start[0] === undefined || start[1] === undefined) && tracking && tracking.length > 0) {
      startPos = [tracking[0].longitude, tracking[0].latitude];
      setStart(startPos);
    }
    if (!startPos || !end) return;
    const url = `https://api.mapbox.com/directions/v5/mapbox/driving/${startPos[0]},${startPos[1]};${end[0]},${end[1]}?geometries=geojson&steps=true&access_token=${MAPBOX_TOKEN}`;
    const res = await fetch(url);
    const data = await res.json();
    if (data.routes && data.routes.length > 0) {
      setRoute(data.routes[0]);
      const wp = data.routes[0].legs[0].steps.map((step: any) => step.maneuver.location);
      setWaypoints(wp);
      // Gửi về BE
      fetch('/api/routes', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          route: data.routes[0],
          waypoints: wp
        })
      });
    }
  }
  // Vẽ polyline lộ trình trên map
  useEffect(() => {
    if (!map.current || !route) return;
    // Xóa layer cũ nếu có
    if (map.current.getSource('route')) {
      if (map.current.getLayer('route')) map.current.removeLayer('route');
      map.current.removeSource('route');
    }
    map.current.addSource('route', {
      type: 'geojson',
      data: route.geometry
    });
    map.current.addLayer({
      id: 'route',
      type: 'line',
      source: 'route',
      layout: { 'line-join': 'round', 'line-cap': 'round' },
      paint: { 'line-color': '#3b82f6', 'line-width': 5 }
    });
    // Fit bounds
    const coords = route.geometry.coordinates;
    if (coords.length > 1) {
      map.current.fitBounds([
        coords[0],
        coords[coords.length - 1]
      ], { padding: 50 });
    }
  }, [route]);
  // Mô phỏng xe tải di chuyển qua từng waypoint
  useEffect(() => {
    if (!waypoints || waypoints.length === 0) return;
    let idx = 0;
    setTruckPos(waypoints[0]);
    const interval = setInterval(() => {
      idx++;
      if (idx < waypoints.length) {
        setTruckPos(waypoints[idx]);
        // Gửi vị trí xe về BE nếu cần
        // fetch('/api/truck-position', { ... })
      } else {
        clearInterval(interval);
      }
    }, 2000); // 2s qua mỗi waypoint
    return () => clearInterval(interval);
  }, [waypoints]);
  const mapContainer = useRef<HTMLDivElement>(null);
  const map = useRef<mapboxgl.Map | null>(null);
  const markers = useRef<mapboxgl.Marker[]>([]);
  const [isLoaded, setIsLoaded] = useState(false);
  const [selectedVehicle, setSelectedVehicle] = useState<number | null>(null);
  // Khi chọn xe, cập nhật điểm đầu là vị trí xe đó
  const handleVehicleClick = (point: any) => {
    flyToVehicle(point.id);
    setStart([point.longitude, point.latitude]);
    setSelectedVehicle(point.id);
  };

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
      {/* Form nhập điểm đầu/điểm cuối */}
      <form onSubmit={handleGetRoute} className="mb-4 flex gap-2">
        <input
          type="text"
          placeholder="Tọa độ điểm đầu (vd: 106.7,10.8)"
          onChange={e => setStart(e.target.value.split(',').map(Number) as [number, number])}
          className="border p-2 rounded"
        />
        <input
          type="text"
          placeholder="Tọa độ điểm cuối (vd: 106.8,10.9)"
          onChange={e => setEnd(e.target.value.split(',').map(Number) as [number, number])}
          className="border p-2 rounded"
        />
        <button type="submit" className="bg-blue-500 text-white px-4 py-2 rounded">Lấy lộ trình</button>
      </form>

      <div 
        ref={mapContainer} 
        className="flex-1 min-h-[250px] h-[350px] w-full rounded-lg border-2 border-blue-500 relative overflow-hidden"
        style={{
          minHeight: '250px',
          height: '350px',
          width: '100%',
          position: 'relative',
          border: '2px solid blue'
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
        {/* Hiển thị marker xe tải tại vị trí truckPos nếu có lộ trình */}
        {truckPos && (
          <div style={{position: 'absolute', left: 0, top: 0, zIndex: 20}}>
            {/* Có thể dùng marker Mapbox hoặc custom icon */}
          </div>
        )}
      </div>

      <VehicleList 
        tracking={tracking || []}
        selectedVehicle={selectedVehicle}
        onVehicleClick={handleVehicleClick}
      />
      {/* Hiển thị danh sách waypoint nếu có */}
      {waypoints.length > 0 && (
        <div className="mt-2 p-2 bg-gray-50 rounded">
          <div className="font-bold">Danh sách waypoint:</div>
          <ul className="text-xs">
            {waypoints.map((wp, idx) => (
              <li key={idx}>{wp.join(', ')}</li>
            ))}
          </ul>
        </div>
      )}
    </div>
  );
}
