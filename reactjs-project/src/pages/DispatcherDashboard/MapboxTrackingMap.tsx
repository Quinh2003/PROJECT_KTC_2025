
import { useEffect, useRef, useState } from 'react';
import mapboxgl from 'mapbox-gl';
import { useDispatcherContext } from "../../contexts/DispatcherContext";
import 'mapbox-gl/dist/mapbox-gl.css';

export default function MapboxTrackingMap() {
  const { selectedOrder } = useDispatcherContext();
  const [start, setStart] = useState<[number, number] | null>(null);
  const [end, setEnd] = useState<[number, number] | null>(null);
  const [route, setRoute] = useState<any>(null);
  const [waypoints, setWaypoints] = useState<[number, number][]>([]);
  const [vehiclePos, setVehiclePos] = useState<[number, number] | null>(null);
  const realTruckMarker = useRef<mapboxgl.Marker | null>(null);
  const mapContainer = useRef<HTMLDivElement>(null);
  const map = useRef<mapboxgl.Map | null>(null);
  const markers = useRef<mapboxgl.Marker[]>([]);
  const [isLoaded, setIsLoaded] = useState(false);
  const waypointMarkers = useRef<mapboxgl.Marker[]>([]);

  // Đoạn đường đã đi qua: từ start đến vị trí xe hiện tại, lấy theo các waypoint
  // Nếu có vehiclePos và waypoint, lấy các waypoint từ start đến gần vehiclePos nhất
  const getTraveledPath = () => {
    if (!start || !vehiclePos || waypoints.length === 0) return [];
    // Tìm index gần nhất với vehiclePos
    let minIdx = 0;
    let minDist = Number.POSITIVE_INFINITY;
    waypoints.forEach((pt, idx) => {
      const d = Math.hypot(pt[0] - vehiclePos[0], pt[1] - vehiclePos[1]);
      if (d < minDist) {
        minDist = d;
        minIdx = idx;
      }
    });
    // Lấy các điểm từ đầu đến vị trí gần nhất
    return waypoints.slice(0, minIdx + 1);
  };
  const MAPBOX_TOKEN = import.meta.env.VITE_MAPBOX_ACCESS_TOKEN || "pk.eyJ1IjoieHVhbmh1eTEiLCJhIjoiY21lN3liN21tMDlzaTJtbXF3MjU0Z2JzaSJ9.vmH3qH_f7qf1ewBC_pJoSg";

  // Fetch vehicle position every 5s
  useEffect(() => {
    if (!selectedOrder?.vehicle?.id) return;
    const vehicleId = selectedOrder.vehicle.id;
    const fetchVehiclePos = async () => {
      try {
        const res = await fetch(`/api/tracking/vehicle/${vehicleId}/current`);
        if (res.ok) {
          const data = await res.json();
          if (data && typeof data.longitude === 'number' && typeof data.latitude === 'number') {
            setVehiclePos([data.longitude, data.latitude]);
            if (map.current) {
              map.current.flyTo({ center: [data.longitude, data.latitude], speed: 1.2, curve: 1.5 });
            }
          }
        }
      } catch (err) {
        console.error('Error fetching vehicle position:', err);
      }
    };
  fetchVehiclePos();
  const interval = setInterval(fetchVehiclePos, 3600000); // 1 tiếng
  return () => clearInterval(interval);
  }, [selectedOrder]);

  // Draw vehicle marker
  useEffect(() => {
    if (!map.current || !vehiclePos || !isLoaded) return;
    if (!realTruckMarker.current) {
      const truckEl = document.createElement('div');
      truckEl.style.width = '32px';
      truckEl.style.height = '32px';
      truckEl.style.display = 'flex';
      truckEl.style.alignItems = 'center';
      truckEl.style.justifyContent = 'center';
      truckEl.style.fontSize = '28px';
      truckEl.style.background = 'rgba(255,255,255,0.85)';
      truckEl.style.borderRadius = '50%';
      truckEl.style.boxShadow = '0 2px 8px rgba(0,0,0,0.15)';
      truckEl.innerHTML = '🚚';
      realTruckMarker.current = new mapboxgl.Marker(truckEl)
        .setLngLat(vehiclePos)
        .addTo(map.current);
    } else {
      realTruckMarker.current.setLngLat(vehiclePos);
    }
  }, [vehiclePos, isLoaded]);

  // Đã xóa code vẽ waypoint markers (các chấm xanh và xám)

  // Update route when selectedOrder changes
  useEffect(() => {
    if (selectedOrder && selectedOrder.store && selectedOrder.address) {
      const store = selectedOrder.store;
      const address = selectedOrder.address;
      if (store.latitude && store.longitude && address.latitude && address.longitude) {
        const startCoord: [number, number] = [store.longitude, store.latitude];
        const endCoord: [number, number] = [address.longitude, address.latitude];
        setStart(startCoord);
        setEnd(endCoord);
        // Fetch route from Mapbox Directions API
        const fetchRoute = async () => {
          const url = `https://api.mapbox.com/directions/v5/mapbox/driving/${startCoord.join(',')};${endCoord.join(',')}?geometries=geojson&access_token=${MAPBOX_TOKEN}`;
          try {
            const res = await fetch(url);
            if (res.ok) {
              const data = await res.json();
              if (data.routes && data.routes[0]) {
                setRoute(data.routes[0]);
                setWaypoints(data.routes[0].geometry.coordinates);
              }
            }
          } catch (err) {
            console.error('Error fetching route:', err);
          }
        };
        fetchRoute();
      }
    }
  }, [selectedOrder]);

  // Draw start/end markers, route line, và traveled path (màu xám)
  useEffect(() => {
    if (!map.current || !start || !end || !route) return;
    // Remove old markers
    markers.current.forEach(marker => marker.remove());
    markers.current = [];
    // Start marker
    const startEl = document.createElement('div');
    startEl.style.width = '16px';
    startEl.style.height = '16px';
    startEl.style.background = '#22c55e';
    startEl.style.border = '2px solid #fff';
    startEl.style.borderRadius = '50%';
    startEl.style.boxShadow = '0 1px 4px rgba(0,0,0,0.12)';
    const startMarker = new mapboxgl.Marker(startEl)
      .setLngLat(start)
      .addTo(map.current);
    markers.current.push(startMarker);
    // End marker
    const endEl = document.createElement('div');
    endEl.style.width = '16px';
    endEl.style.height = '16px';
    endEl.style.background = '#ef4444';
    endEl.style.border = '2px solid #fff';
    endEl.style.borderRadius = '50%';
    endEl.style.boxShadow = '0 1px 4px rgba(0,0,0,0.12)';
    const endMarker = new mapboxgl.Marker(endEl)
      .setLngLat(end)
      .addTo(map.current);
    markers.current.push(endMarker);
    // Draw route line
    const routeFeature: GeoJSON.Feature<GeoJSON.LineString> = {
      type: 'Feature',
      geometry: route.geometry,
      properties: {}
    };
    if (map.current.getSource('route')) {
      (map.current.getSource('route') as mapboxgl.GeoJSONSource).setData(routeFeature);
    } else {
      map.current.addSource('route', {
        type: 'geojson',
        data: routeFeature
      });
      map.current.addLayer({
        id: 'route',
        type: 'line',
        source: 'route',
        layout: { 'line-join': 'round', 'line-cap': 'round' },
        paint: { 'line-color': '#3b82f6', 'line-width': 5 }
      });
    }

    // Draw traveled path (gray)
    const traveledPath = getTraveledPath();
    const traveledFeature: GeoJSON.Feature<GeoJSON.LineString> = {
      type: 'Feature',
      geometry: {
        type: 'LineString',
        coordinates: traveledPath,
      },
      properties: {}
    };
    if (map.current.getSource('traveled-path')) {
      (map.current.getSource('traveled-path') as mapboxgl.GeoJSONSource).setData(traveledFeature);
    } else if (traveledPath.length > 1) {
      map.current.addSource('traveled-path', {
        type: 'geojson',
        data: traveledFeature
      });
      map.current.addLayer({
        id: 'traveled-path-layer',
        type: 'line',
        source: 'traveled-path',
        layout: { 'line-join': 'round', 'line-cap': 'round' },
        paint: { 'line-color': '#888', 'line-width': 5, 'line-opacity': 0.7 }
      });
    }
  }, [route, start, end, vehiclePos, waypoints]);

  // Initialize map
  useEffect(() => {
    if (map.current) return;
    if (!mapContainer.current) return;
    try {
      mapboxgl.accessToken = MAPBOX_TOKEN;
      map.current = new mapboxgl.Map({
        container: mapContainer.current,
        style: 'mapbox://styles/mapbox/satellite-streets-v12',
        center: [106.660172, 10.762622],
        zoom: 12
      });
      map.current.on('load', () => setIsLoaded(true));
      map.current.on('error', (error) => {
        console.error('MapboxTrackingMap: Map error:', error);
      });
    } catch (error) {
      console.error('MapboxTrackingMap: Initialization error:', error);
    }
    return () => {
      markers.current.forEach(marker => marker.remove());
      markers.current = [];
      waypointMarkers.current.forEach((m) => m.remove());
      waypointMarkers.current = [];
      if (map.current) {
        map.current.remove();
        map.current = null;
      }
      setIsLoaded(false);
    };
  }, []);

  // Handle manual route input (when no order selected)
  const handleGetRoute = async (e: React.FormEvent) => {
    e.preventDefault();
    if (!start || !end) return;
    const url = `https://api.mapbox.com/directions/v5/mapbox/driving/${start.join(',')};${end.join(',')}?geometries=geojson&access_token=${MAPBOX_TOKEN}`;
    try {
      const res = await fetch(url);
      if (res.ok) {
        const data = await res.json();
        if (data.routes && data.routes[0]) {
          setRoute(data.routes[0]);
          setWaypoints(data.routes[0].geometry.coordinates);
        }
      }
    } catch (err) {
      console.error('Error fetching route:', err);
    }
  };

  return (
    <div className="bg-white rounded-xl shadow-lg p-4 h-full min-h-[300px] w-full flex flex-col">
      {selectedOrder && (
        <div className="mb-4 p-3 bg-blue-50 rounded-lg border border-blue-200">
          <div className="text-sm font-semibold text-blue-900 mb-1">Đơn hàng #{selectedOrder.id}</div>
          <div className="text-xs text-gray-600">
            <div className="flex items-center gap-2 mb-1">
              <span className="w-3 h-3 bg-green-500 rounded-full inline-block"></span>
              <span><strong>Từ:</strong> {selectedOrder.store?.storeName} - {selectedOrder.store?.address}</span>
            </div>
            <div className="flex items-center gap-2">
              <span className="w-3 h-3 bg-red-500 rounded-full inline-block"></span>
              <span><strong>Đến:</strong> {selectedOrder.address?.address}</span>
            </div>
          </div>
          {route && (
            <div className="mt-2 text-xs text-blue-600">
              <strong>Khoảng cách:</strong> {(route.distance / 1000).toFixed(1)} km | 
              <strong> Thời gian:</strong> {Math.round(route.duration / 60)} phút
            </div>
          )}
        </div>
      )}
      {!selectedOrder && (
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
      )}
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
      </div>
 {/* Hiển thị danh sách waypoint nếu có */}
      {/* {waypoints.length > 0 && (
        <div className="mt-2 p-2 bg-gray-50 rounded">
          <div className="font-bold">Danh sách waypoint:</div>
          <ul className="text-xs">
            {waypoints.map((wp, idx) => (
              <li key={idx}>{wp.join(', ')}</li>
            ))}
          </ul>
        </div>
      )} */}
      </div>
  );
}