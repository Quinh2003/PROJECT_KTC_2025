import React, { useEffect, useRef, useState } from 'react';
import mapboxgl from 'mapbox-gl';
import VehicleList from "../../components/VehicleList";
import useSimpleTracking from "../../hooks/useSimpleTracking";
import 'mapbox-gl/dist/mapbox-gl.css';


// Component tìm kiếm địa điểm hoặc tọa độ, trả về [lng, lat]
interface SearchBoxProps {
  placeholder: string;
  onSelect: (coords: [number, number]) => void;
}

const SearchBox: React.FC<SearchBoxProps> = ({ placeholder, onSelect }) => {
  const [query, setQuery] = useState('');
  const [suggestions, setSuggestions] = useState<{ name: string; coords: [number, number] }[]>([]);
  const [loading, setLoading] = useState(false);

  // Hàm tìm kiếm địa điểm qua Nominatim (OpenStreetMap)
  const handleSearch = async (value: string) => {
    setQuery(value);
    if (!value || value.match(/^\s*$/)) {
      setSuggestions([]);
      return;
    }
    // Nếu nhập đúng định dạng tọa độ thì trả về luôn
    const coordMatch = value.match(/^\s*(-?\d{1,3}\.\d+),\s*(-?\d{1,2}\.\d+)\s*$/);
    if (coordMatch) {
      const lng = parseFloat(coordMatch[1]);
      const lat = parseFloat(coordMatch[2]);
      setSuggestions([{ name: `Tọa độ: ${lng},${lat}`, coords: [lng, lat] }]);
      return;
    }
    setLoading(true);
    try {
      const res = await fetch(`https://nominatim.openstreetmap.org/search?q=${encodeURIComponent(value)}&format=json&addressdetails=1&limit=5`);
      const data = await res.json();
      if (Array.isArray(data) && data.length > 0) {
        setSuggestions(data.map((item: any) => ({
          name: item.display_name,
          coords: [parseFloat(item.lon), parseFloat(item.lat)]
        })));
      } else {
        setSuggestions([]);
      }
    } catch {
      setSuggestions([]);
    }
    setLoading(false);
  };

  return (
    <div className="relative w-64">
      <input
        type="text"
        value={query}
        placeholder={placeholder}
        onChange={e => handleSearch(e.target.value)}
        className="border p-2 rounded w-full"
      />
      {loading && <div className="absolute left-0 top-full bg-white p-2 text-xs">Đang tìm kiếm...</div>}
      {suggestions.length > 0 ? (
        <ul className="absolute left-0 top-full bg-white border rounded shadow w-full z-10">
          {suggestions.map((s, idx) => (
            <li
              key={idx}
              className="p-2 cursor-pointer hover:bg-blue-100 text-xs"
              onClick={() => {
                setQuery(s.name);
                setSuggestions([]);
                onSelect(s.coords);
              }}
            >{s.name}</li>
          ))}
        </ul>
      ) : (!loading && query.trim() !== '' && (
        <div className="absolute left-0 top-full bg-white p-2 text-xs text-red-500 border rounded shadow w-full z-10">Không tìm thấy địa điểm phù hợp. Vui lòng thử lại hoặc nhập địa chỉ khác.</div>
      ))}
    </div>
  );
};
export default function MapboxTrackingMap() {
  // Danh sách warehouse mẫu
  const [warehouses] = useState([
    {
      id: 1,
      name: "Warehouse TP.HCM",
      longitude: 106.7,
      latitude: 10.8,
      address: "86 Lê Thánh Tôn, Quận 1, TP.HCM"
    },
    {
      id: 2,
      name: "Warehouse Hà Nội",
      longitude: 105.854444,
      latitude: 21.028511,
      address: "1 Tràng Tiền, Hoàn Kiếm, Hà Nội"
    },
    {
      id: 3,
      name: "Warehouse Đà Nẵng",
      longitude: 108.220556,
      latitude: 16.047079,
      address: "42 Bạch Đằng, Hải Châu, Đà Nẵng"
    },
    {
      id: 4,
      name: "Warehouse Cần Thơ",
      longitude: 105.783333,
      latitude: 10.033333,
      address: "2 Hòa Bình, Ninh Kiều, Cần Thơ"
    },
    {
      id: 5,
      name: "Warehouse Hải Phòng",
      longitude: 106.682,
      latitude: 20.864,
      address: "10 Lê Hồng Phong, Hải Phòng"
    },
    {
      id: 6,
      name: "Warehouse Nha Trang",
      longitude: 109.196747,
      latitude: 12.238791,
      address: "50 Trần Phú, Nha Trang, Khánh Hòa"
    },
    {
      id: 7,
      name: "Warehouse Buôn Ma Thuột",
      longitude: 108.083333,
      latitude: 12.666667,
      address: "1 Nguyễn Tất Thành, Buôn Ma Thuột, Đắk Lắk"
    },
    {
      id: 8,
      name: "Warehouse Vinh",
      longitude: 105.341,
      latitude: 18.666,
      address: "5 Lê Mao, Vinh, Nghệ An"
    }
  ]);
  // Chọn các warehouse để lấy lộ trình
  const [selectedWarehouses, setSelectedWarehouses] = useState<number[]>([]);
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
    // Lấy danh sách warehouse đã chọn làm waypoint
    const waypointsStr = warehouses
      .filter(wh => selectedWarehouses.includes(wh.id))
      .map(wh => `${wh.longitude},${wh.latitude}`)
      .join(';');
    // Tạo URL Mapbox Directions API với các điểm dừng
    // Format: start;wp1;wp2;...;end
    const url = `https://api.mapbox.com/directions/v5/mapbox/driving/${startPos[0]},${startPos[1]};${waypointsStr}${waypointsStr ? ';' : ''}${end[0]},${end[1]}?geometries=geojson&steps=true&access_token=${MAPBOX_TOKEN}`;
    const res = await fetch(url);
    const data = await res.json();
    if (data.routes && data.routes.length > 0) {
      setRoute(data.routes[0]);
      const wp = data.routes[0].legs.flatMap((leg: any) => leg.steps.map((step: any) => step.maneuver.location));
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
    if (!map.current || !isLoaded) return;
    // Xóa marker cũ
    markers.current.forEach(marker => marker.remove());
    markers.current = [];
    // Hiển thị marker warehouse
    warehouses.forEach(wh => {
      const warehouseMarker = new mapboxgl.Marker({ color: 'orange' })
        .setLngLat([wh.longitude, wh.latitude])
        .setPopup(new mapboxgl.Popup().setText(`${wh.name}\n${wh.address}`))
        .addTo(map.current!);
      markers.current.push(warehouseMarker);
    });
    // Hiển thị marker điểm cuối nếu có
    if (end) {
      const endIcon = document.createElement('div');
      endIcon.className = 'w-8 h-8 flex items-center justify-center rounded-full bg-green-500 text-white text-xl shadow-lg';
      endIcon.innerHTML = '🏁'; // icon cờ kết thúc
      const endMarker = new mapboxgl.Marker({ element: endIcon })
        .setLngLat(end)
        .setPopup(new mapboxgl.Popup().setText('Điểm cuối'))
        .addTo(map.current!);
      markers.current.push(endMarker);
    }
    // Hiển thị marker xe
    if (tracking && tracking.length > 0) {
      tracking.forEach((vehicle) => {
        try {
          const truckIcon = document.createElement('div');
          const baseClasses = 'w-8 h-8 flex items-center justify-center rounded-full bg-white shadow-lg cursor-pointer text-base transition-all duration-300 hover:shadow-xl';
          const outlineClass = selectedVehicle === vehicle.id 
            ? 'outline outline-4 outline-red-500' 
            : 'outline outline-2 outline-blue-500';
          truckIcon.className = `${baseClasses} ${outlineClass}`;
          truckIcon.innerHTML = '🚚';
          const marker = new mapboxgl.Marker({ element: truckIcon })
            .setLngLat([vehicle.longitude, vehicle.latitude])
            .addTo(map.current!);
          truckIcon.title = `Vehicle ${vehicle.vehicleId} - ${vehicle.status}`;
          truckIcon.addEventListener('click', () => {
            setSelectedVehicle(vehicle.id);
            flyToVehicle(vehicle.id);
          });
          markers.current.push(marker);
        } catch (error) {
          console.error('MapboxTrackingMap: Error adding marker for vehicle', vehicle.id, error);
        }
      });
    }
  }, [isLoaded, tracking, selectedVehicle, warehouses]);

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
    <div className="bg-white rounded-xl shadow-lg p-4 h-full min-h-[300px] w-full flex flex-col gap-4">
      {/* Chọn warehouse để lấy lộ trình */}
      <div className="mb-2">
        <div className="font-bold text-blue-600 mb-1">Chọn các warehouse để lấy lộ trình:</div>
        <div className="flex gap-2 flex-wrap">
          {warehouses.map(wh => (
            <label key={wh.id} className={`px-3 py-1 rounded-full border cursor-pointer ${selectedWarehouses.includes(wh.id) ? 'bg-orange-200 border-orange-400' : 'bg-gray-100 border-gray-300'}`}>
              <input
                type="checkbox"
                checked={selectedWarehouses.includes(wh.id)}
                onChange={e => {
                  if (e.target.checked) {
                    setSelectedWarehouses([...selectedWarehouses, wh.id]);
                  } else {
                    setSelectedWarehouses(selectedWarehouses.filter(id => id !== wh.id));
                  }
                }}
                className="mr-2"
              />
              {wh.name}
            </label>
          ))}
        </div>
      </div>
      {/* Form nhập điểm đầu/điểm cuối */}
      <form onSubmit={handleGetRoute} className="mb-4 flex gap-2">
        <SearchBox
          placeholder="Nhập tên địa điểm hoặc tọa độ điểm đầu (ví dụ: Hồ Chí Minh hoặc 106.7,10.8)"
          onSelect={coords => {
            setStart(coords);
            setRoute(null);
            setWaypoints([]);
          }}
        />
        <SearchBox
          placeholder="Nhập tên địa điểm hoặc tọa độ điểm cuối (ví dụ: Hà Nội hoặc 106.8,10.9)"
          onSelect={coords => {
            setEnd(coords);
            setRoute(null);
            setWaypoints([]);
          }}
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
          <div className="font-bold mb-1">Chi tiết lộ trình:</div>
          {route && route.legs && route.legs.length > 0 && (
            <ul className="text-xs mb-2">
              {route.legs.map((leg: any, idx: number) => (
                <li key={idx} className="mb-1">
                  <span className="font-semibold">Đoạn {idx + 1}:</span> 
                  {leg.summary ? <span>{leg.summary} - </span> : null}
                  <span>Quãng đường: {(leg.distance/1000).toFixed(2)} km</span>,
                  <span> Thời gian dự tính: {Math.round(leg.duration/60)} phút</span>
                </li>
              ))}
            </ul>
          )}
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