<<<<<<< HEAD
import React, { useEffect, useRef, useState } from 'react';
import { getRoute, postRoute } from "../../services/mapboxAPI";
=======

import { useEffect, useRef, useState } from 'react';
>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00
import mapboxgl from 'mapbox-gl';
import { useDispatcherContext } from "../../contexts/DispatcherContext";
import 'mapbox-gl/dist/mapbox-gl.css';
import { fetchOrders } from "../../services/OrderAPI";
import { searchLocation } from "../../services/mapboxAPI";

export default function MapboxTrackingMap() {
<<<<<<< HEAD
  // Warehouse type
  type Warehouse = {
    id: number;
    name: string;
    longitude: number;
    latitude: number;
    address: string;
  };
  // Danh sách warehouse mẫu
  const [warehouses] = useState<Warehouse[]>([
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
=======
  const { selectedOrder } = useDispatcherContext();
>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00
  const [start, setStart] = useState<[number, number] | null>(null);
  const [end, setEnd] = useState<[number, number] | null>(null);
  // State cho nhập mã đơn hàng
  const [orderCode, setOrderCode] = useState("");
  const [orderLoading, setOrderLoading] = useState(false);
  const [orderError, setOrderError] = useState("");
  // State hiển thị thông tin đơn hàng đã tìm
  const [orderInfo, setOrderInfo] = useState<any>(null);

    type Route = {
      geometry: {
        coordinates: [number, number][];
      };
      legs: Array<{
        steps: Array<{
          maneuver: {
            location: [number, number];
          };
        }>;
      }>;
      // ...other Mapbox route fields
    };
    const [route, setRoute] = useState<Route | null>(null);
  const [waypoints, setWaypoints] = useState<[number, number][]>([]);
<<<<<<< HEAD
  const [truckPos, setTruckPos] = useState<[number, number] | null>(null);
  // Thêm state cho kết quả route
  const [routeInfo, setRouteInfo] = useState<{distance?: number, duration?: number} | null>(null);

  const MAPBOX_TOKEN = import.meta.env.VITE_MAPBOX_ACCESS_TOKEN || "pk.eyJ1IjoieHVhbmh1eTEiLCJhIjoiY21lN3liN21tMDlzaTJtbXF3MjU0Z2JzaSJ9.vmH3qH_f7qf1ewBC_pJoSg";

  // Get tracking data
  const { tracking, error } = useSimpleTracking();

  // Đưa flyToVehicle xuống sau khi khai báo tracking
  const flyToVehicle = React.useCallback((vehicleId: number) => {
    if (!map.current || !tracking) return;
    const vehicle = tracking.find(v => v.id === vehicleId);
    if (!vehicle) return;
    map.current.flyTo({
      center: [vehicle.longitude, vehicle.latitude],
      zoom: 15,
      duration: 1000
    });
    setSelectedVehicle(vehicleId);
  }, [tracking]);
  // Hàm lấy lộ trình từ Mapbox Directions API
  async function handleGetRoute(e?: React.FormEvent) {
    if (e) e.preventDefault();
    let startPos: [number, number] | null = start;
    if ((!start || start[0] === undefined || start[1] === undefined) && tracking && tracking.length > 0) {
      startPos = [tracking[0].longitude, tracking[0].latitude];
      setStart(startPos);
    }
    if (!startPos || !end) return;
    const waypointsArr: [number, number][] = warehouses
      .filter(wh => selectedWarehouses.includes(wh.id))
      .map(wh => [wh.longitude, wh.latitude] as [number, number]);
    try {
      const data = await getRoute(startPos, waypointsArr, end, MAPBOX_TOKEN);
      if (data.routes && data.routes.length > 0) {
        setRoute(data.routes[0]);
        const wp = data.routes[0].legs.flatMap((leg: { steps: Array<{ maneuver: { location: [number, number] } }> }) => leg.steps.map((step: { maneuver: { location: [number, number] } }) => step.maneuver.location));
        setWaypoints(wp);
        // Luôn set quãng đường và thời gian dự kiến trước khi postRoute
        setRouteInfo({
          distance: data.routes[0].distance, // mét
          duration: data.routes[0].duration // giây
        });
        try {
          await postRoute(data.routes[0], wp);
        } catch (err) {
          console.error("Lỗi gửi lộ trình về backend:", err);
        }
      }
    } catch (err) {
      console.error("Lỗi lấy lộ trình:", err);
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
      data: {
        type: "LineString",
        coordinates: route.geometry.coordinates
      }
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
=======
  const [vehiclePos, setVehiclePos] = useState<[number, number] | null>(null);
  const realTruckMarker = useRef<mapboxgl.Marker | null>(null);
>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00
  const mapContainer = useRef<HTMLDivElement>(null);
  const map = useRef<mapboxgl.Map | null>(null);
  const markers = useRef<mapboxgl.Marker[]>([]);
  const [isLoaded, setIsLoaded] = useState(false);
<<<<<<< HEAD
  const [selectedVehicle, setSelectedVehicle] = useState<number | null>(null);
  // Khi chọn xe, cập nhật điểm đầu là vị trí xe đó
    const handleVehicleClick = (point: { id: number; longitude: number; latitude: number }) => {
    flyToVehicle(point.id);
    setStart([point.longitude, point.latitude]);
    setSelectedVehicle(point.id);
  };

=======
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

>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00
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
  }, [MAPBOX_TOKEN]);

<<<<<<< HEAD
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
  }, [isLoaded, tracking, selectedVehicle, warehouses, end, flyToVehicle]);


  if (error) {
    return (
      <div className="bg-white rounded-xl shadow p-4 h-full min-h-[300px] flex items-center justify-center">
        <div className="text-red-500">{error}</div>
      </div>
    );
  }

  async function geocodeAddress(address: string): Promise<[number, number] | null> {
    try {
      const results = await searchLocation(address);
      if (results && results.length > 0) {
        // Nominatim trả về lat/lon dạng chuỗi
        return [parseFloat(results[0].lon), parseFloat(results[0].lat)];
      }
    } catch {
      // ignore
    }
    return null;
  }

  return (
    <div className="bg-white rounded-xl shadow-lg p-4 w-full flex flex-col gap-4" style={{ minHeight: '500px' }}>
      {/* Nhập mã đơn hàng để tự động điền tọa độ */}
      <div className="mb-2">
        <div className="font-bold text-blue-600 mb-1">Nhập mã đơn hàng để tự động điền điểm từ/đến:</div>
        <form
          className="flex gap-2 mb-2"
          onSubmit={async e => {
            e.preventDefault();
            setOrderLoading(true);
            setOrderError("");
            setOrderInfo(null);
            try {
              const token = localStorage.getItem("token") || "";
              const res = await fetchOrders(1, 10, token);
              console.log("fetchOrders result:", res);
              const found = res.data.find(o => o.id.toString() === orderCode);
              console.log("Order found:", found);
              if (!found) {
                setOrderError("Không tìm thấy đơn hàng với mã này.");
                setOrderLoading(false);
                return;
              }
              setOrderInfo(found);
              // Lấy địa chỉ điểm đầu/điểm cuối
              const startAddress = found.store?.address || "";
              const endAddress = found.address?.address || "";
              console.log("Start address:", startAddress);
              console.log("End address:", endAddress);
              if (!startAddress || !endAddress) {
                setOrderError("Đơn hàng chưa có đủ địa chỉ điểm từ/đến.");
                setOrderLoading(false);
                return;
              }
              // Geocode địa chỉ
              const startCoords = await geocodeAddress(startAddress);
              const endCoords = await geocodeAddress(endAddress);
              console.log("Start coords:", startCoords);
              console.log("End coords:", endCoords);
              if (!startCoords || !endCoords) {
                setOrderError("Không chuyển được địa chỉ thành tọa độ. Vui lòng kiểm tra lại địa chỉ.");
                setOrderLoading(false);
                return;
              }
              setStart(startCoords);
              setEnd(endCoords);
              setRoute(null);
              setWaypoints([]);
              setOrderLoading(false);
            } catch (err) {
              console.error("Lỗi khi lấy thông tin đơn hàng hoặc chuyển địa chỉ:", err);
              setOrderError("Lỗi khi lấy thông tin đơn hàng hoặc chuyển địa chỉ.");
              setOrderLoading(false);
            }
          }}
        >
          <input
            type="text"
            className="border px-3 py-1 rounded w-40"
            placeholder="Nhập mã đơn hàng"
            value={orderCode}
            onChange={e => setOrderCode(e.target.value)}
          />
          <button type="submit" className="bg-blue-500 text-white px-3 py-1 rounded">Tìm đơn hàng</button>
          {orderLoading && <span className="ml-2 text-gray-500">Đang tải...</span>}
          {orderError && <span className="ml-2 text-red-500">{orderError}</span>}
        </form>
        {/* Hiển thị thông tin đơn hàng dưới ô nhập */}
        {orderInfo && (
          <div className="mt-2 p-3 bg-blue-50 border border-blue-200 rounded-xl shadow">
            <div className="font-bold text-blue-700 mb-1">Thông tin đơn hàng:</div>
            <div className="text-sm text-gray-700">
              <div><span className="font-semibold">Từ:</span> {orderInfo.store?.address}</div>
              <div><span className="font-semibold">Đến:</span> {orderInfo.address?.address}</div>
              <div><span className="font-semibold">Quãng đường:</span> {routeInfo && routeInfo.distance !== undefined ? `${(routeInfo.distance/1000).toFixed(2)} km` : "-"}</div>
              <div><span className="font-semibold">Thời gian dự kiến:</span> {routeInfo && routeInfo.duration !== undefined ? `${Math.round(routeInfo.duration/60)} phút` : "-"}</div>
            </div>
          </div>
        )}
      </div>
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
      {/* Nút lấy lộ trình */}
      <button onClick={handleGetRoute} className="bg-blue-500 text-white px-4 py-2 rounded mb-4">Lấy lộ trình</button>

      <div 
  ref={mapContainer}
  className="w-full rounded-lg border-2 border-blue-500 relative overflow-hidden"
  style={{ minHeight: '400px', width: '100%', position: 'relative', border: '2px solid blue' }}
=======
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
>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00
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
<<<<<<< HEAD

      <VehicleList 
        tracking={tracking || []}
        selectedVehicle={selectedVehicle}
        onVehicleClick={handleVehicleClick}
      />
  {/* Đã ẩn phần hiển thị chi tiết lộ trình và waypoint */}
    </div>
=======
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
>>>>>>> dd820b7dec040ef3e189b718e7431eec3e2d3d00
  );
}