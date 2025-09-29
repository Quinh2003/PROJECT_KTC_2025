import { useEffect, useRef, useState } from "react";
import { useTranslation } from 'react-i18next';
import mapboxgl from "mapbox-gl";
import { useDispatcherContext } from "../../contexts/DispatcherContext";
import "mapbox-gl/dist/mapbox-gl.css";
import { logTrackingConsistency, getDisplayOrderId, getDisplayDeliveryId } from "../../utils/debugTracking";

export default function MapboxTrackingMap() {
  const { t } = useTranslation();
  const { selectedOrder } = useDispatcherContext();
  const [start, setStart] = useState<[number, number] | null>(null);
  const [end, setEnd] = useState<[number, number] | null>(null);

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
    distance: number;
    duration: number;
    // ...other Mapbox route fields
  };
  const [route, setRoute] = useState<Route | null>(null);
  const [waypoints, setWaypoints] = useState<[number, number][]>([]);
  const [vehiclePos, setVehiclePos] = useState<[number, number] | null>(null);
  const [trackingData, setTrackingData] = useState<any>(null); // Store tracking info from API
  const [deliveryData, setDeliveryData] = useState<any>(null); // Store delivery info from API
  // State tạm để lưu deliveryId nếu selectedOrder không có
  const [fallbackDeliveryId, setFallbackDeliveryId] = useState<string | number | null>(null);
  // Đảm bảo fetch delivery/tracking luôn đúng order, tránh race condition
  const currentOrderIdRef = useRef<number | null>(null);
  useEffect(() => {
    setDeliveryData(null);
    setTrackingData(null);
    setFallbackDeliveryId(null);
    if (!selectedOrder?.id) return;
    currentOrderIdRef.current = selectedOrder.id;
    if (!selectedOrder?.delivery?.id) {
      const fetchDelivery = async () => {
        const thisOrderId = selectedOrder.id;
        try {
          const res = await fetch(`http://localhost:8080/api/deliveries?orderId=${thisOrderId}`, {
            headers: {
              'Authorization': `Bearer ${localStorage.getItem('token')}`
            }
          });
          if (res.ok) {
            const deliveries = await res.json();
            const correctDelivery = Array.isArray(deliveries)
              ? deliveries.find(d => (d.orderId === thisOrderId || d.order?.id === thisOrderId))
              : null;
            if (correctDelivery && currentOrderIdRef.current === thisOrderId) {
              setFallbackDeliveryId(correctDelivery.id);
              setDeliveryData(correctDelivery);
              console.log('✅ Fetched delivery by orderId:', thisOrderId, '-> deliveryId:', correctDelivery.id);
              logTrackingConsistency('After fetching delivery data', selectedOrder, correctDelivery, trackingData);
            } else if (!correctDelivery) {
              setFallbackDeliveryId(null);
              setDeliveryData(null);
              console.log('⚠️ No correct delivery found for orderId:', thisOrderId, deliveries);
            }
          } else {
            setFallbackDeliveryId(null);
            setDeliveryData(null);
            console.log('❌ Failed to fetch delivery, status:', res.status);
          }
        } catch (err) {
          setFallbackDeliveryId(null);
          setDeliveryData(null);
          console.error('❌ Error fetching delivery by orderId:', err);
        }
      };
      fetchDelivery();
    }
  }, [selectedOrder]);
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
  const MAPBOX_TOKEN =
    import.meta.env.VITE_MAPBOX_ACCESS_TOKEN ||
    "pk.eyJ1IjoieHVhbmh1eTEiLCJhIjoiY21lN3liN21tMDlzaTJtbXF3MjU0Z2JzaSJ9.vmH3qH_f7qf1ewBC_pJoSg";

  console.log('🗺️ MapboxTrackingMap: MAPBOX_TOKEN length:', MAPBOX_TOKEN?.length || 0);

  // Fetch vehicle position every 5s
  useEffect(() => {
    // Get delivery ID from selectedOrder - tracking theo delivery_id, KHÔNG phải vehicleId
    const deliveryId = selectedOrder?.delivery?.id;
    const orderId = selectedOrder?.id;
    
    console.log('🔍 MapboxTrackingMap: selectedOrder changed:', selectedOrder);
    console.log('🔍 MapboxTrackingMap: deliveryId found:', deliveryId);
    console.log('🔍 MapboxTrackingMap: orderId found:', orderId);
    console.log('🔍 MapboxTrackingMap: delivery object:', selectedOrder?.delivery);
    
    if (!deliveryId && !orderId) {
      console.warn('⚠️ MapboxTrackingMap: No deliveryId or orderId found, cannot fetch tracking data');
      return;
    }

    // Function để fetch delivery data cho popup (chỉ set nếu orderId vẫn là selectedOrder.id)
    const fetchDeliveryData = async (orderId: number) => {
      const currentOrderId = orderId;
      try {
        const deliveryUrl = `http://localhost:8080/api/deliveries?orderId=${currentOrderId}`;
        console.log('🔍 MapboxTrackingMap: Fetching delivery data from:', deliveryUrl);
        const res = await fetch(deliveryUrl, {
          headers: {
            'Authorization': `Bearer ${localStorage.getItem('token')}`
          }
        });
        if (res.ok) {
          const deliveries = await res.json();
          if (Array.isArray(deliveries) && deliveries.length > 0) {
            // Chỉ set nếu orderId vẫn là selectedOrder.id
            if (selectedOrder?.id === currentOrderId) {
              setDeliveryData(deliveries[0]);
              logTrackingConsistency('After fetchDeliveryData', selectedOrder, deliveries[0], trackingData);
              return deliveries[0];
            } else {
              console.log('⚠️ Skipped setDeliveryData: selectedOrder.id changed during fetch.');
            }
          } else {
            console.log('⚠️ No deliveries found for orderId:', currentOrderId);
          }
        } else {
          console.log('❌ Failed to fetch delivery data, status:', res.status);
        }
      } catch (error) {
        console.error('❌ MapboxTrackingMap: Error fetching delivery data:', error);
      }
      return null;
    };

    const useStoreCoordinatesAsDefault = () => {
      // Chỉ sử dụng tọa độ thực tế của store để hiển thị map, KHÔNG lưu tracking
      if (selectedOrder?.store?.latitude && selectedOrder?.store?.longitude) {
        const storeCoords: [number, number] = [selectedOrder.store.longitude, selectedOrder.store.latitude];
        console.log('🔍 MapboxTrackingMap: Using actual store coordinates for display only:', storeCoords);
        console.log('🔍 MapboxTrackingMap: Store info:', selectedOrder.store);
        // CHỈ set position để hiển thị trên map, KHÔNG lưu vào database
        setVehiclePos(storeCoords);
        if (map.current) {
          map.current.flyTo({
            center: storeCoords,
            speed: 1.2,
            curve: 1.5,
          });
        }
      } else {
        // Không có tọa độ store thì không cập nhật vị trí xe, chỉ log cảnh báo
        console.warn('⚠️ MapboxTrackingMap: Store does not have coordinates, cannot set vehicle position!');
      }
    };
    
    const fetchVehiclePos = async () => {
      console.log('🔍 MapboxTrackingMap: Fetching tracking data for deliveryId:', deliveryId);
      
      try {
        // Ưu tiên fetch theo delivery_id từ delivery_tracking table
        let apiUrl = '';
        let trackingSource = '';
        const currentOrderId = selectedOrder?.id;
        if (deliveryId) {
          apiUrl = `http://localhost:8080/api/tracking/delivery/${deliveryId}/current`;
          trackingSource = 'delivery_tracking table (by delivery_id)';
        } else if (orderId) {
          apiUrl = `http://localhost:8080/api/tracking/order/${orderId}/current`;
          trackingSource = 'delivery_tracking table (by order_id)';
        }
        console.log('🔍 MapboxTrackingMap: API URL:', apiUrl);
        console.log('🔍 MapboxTrackingMap: Tracking source:', trackingSource);
        const res = await fetch(apiUrl, {
          headers: {
            'Authorization': `Bearer ${localStorage.getItem('token')}`
          }
        });
        console.log('🔍 MapboxTrackingMap: API response status:', res.status);
        if (res.ok) {
          const data = await res.json();
          console.log('🔍 MapboxTrackingMap: tracking data from', trackingSource, ':', data);
          // Chỉ set nếu orderId vẫn là selectedOrder.id
          if (selectedOrder?.id === currentOrderId) {
            setTrackingData({
              ...data,
              source: trackingSource
            });
            if (data && typeof data.latitude === "number" && typeof data.longitude === "number") {
              const coords: [number, number] = [data.longitude, data.latitude];
              console.log('🔍 MapboxTrackingMap: Setting vehicle position from tracking data:', coords);
              setVehiclePos(coords);
              if (map.current) {
                map.current.flyTo({
                  center: coords,
                  speed: 1.2,
                  curve: 1.5,
                });
              }
              return; // Thành công, không cần fallback
            } else {
              console.log('🔍 MapboxTrackingMap: No valid coordinates in tracking API response');
            }
          } else {
            console.log('⚠️ Skipped setTrackingData: selectedOrder.id changed during fetch.');
          }
        } else {
          console.log('🔍 MapboxTrackingMap: Tracking API response not ok:', res.status);
          const errorText = await res.text();
          console.log('🔍 MapboxTrackingMap: Error response:', errorText);
        }
      } catch (err) {
        console.error("❌ Error fetching tracking data from delivery_tracking:", err);
      }
      
      // Fallback: Sử dụng tọa độ store và lưu vào tracking
      console.log('🔍 MapboxTrackingMap: Using store coordinates as fallback');
      useStoreCoordinatesAsDefault();
    };
    
    // Luôn cập nhật vehicle position với store coordinates khi chọn order mới
    console.log('🔍 MapboxTrackingMap: Updating vehicle position with store coordinates');
    useStoreCoordinatesAsDefault();
    
    // Fetch delivery data for popup
    if (orderId) {
      fetchDeliveryData(orderId);
    }
    
    fetchVehiclePos();
    const interval = setInterval(fetchVehiclePos, 3600000); // 1 tiếng
    return () => clearInterval(interval);
  }, [selectedOrder]);

  // Debug selectedOrder changes
  useEffect(() => {
    console.log('MapboxTrackingMap: selectedOrder changed:', selectedOrder);
    console.log('MapboxTrackingMap: Vehicle info:', selectedOrder?.vehicle);
    console.log('MapboxTrackingMap: Vehicle ID:', selectedOrder?.vehicle?.id);
    console.log('MapboxTrackingMap: Current driver:', selectedOrder?.vehicle?.currentDriver);
  }, [selectedOrder]);

  // Draw vehicle marker
  useEffect(() => {
    // Luôn clear marker/popup cũ khi selectedOrder, vehiclePos, trackingData, deliveryData thay đổi
    if (realTruckMarker.current) {
      realTruckMarker.current.remove();
      realTruckMarker.current = null;
    }
    if (!map.current || !vehiclePos || !isLoaded) return;
    // Check if current driver exists
    const currentDriver = selectedOrder?.vehicle?.currentDriver;
    if (!currentDriver) return;

    const truckEl = document.createElement("div");
    truckEl.style.width = "32px";
    truckEl.style.height = "32px";
    truckEl.style.display = "flex";
    truckEl.style.alignItems = "center";
    truckEl.style.justifyContent = "center";
    truckEl.style.fontSize = "28px";
    truckEl.style.background = "rgba(255,255,255,0.85)";
    truckEl.style.borderRadius = "50%";
    truckEl.style.boxShadow = "0 2px 8px rgba(0,0,0,0.15)";
    truckEl.style.cursor = "pointer";
    truckEl.innerHTML = "🚚";

    // Tạo popup với thông tin tracking, luôn lấy từ state mới nhất
    const createTrackingPopup = () => {
      // Lấy dữ liệu trực tiếp từ selectedOrder để đảm bảo đồng bộ
      const orderId = selectedOrder?.id || 'N/A';
      const vehicleId = selectedOrder?.vehicle?.id || 'N/A';
      const driverId = selectedOrder?.vehicle?.currentDriver?.id || 'N/A';
      const driverName = selectedOrder?.vehicle?.currentDriver?.fullName || 'N/A';
      // Ưu tiên lấy delivery info từ trackingData.delivery nếu có, fallback sang deliveryData
      let deliveryId = 'N/A';
      let deliveryStatus = 'N/A';
      let deliveryAddress = selectedOrder?.address?.address || 'N/A';
      let deliveryContactName = 'N/A';
      let deliveryContactPhone = 'N/A';
      if (trackingData && trackingData.delivery) {
        deliveryId = trackingData.delivery.id || 'N/A';
        deliveryStatus = trackingData.delivery.status || 'N/A';
        // Nếu có order/address trong trackingData.delivery, ưu tiên lấy
        deliveryAddress = trackingData.delivery.order?.address?.address || deliveryAddress;
        deliveryContactName = trackingData.delivery.order?.address?.contactName || 'N/A';
        deliveryContactPhone = trackingData.delivery.order?.address?.contactPhone || 'N/A';
      } else if (deliveryData && (deliveryData.orderId === selectedOrder?.id || deliveryData.order?.id === selectedOrder?.id)) {
        deliveryId = deliveryData.id || 'N/A';
        deliveryStatus = deliveryData.status || 'N/A';
        deliveryAddress = deliveryData.order?.address?.address || deliveryAddress;
        deliveryContactName = deliveryData.order?.address?.contactName || 'N/A';
        deliveryContactPhone = deliveryData.order?.address?.contactPhone || 'N/A';
      }
      // TrackingId lấy từ trackingData nếu có và đúng order
      let trackingId = 'N/A', lat = 'N/A', lng = 'N/A', location = 'N/A', timestamp = '';
      let trackingWarning = '';
      // Nếu có trackingData và có id thì luôn show trackingId
      if (trackingData && trackingData.id) {
        trackingId = trackingData.id;
        lat = trackingData.latitude ? trackingData.latitude.toFixed(6) : (vehiclePos ? vehiclePos[1].toFixed(6) : 'N/A');
        lng = trackingData.longitude ? trackingData.longitude.toFixed(6) : (vehiclePos ? vehiclePos[0].toFixed(6) : 'N/A');
        location = trackingData.location || selectedOrder?.store?.storeName || 'Điểm tracking mới';
        timestamp = trackingData.timestamp ? new Date(trackingData.timestamp).toLocaleString('vi-VN') : new Date().toLocaleString('vi-VN');
      } else {
        trackingWarning = '<div style="color: #FFD700; font-size: 13px; margin-bottom: 8px;">Chưa có tracking cho đơn này hoặc tracking không khớp!</div>';
        lat = vehiclePos ? vehiclePos[1].toFixed(6) : 'N/A';
        lng = vehiclePos ? vehiclePos[0].toFixed(6) : 'N/A';
        location = selectedOrder?.store?.storeName || 'Chưa có tracking';
        timestamp = new Date().toLocaleString('vi-VN');
      }
      // Debug log để kiểm tra dữ liệu
      logTrackingConsistency('Before creating popup', selectedOrder, deliveryData, trackingData);
      return new mapboxgl.Popup({
        closeButton: false,
        closeOnClick: false,
        closeOnMove: true,
        offset: [0, -40],
        className: 'tracking-popup'
      }).setHTML(`
        <div style="
          padding: 12px; 
          font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          color: white;
          border-radius: 8px;
          box-shadow: 0 4px 20px rgba(0,0,0,0.25);
          min-width: 300px;
          max-width: 380px;
          font-size: 12px;
        ">
          <div style="font-size: 14px; font-weight: bold; margin-bottom: 8px; text-align: center;">
            🚛 Thông tin Tracking
          </div>
          ${trackingWarning}
          <div style="font-size: 13px; line-height: 1.6;">
            <div style="margin-bottom: 6px; background: rgba(255,255,255,0.1); padding: 6px; border-radius: 4px;">
              <strong>📦 Đơn hàng:</strong> #${orderId} | <strong>🚚 Xe:</strong> #${vehicleId}
              <br><strong>🚚 Delivery:</strong> #${deliveryId} | <strong>📊:</strong> ${deliveryStatus}
            </div>
            <div style="margin-bottom: 6px; background: rgba(255,255,255,0.1); padding: 6px; border-radius: 4px;">
              <strong>👨‍💼 Tài xế:</strong> ${driverName} | <strong>🆔 Tracking:</strong> #${trackingId}
            </div>
            <div style="margin-bottom: 6px; background: rgba(255,255,255,0.1); padding: 6px; border-radius: 4px;">
              <strong>📍 GPS:</strong> ${lat}, ${lng}
              <br><strong>📍 Tại:</strong> ${location}
              <div style="font-size: 10px; color: #90EE90; margin-top: 2px;">
                ${trackingId !== 'N/A'
                  ? `🟢 Từ Tracking API (ID: ${trackingId})`
                  : '🟡 Từ Store (Fallback)'}
              </div>
            </div>
            <div style="margin-bottom: 8px; background: rgba(255,255,255,0.1); padding: 8px; border-radius: 6px;">
              <strong style="color: #FFE066;">� Thông tin Giao hàng</strong>
              <div style="margin-top: 4px; color: #E0E0E0; font-size: 12px;">
                ${deliveryAddress}
              </div>
              ${deliveryContactName !== 'N/A' ? `<div style=\"color: #E0E0E0; font-size: 12px;\">Liên hệ: ${deliveryContactName}</div>` : ''}
              ${deliveryContactPhone !== 'N/A' ? `<div style=\"color: #E0E0E0; font-size: 12px;\">SĐT: ${deliveryContactPhone}</div>` : ''}
            </div>
            <div style="font-size: 10px; color: #C0C0C0; text-align: center; margin-top: 6px; border-top: 1px solid rgba(255,255,255,0.1); padding-top: 4px;">
              ⏰ ${timestamp}
            </div>
          </div>
        </div>
      `);
    };

    // Thêm event listeners cho hover
    let hoverPopup: mapboxgl.Popup | null = null;
    truckEl.addEventListener('mouseenter', () => {
      if (hoverPopup) hoverPopup.remove();
      const popup = createTrackingPopup();
      if (popup) {
        hoverPopup = popup;
        hoverPopup.addTo(map.current!);
        realTruckMarker.current?.setPopup(hoverPopup);
      }
    });
    truckEl.addEventListener('mouseleave', () => {
      if (hoverPopup) {
        hoverPopup.remove();
        hoverPopup = null;
      }
    });
    realTruckMarker.current = new mapboxgl.Marker(truckEl)
      .setLngLat(vehiclePos)
      .addTo(map.current);
  }, [vehiclePos, isLoaded, selectedOrder, trackingData, deliveryData]);


  // Helper: Render tracking popup content (always use selectedOrder for order_id)
  const renderTrackingPopup = () => {
    // Luôn lấy orderId/deliveryId/trackingId từ selectedOrder để đồng bộ với header
    const order = selectedOrder;
    const orderId = order?.id || 'N/A';
    const vehicle = order?.vehicle;
    const vehicleId = vehicle?.id || 'N/A';
    const driver = vehicle?.currentDriver;
    const driverName = driver?.fullName || 'N/A';
    // Nếu deliveryData khớp orderId thì lấy, không thì bỏ qua
    let deliveryId = 'N/A';
    if (deliveryData && (deliveryData.orderId === order?.id || deliveryData.order?.id === order?.id)) {
      deliveryId = deliveryData.id || 'N/A';
    }

    // Xử lý trường hợp không có deliveryData
    if (!deliveryData) {
      return (
        <div className="rounded-2xl bg-yellow-100/90 border border-yellow-300 shadow-xl p-4 min-w-[320px] max-w-[400px] mx-auto flex flex-col gap-2">
          <div className="font-bold text-yellow-900 text-lg flex items-center gap-2">
            <span role="img" aria-label="warning">⚠️</span> Chưa có giao hàng cho đơn này
          </div>
          <div className="text-base text-yellow-800">{t('orders.orderNumber', 'Order')}: <span className="text-blue-700">#{orderId}</span></div>
          <div className="text-sm text-yellow-700">Hệ thống chưa tạo delivery cho đơn này. Vui lòng kiểm tra lại việc gán xe và backend.</div>
        </div>
      );
    }

    // Xử lý trường hợp có delivery nhưng không có trackingData
    if (!trackingData) {
      return (
        <div className="rounded-2xl bg-orange-100/90 border border-orange-300 shadow-xl p-4 min-w-[320px] max-w-[400px] mx-auto flex flex-col gap-2">
          <div className="font-bold text-orange-900 text-lg flex items-center gap-2">
            <span role="img" aria-label="warning">🛈</span> Chưa có tracking cho đơn này
          </div>
          <div className="text-base text-orange-800">{t('orders.orderNumber', 'Order')}: <span className="text-blue-700">#{orderId}</span></div>
          <div className="text-base text-orange-800">Delivery: <span className="text-blue-700">#{deliveryId}</span></div>
          <div className="text-sm text-orange-700">Hệ thống chưa ghi nhận tracking cho delivery này.</div>
        </div>
      );
    }

    // Tracking chỉ hiển thị nếu trackingData thuộc đúng order hiện tại
    let trackingId = 'N/A', lat = 'N/A', lng = 'N/A', location = 'N/A', notes = '', statusName = 'N/A', statusDesc = '', timestamp = '';
    if (trackingData && trackingData.id) {
      trackingId = trackingData.id;
      lat = trackingData.latitude ?? 'N/A';
      lng = trackingData.longitude ?? 'N/A';
      location = trackingData.location || 'N/A';
      notes = trackingData.notes || '';
      statusName = trackingData.status?.name || 'N/A';
      statusDesc = trackingData.status?.description || '';
      timestamp = trackingData.timestamp || '';
    } else {
      // Nếu không có trackingData thì không hiển thị gì
      return null;
    }
    return (
      <div className="rounded-2xl bg-indigo-100/90 border border-indigo-300 shadow-xl p-4 min-w-[320px] max-w-[400px] mx-auto flex flex-col gap-2">
        <div className="font-bold text-indigo-900 text-lg flex items-center gap-2">
          <span role="img" aria-label="truck">🚚</span> Thông tin Tracking
        </div>
        <div className="flex flex-wrap gap-2 text-base font-semibold text-indigo-800">
          <span>Đơn hàng: <span className="text-blue-700">#{orderId}</span></span>
          <span>| Xe: <span className="text-blue-700">#{vehicleId}</span></span>
          <span>| Delivery: <span className="text-blue-700">#{deliveryId}</span></span>
          <span>| ID: <span className="text-blue-700">{trackingId}</span></span>
        </div>
        <div className="flex flex-wrap gap-2 text-base text-indigo-700">
          <span>🧑‍✈️ Tài xế: <span className="font-bold">{driverName}</span></span>
          <span>| Trạng thái: <span className="font-bold">{statusName}</span></span>
        </div>
        <div className="text-sm text-gray-700">
          <span>📍 GPS: <span className="font-mono">{lat}, {lng}</span></span>
        </div>
        <div className="text-sm text-gray-700">
          <span>🏠 Tại: <span className="font-semibold">{location}</span></span>
        </div>
        {notes && <div className="text-xs text-gray-500 italic">{notes}</div>}
        <div className="text-xs text-gray-500">{statusDesc}</div>
        <div className="text-xs text-gray-400 text-right mt-2">{timestamp}</div>
      </div>
    );
  };

  // Update route when selectedOrder changes
  useEffect(() => {
    // Nếu không có selectedOrder hoặc selectedOrder không có assignedVehicle, xóa route
  if (!selectedOrder || !selectedOrder.vehicle) {
      setStart(null);
      setEnd(null);
      setRoute(null);
      setWaypoints([]);
      setVehiclePos(null);

      // Xóa route khỏi map nếu tồn tại (an toàn)
      if (map.current) {
        if (map.current.getLayer('route')) {
          map.current.removeLayer('route');
        }
        if (map.current.getSource('route')) {
          map.current.removeSource('route');
        }
        if (map.current.getLayer('traveled-path-layer')) {
          map.current.removeLayer('traveled-path-layer');
        }
        if (map.current.getSource('traveled-path')) {
          map.current.removeSource('traveled-path');
        }
      }

      // Xóa markers
      markers.current.forEach((marker) => marker.remove());
      markers.current = [];

      // Xóa vehicle marker
      if (realTruckMarker.current) {
        realTruckMarker.current.remove();
        realTruckMarker.current = null;
      }

      return;
    }

  if (selectedOrder && selectedOrder.vehicle && selectedOrder.store && selectedOrder.address) {
      console.log('🗺️ MapboxTrackingMap: Order has all required data');
      console.log('🗺️ MapboxTrackingMap: Store coords:', selectedOrder.store.latitude, selectedOrder.store.longitude);
      console.log('🗺️ MapboxTrackingMap: Address coords:', selectedOrder.address.latitude, selectedOrder.address.longitude);
      
      const store = selectedOrder.store;
      const address = selectedOrder.address;
      if (
        store.latitude &&
        store.longitude &&
        address.latitude &&
        address.longitude
      ) {
        const startCoord: [number, number] = [store.longitude, store.latitude];
        const endCoord: [number, number] = [
          address.longitude,
          address.latitude,
        ];
        setStart(startCoord);
        setEnd(endCoord);
        // Fetch route from Mapbox Directions API
        const fetchRoute = async () => {
          const url = `https://api.mapbox.com/directions/v5/mapbox/driving/${startCoord.join(
            ","
          )};${endCoord.join(
            ","
          )}?geometries=geojson&access_token=${MAPBOX_TOKEN}`;
          
          console.log('🗺️ MapboxTrackingMap: Fetching route from:', url);
          console.log('🗺️ MapboxTrackingMap: Start coord:', startCoord);
          console.log('🗺️ MapboxTrackingMap: End coord:', endCoord);
          
          try {
            const res = await fetch(url);
            console.log('🗺️ MapboxTrackingMap: Route API response status:', res.status);
            
            if (res.ok) {
              const data = await res.json();
              console.log('🗺️ MapboxTrackingMap: Route data:', data);
              
              if (data.routes && data.routes[0]) {
                console.log('✅ MapboxTrackingMap: Route found, setting route and waypoints');
                setRoute(data.routes[0]);
                setWaypoints(data.routes[0].geometry.coordinates);
                
                // Fit map to route bounds
                if (map.current && data.routes[0].geometry.coordinates.length > 0) {
                  const coordinates = data.routes[0].geometry.coordinates;
                  const bounds = new mapboxgl.LngLatBounds();
                  coordinates.forEach((coord: [number, number]) => bounds.extend(coord));
                  map.current.fitBounds(bounds, { padding: 50 });
                }
              } else {
                console.warn('⚠️ MapboxTrackingMap: No routes found in response');
              }
            } else {
              const errorText = await res.text();
              console.error('❌ MapboxTrackingMap: Route API error:', res.status, errorText);
            }
          } catch (err) {
            console.error("❌ MapboxTrackingMap: Error fetching route:", err);
          }
        };
        fetchRoute();
      }
    }
  }, [selectedOrder, MAPBOX_TOKEN]);

  // Draw start/end markers, route line, và traveled path (màu xám)
  useEffect(() => {
    console.log('🗺️ MapboxTrackingMap: Draw route useEffect called');
    console.log('🗺️ MapboxTrackingMap: map.current:', !!map.current);
    console.log('🗺️ MapboxTrackingMap: isLoaded:', isLoaded);
    console.log('🗺️ MapboxTrackingMap: start:', start);
    console.log('🗺️ MapboxTrackingMap: end:', end);
    console.log('🗺️ MapboxTrackingMap: route:', !!route);
    
    if (!map.current || !isLoaded || !start || !end || !route) {
      console.log('🗺️ MapboxTrackingMap: Skipping route draw - missing requirements');
      return;
    }
    // Remove old markers
    markers.current.forEach((marker) => marker.remove());
    markers.current = [];
    // Start marker
    const startEl = document.createElement("div");
    startEl.style.width = "16px";
    startEl.style.height = "16px";
    startEl.style.background = "#22c55e";
    startEl.style.border = "2px solid #fff";
    startEl.style.borderRadius = "50%";
    startEl.style.boxShadow = "0 1px 4px rgba(0,0,0,0.12)";
    const startMarker = new mapboxgl.Marker(startEl)
      .setLngLat(start)
      .addTo(map.current);
    markers.current.push(startMarker);
    // End marker
    const endEl = document.createElement("div");
    endEl.style.width = "16px";
    endEl.style.height = "16px";
    endEl.style.background = "#ef4444";
    endEl.style.border = "2px solid #fff";
    endEl.style.borderRadius = "50%";
    endEl.style.boxShadow = "0 1px 4px rgba(0,0,0,0.12)";
    const endMarker = new mapboxgl.Marker(endEl)
      .setLngLat(end)
      .addTo(map.current);
    markers.current.push(endMarker);
    // Draw route line
    console.log('🗺️ MapboxTrackingMap: Drawing route line with coordinates:', route.geometry.coordinates.length);
    const routeFeature: GeoJSON.Feature<GeoJSON.LineString> = {
      type: "Feature",
      geometry: {
        type: "LineString",
        coordinates: route.geometry.coordinates,
      },
      properties: {},
    };
    if (map.current.getSource("route")) {
      console.log('🗺️ MapboxTrackingMap: Updating existing route source');
      (map.current.getSource("route") as mapboxgl.GeoJSONSource).setData(
        routeFeature
      );
    } else {
      console.log('🗺️ MapboxTrackingMap: Adding new route source and layer');
      map.current.addSource("route", {
        type: "geojson",
        data: routeFeature,
      });
      map.current.addLayer({
        id: "route",
        type: "line",
        source: "route",
        layout: { "line-join": "round", "line-cap": "round" },
        paint: { "line-color": "#3b82f6", "line-width": 5 },
      });
    }

    // Draw traveled path (gray)
    const traveledPath = getTraveledPath();
    const traveledFeature: GeoJSON.Feature<GeoJSON.LineString> = {
      type: "Feature",
      geometry: {
        type: "LineString",
        coordinates: traveledPath,
      },
      properties: {},
    };
    if (map.current.getSource("traveled-path")) {
      (
        map.current.getSource("traveled-path") as mapboxgl.GeoJSONSource
      ).setData(traveledFeature);
    } else if (traveledPath.length > 1) {
      map.current.addSource("traveled-path", {
        type: "geojson",
        data: traveledFeature,
      });
      map.current.addLayer({
        id: "traveled-path-layer",
        type: "line",
        source: "traveled-path",
        layout: { "line-join": "round", "line-cap": "round" },
        paint: { "line-color": "#888", "line-width": 5, "line-opacity": 0.7 },
      });
    }
  }, [route, start, end, vehiclePos, waypoints, isLoaded]);

  // Initialize map
  useEffect(() => {
    if (map.current) return;
    if (!mapContainer.current) return;
    try {
      mapboxgl.accessToken = MAPBOX_TOKEN;
      map.current = new mapboxgl.Map({
        container: mapContainer.current,
        style: "mapbox://styles/mapbox/satellite-streets-v12",
        center: [106.660172, 10.762622],
        zoom: 12,
      });
      map.current.on("load", () => {
        setIsLoaded(true);
        // Force resize after load to fix white space issue
        setTimeout(() => {
          map.current && map.current.resize();
        }, 200);
      });
      map.current.on("error", (error) => {
        console.error("MapboxTrackingMap: Map error:", error);
      });
    } catch (error) {
      console.error("MapboxTrackingMap: Initialization error:", error);
    }
    // Force resize after mount (in case of login/tab switch)
    setTimeout(() => {
      map.current && map.current.resize();
    }, 400);
    return () => {
      markers.current.forEach((marker) => marker.remove());
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

  return (
    <div className="bg-white rounded-xl shadow-lg p-4 h-full min-h-[300px] w-full flex flex-col">
      {selectedOrder && selectedOrder.vehicle && selectedOrder.vehicle.currentDriver && (
        <div className="mb-4">
          <h3 className="text-lg font-bold text-gray-800 mb-2">{t('dashboard.dispatcher.orders.trackingTitle')}</h3>
        </div>
      )}
      {selectedOrder && selectedOrder.vehicle && selectedOrder.vehicle.currentDriver && (
        <div className="mb-4 p-3 bg-blue-50 rounded-lg border border-blue-200">
          <div className="text-sm font-semibold text-blue-900 mb-1">
            {t('orders.orderNumber', 'Order')} #{selectedOrder.id}
          </div>
          <div className="text-xs text-gray-600">
            <div className="flex items-center gap-2 mb-1">
              <span className="w-3 h-3 bg-green-500 rounded-full inline-block"></span>
              <span>
                <strong>{t('common.from', 'From')}:</strong> {selectedOrder.store?.storeName} -{" "}
                {selectedOrder.store?.address}
              </span>
            </div>
            <div className="flex items-center gap-2">
              <span className="w-3 h-3 bg-red-500 rounded-full inline-block"></span>
              <span>
                <strong>{t('common.to', 'To')}:</strong> {selectedOrder.address?.address}
              </span>
            </div>
            <div className="flex items-center gap-2 mt-1">
              <span className="w-3 h-3 bg-blue-500 rounded-full inline-block"></span>
              <span>
                <strong>{t('dashboard.dispatcher.vehicles.licensePlate')}:</strong> {selectedOrder.vehicle?.licensePlate || t('common.unknown', 'Unknown')}
                {" | "}
                <strong>{t('dashboard.dispatcher.drivers.name')}:</strong> {selectedOrder.vehicle.currentDriver.fullName}
              </span>
            </div>
            {/* Đã xóa nút làm mới đường đi theo yêu cầu */}
          </div>
          {route &&
            typeof route.distance === "number" &&
            typeof route.duration === "number" && (
              <div className="mt-2 text-xs text-blue-600">
                <strong>{t('common.distance', 'Distance')}:</strong>{" "}
                {(route.distance / 1000).toFixed(1)} km |
                <strong> {t('common.duration', 'Duration')}:</strong> {(() => {
                  const totalMinutes = Math.round(route.duration / 60);
                  const hours = Math.floor(totalMinutes / 60);
                  const minutes = totalMinutes % 60;
                  if (hours > 0) {
                    return `${hours}h ${minutes}m`;
                  } else {
                    return `${minutes} min`;
                  }
                })()}
              </div>
            )}
        </div>
      )}
      {selectedOrder && !selectedOrder.vehicle && (
        <div className="mb-4 p-3 bg-gray-50 rounded-lg border border-gray-200">
          <div className="text-sm font-semibold text-gray-700 mb-1">
            {t('orders.orderNumber', 'Order')} #{selectedOrder.id}
          </div>
          <div className="text-xs text-gray-500">
            <div className="flex items-center gap-2">
              <span className="w-3 h-3 bg-orange-500 rounded-full inline-block"></span>
              <span>{t('dashboard.dispatcher.orders.notAssignedVehicle', 'This order has not been assigned a vehicle')}</span>
            </div>
          </div>
        </div>
      )}
      {!selectedOrder && (
        <div className="mb-4 p-3 bg-gray-50 rounded-lg border border-gray-200">
          <div className="text-sm text-gray-500 text-center">
            {t('dashboard.dispatcher.orders.selectOrderToViewMap', 'Select an order to view route on map')}
          </div>
        </div>
      )}
      <div
        ref={mapContainer}
        className="flex-1 min-h-[250px] h-[350px] w-full rounded-lg border-2 border-blue-500 relative overflow-hidden"
        style={{
          minHeight: "250px",
          height: "350px",
          width: "100%",
          position: "relative",
          border: "2px solid blue",
        }}
      >
        {!isLoaded && (
          <div className="absolute inset-0 bg-gray-100 flex items-center justify-center z-10 pointer-events-none">
            <div className="text-gray-600">
              <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto mb-2"></div>
              {t('common.loading', 'Loading')}...
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
