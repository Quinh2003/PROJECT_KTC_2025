import { useState, useEffect } from "react";

// Mock data cho demo
const mockOrders = [
  {
    id: "FR001",
    status: "Đang vận chuyển",
    priority: "Cao",
    customer: "ABC Company",
    from: { lat: 10.7769, lng: 106.7009, address: "123 Đường Lê Lợi, Q1, TP.HCM" },
    to: { lat: 10.7829, lng: 106.6934, address: "456 Đường Nguyễn Huệ, Q3, TP.HCM" },
    driver: "Tài xế Demo - 51A-12345",
    vehicle: "51A-12345",
    currentLocation: { lat: 10.7799, lng: 106.6971 }, // Vị trí hiện tại của xe
    estimatedTime: "15 phút",
    date: "14/1/2025",
  },
  {
    id: "FR002",
    status: "Đã phân công",
    priority: "Trung bình",
    customer: "XYZ Corp",
    from: { lat: 10.7858, lng: 106.7019, address: "789 Đường Điện Biên Phủ, Q3, TP.HCM" },
    to: { lat: 10.7718, lng: 106.6580, address: "321 Đường Cách Mạng Tháng 8, Q10, TP.HCM" },
    driver: "Lê Văn C - 51C-11111",
    vehicle: "51C-11111",
    currentLocation: null, // Chưa xuất phát
    estimatedTime: "Chưa khởi hành",
    date: "30/7/2025",
  },
];

interface RouteMapProps {
  selectedOrderId?: string;
  onOrderSelect?: (orderId: string) => void;
}

export default function RouteMap({ selectedOrderId, onOrderSelect }: RouteMapProps) {
  const [orders, setOrders] = useState(mockOrders);
  const [selectedOrder, setSelectedOrder] = useState<string | null>(selectedOrderId || null);
  const [isLiveTracking, setIsLiveTracking] = useState(false);

  // Simulate real-time location updates
  useEffect(() => {
    if (!isLiveTracking) return;

    const interval = setInterval(() => {
      setOrders(prevOrders => 
        prevOrders.map(order => {
          if (order.status === "Đang vận chuyển" && order.currentLocation) {
            // Simulate movement towards destination
            const deltaLat = (order.to.lat - order.currentLocation.lat) * 0.1;
            const deltaLng = (order.to.lng - order.currentLocation.lng) * 0.1;
            
            return {
              ...order,
              currentLocation: {
                lat: order.currentLocation.lat + deltaLat,
                lng: order.currentLocation.lng + deltaLng,
              }
            };
          }
          return order;
        })
      );
    }, 3000); // Update every 3 seconds

    return () => clearInterval(interval);
  }, [isLiveTracking]);

  const handleOrderClick = (orderId: string) => {
    setSelectedOrder(orderId === selectedOrder ? null : orderId);
    onOrderSelect?.(orderId);
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case "Đang vận chuyển": return "bg-green-500/20 border-green-400/30 text-green-300";
      case "Đã phân công": return "bg-blue-500/20 border-blue-400/30 text-blue-300";
      case "Hoàn thành": return "bg-gray-500/20 border-gray-400/30 text-gray-300";
      default: return "bg-yellow-500/20 border-yellow-400/30 text-yellow-300";
    }
  };

  const selectedOrderData = orders.find(order => order.id === selectedOrder);

  return (
    <div className="bg-white border border-red-100 shadow-lg rounded-xl p-6 transition-all duration-200 h-full min-h-[400px]">
      <div className="flex justify-between items-center mb-4">
        <h3 className="text-xl font-bold text-gray-800">Real-time Route Map</h3>
        <div className="flex gap-2">
          <button
            onClick={() => setIsLiveTracking(!isLiveTracking)}
            className={`px-3 py-1 rounded-lg text-sm font-medium transition-all ${
              isLiveTracking 
                ? "bg-green-600 text-white" 
                : "bg-gray-200 text-gray-700 hover:bg-gray-300"
            }`}
          >
            {isLiveTracking ? "🔴 Live" : "▶️ Start Tracking"}
          </button>
        </div>
      </div>

      {/* Map Visualization Area */}
      <div className="bg-gradient-to-br from-blue-50 to-green-50 rounded-lg p-4 h-64 mb-4 relative overflow-hidden">
        <div className="absolute inset-0 bg-gray-100 rounded-lg flex items-center justify-center">
          <div className="text-center">
            <div className="text-4xl mb-2">🗺️</div>
            <div className="text-gray-600 font-medium">Interactive Map</div>
            <div className="text-sm text-gray-500">
              {selectedOrderData ? `Tracking: ${selectedOrderData.id}` : "Select an order to view route"}
            </div>
          </div>
        </div>

        {/* Route visualization for selected order */}
        {selectedOrderData && (
          <div className="absolute inset-2 bg-white/80 rounded border-2 border-red-200 p-3">
            <div className="text-sm font-medium text-gray-800 mb-2">Route: {selectedOrderData.id}</div>
            <div className="space-y-1">
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 bg-blue-500 rounded-full"></div>
                <span className="text-xs text-gray-600">From: {selectedOrderData.from.address}</span>
              </div>
              {selectedOrderData.currentLocation && (
                <div className="flex items-center gap-2">
                  <div className="w-3 h-3 bg-yellow-500 rounded-full animate-pulse"></div>
                  <span className="text-xs text-gray-600">Current: Lat {selectedOrderData.currentLocation.lat.toFixed(4)}, Lng {selectedOrderData.currentLocation.lng.toFixed(4)}</span>
                </div>
              )}
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 bg-red-500 rounded-full"></div>
                <span className="text-xs text-gray-600">To: {selectedOrderData.to.address}</span>
              </div>
            </div>
          </div>
        )}
      </div>

      {/* Order List for Map */}
      <div className="space-y-2">
        <h4 className="font-semibold text-gray-700 mb-3">Active Orders</h4>
        {orders.map((order) => (
          <div 
            key={order.id}
            onClick={() => handleOrderClick(order.id)}
            className={`p-3 rounded-lg border cursor-pointer transition-all duration-200 ${
              selectedOrder === order.id 
                ? "border-red-300 bg-red-50 shadow-md" 
                : "border-gray-200 hover:border-red-200 hover:bg-gray-50"
            }`}
          >
            <div className="flex justify-between items-start">
              <div className="flex-1">
                <div className="flex items-center gap-2 mb-1">
                  <span className="font-bold text-gray-800">{order.id}</span>
                  <span className={`text-xs px-2 py-1 rounded border ${getStatusColor(order.status)}`}>
                    {order.status}
                  </span>
                </div>
                <div className="text-sm text-gray-600">
                  {order.driver} • {order.vehicle}
                </div>
                <div className="text-xs text-gray-500">
                  ETA: {order.estimatedTime}
                </div>
              </div>
              
              {order.status === "Đang vận chuyển" && (
                <div className="text-right">
                  <div className="w-2 h-2 bg-green-500 rounded-full animate-pulse"></div>
                  <div className="text-xs text-gray-500 mt-1">Live</div>
                </div>
              )}
            </div>
          </div>
        ))}
      </div>

      {/* Selected Order Details */}
      {selectedOrderData && (
        <div className="mt-4 p-3 bg-red-50 border border-red-200 rounded-lg">
          <h5 className="font-semibold text-red-800 mb-2">Order Details: {selectedOrderData.id}</h5>
          <div className="grid grid-cols-2 gap-2 text-sm">
            <div>
              <span className="text-gray-600">Customer:</span>
              <div className="font-medium text-gray-800">{selectedOrderData.customer}</div>
            </div>
            <div>
              <span className="text-gray-600">Priority:</span>
              <div className="font-medium text-gray-800">{selectedOrderData.priority}</div>
            </div>
            <div className="col-span-2">
              <span className="text-gray-600">Route:</span>
              <div className="text-xs text-gray-700">
                {selectedOrderData.from.address} → {selectedOrderData.to.address}
              </div>
            </div>
          </div>
        </div>
      )}
    </div>
  );
}