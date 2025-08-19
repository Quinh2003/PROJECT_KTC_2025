interface OrderListProps {
  onOrderSelect?: (orderId: string) => void;
  selectedOrderId?: string;
}

export default function OrderList({ onOrderSelect, selectedOrderId }: OrderListProps) {
  // Demo data
  const orders = [
    {
      id: "FR004",
      status: "Đã phân công",
      priority: "Trung bình",
      customer: "Khách hàng Demo",
      to: "123",
      from: "123",
      driver: "Lê Văn C - 51C-11111",
      date: "30/7/2025",
    },
    {
      id: "FR003",
      status: "Đã phân công",
      priority: "Trung bình",
      customer: "Khách hàng Demo",
      to: "123",
      from: "123",
      driver: "Lê Văn C - 51C-11111",
      date: "30/7/2025",
    },
    {
      id: "FR001",
      status: "Đang vận chuyển",
      priority: "Cao",
      customer: "Khách hàng Demo",
      to: "456 Đường Nguyễn Huệ, Q3, TP.HCM",
      from: "123 Đường Lê Lợi, Q1, TP.HCM",
      driver: "Tài xế Demo - 51A-12345",
      date: "14/1/2025",
    },
  ];

  const handleOrderClick = (orderId: string) => {
    onOrderSelect?.(orderId);
  };

  const getStatusColor = (status: string) => {
    switch (status) {
      case "Đang vận chuyển": return "bg-green-500/20 text-green-300 border-green-400/30";
      case "Đã phân công": return "bg-cyan-500/20 text-cyan-300 border-cyan-400/30";
      case "Hoàn thành": return "bg-gray-500/20 text-gray-300 border-gray-400/30";
      default: return "bg-orange-500/20 text-orange-300 border-orange-400/30";
    }
  };

  return (
    <div className="bg-white border border-red-100 shadow-lg rounded-xl p-6 transition-all duration-200">
      <div className="flex justify-between items-center mb-4">
        <div>
          <h3 className="text-xl font-bold text-gray-800">Total Orders</h3>
          <p className="text-gray-600">Track the status of all orders in the system</p>
        </div>
        <div className="text-2xl font-bold text-red-600">{orders.length}</div>
      </div>
      
      <div className="flex flex-col gap-3">
        {orders.map((order) => (
          <div 
            key={order.id} 
            onClick={() => handleOrderClick(order.id)}
            className={`rounded-xl p-4 border cursor-pointer transition-all duration-200 hover:shadow-md ${
              selectedOrderId === order.id 
                ? "border-red-300 bg-red-50 shadow-md" 
                : "border-gray-200 hover:border-red-200 bg-white hover:bg-gray-50"
            }`}
          >
            <div className="flex justify-between items-start">
              <div className="flex-1">
                <div className="flex gap-2 items-center mb-2">
                  <span className="font-bold text-lg text-gray-800">{order.id}</span>
                  <span className={`text-xs px-2 py-1 rounded border ${getStatusColor(order.status)}`}>
                    {order.status}
                  </span>
                  <span className="bg-orange-500/20 text-orange-700 text-xs px-2 py-1 rounded border border-orange-400/30">
                    {order.priority}
                  </span>
                </div>
                <div className="text-sm text-gray-600 mb-1">
                  Khách hàng: <span className="font-medium text-gray-800">{order.customer}</span>
                </div>
                <div className="text-sm text-gray-600">
                  Đến: <span className="font-medium text-gray-800">{order.to}</span>
                </div>
              </div>
              <div className="text-right min-w-[180px]">
                <div className="text-sm text-gray-500 font-medium mb-1">{order.date}</div>
                <div className="text-sm text-gray-600 mb-1">
                  Từ: <span className="font-medium text-gray-800">{order.from}</span>
                </div>
                <div className="text-sm text-gray-600">
                  Tài xế: <span className="font-medium text-gray-800">{order.driver}</span>
                </div>
                {order.status === "Đang vận chuyển" && (
                  <div className="flex items-center justify-end gap-1 mt-1">
                    <div className="w-2 h-2 bg-green-500 rounded-full animate-pulse"></div>
                    <span className="text-xs text-green-600 font-medium">Live Tracking</span>
                  </div>
                )}
              </div>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}