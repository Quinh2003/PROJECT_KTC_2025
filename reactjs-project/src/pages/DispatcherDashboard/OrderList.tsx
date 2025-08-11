export default function OrderList() {
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
  return (
    <div className="bg-white/30 backdrop-blur-lg rounded-2xl p-6 border border-white/30 shadow-lg">
      <div className="text-2xl font-bold mb-2 text-gray-800">Total Orders</div>
      <div className="text-gray-600 mb-4">Track the status of all orders in the system</div>
      <div className="flex flex-col gap-4">
        {orders.map((order) => (
          <div key={order.id} className="rounded-xl bg-white/40 backdrop-blur-sm border border-white/40 p-4 flex justify-between items-start hover:bg-white/50 transition-all duration-300 shadow-md hover:shadow-lg">
            <div>
              <div className="flex gap-2 items-center mb-1">
                <span className="font-bold text-lg text-gray-800">{order.id}</span>
                <span className="bg-blue-200/60 backdrop-blur-sm text-blue-700 text-xs px-2 py-1 rounded border border-blue-300/50">{order.status}</span>
                <span className={`bg-orange-200/60 backdrop-blur-sm text-orange-700 text-xs px-2 py-1 rounded border border-orange-300/50`}>{order.priority}</span>
              </div>
              <div className="text-sm text-gray-700">Khách hàng: <span className="font-medium">{order.customer}</span></div>
              <div className="text-sm text-gray-700">Đến: <span className="font-medium">{order.to}</span></div>
            </div>
            <div className="text-right min-w-[180px]">
              <div className="text-sm text-gray-600 font-medium">{order.date}</div>
              <div className="text-sm text-gray-700">Từ: <span className="font-medium">{order.from}</span></div>
              <div className="text-sm text-gray-700">Tài xế: <span className="font-medium">{order.driver}</span></div>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}