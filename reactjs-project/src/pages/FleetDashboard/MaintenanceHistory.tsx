interface MaintenanceHistoryItem {
  licensePlate: string;
  status: "Hoàn thành" | "Đang thực hiện";
  type: string;
  description: string;
  cost: string;
  maintenanceTime: string;
  nextSchedule: string;
}

const maintenanceHistory: MaintenanceHistoryItem[] = [
  {
    licensePlate: "51A-12345",
    status: "Hoàn thành",
    type: "Bảo dưỡng định kỳ",
    description: "Thay dầu máy, kiểm tra phanh",
    cost: "1.500.000 VNĐ",
    maintenanceTime: "2024-12-15",
    nextSchedule: "2025-06-15",
  },
  {
    licensePlate: "51B-67890",
    status: "Đang thực hiện",
    type: "Sửa chữa",
    description: "Thay lốp xe, sửa hệ thống điện",
    cost: "3.000.000 VNĐ",
    maintenanceTime: "2025-01-10",
    nextSchedule: "2025-07-10",
  },
];

export default function MaintenanceHistory() {
  return (
    <div className="bg-white/30 backdrop-blur-lg rounded-2xl p-6 border border-white/30 shadow-xl">
      <div className="text-xl font-bold mb-2">Lịch sử bảo trì</div>
      <div className="flex flex-col gap-4">
        {maintenanceHistory.map((item, idx) => (
          <div
            key={idx}
            className="bg-white border rounded-xl p-4 flex flex-col md:flex-row md:items-center md:justify-between gap-4"
          >
            <div className="flex-1 min-w-0">
              <div className="text-lg font-bold flex items-center gap-2">
                {item.licensePlate}
                <span
                  className={`px-2 py-1 rounded text-xs font-semibold ${item.status === "Hoàn thành" ? "bg-green-100 text-green-700" : "bg-yellow-100 text-yellow-700"}`}
                >
                  {item.status}
                </span>
              </div>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-2 mt-2 text-sm">
                <div>
                  <span className="font-semibold">Loại:</span> {item.type}
                  <div><span className="font-semibold">Mô tả:</span> {item.description}</div>
                </div>
                <div>
                  <span className="font-semibold">Chi phí:</span>
                  <div className="font-bold">{item.cost}</div>
                </div>
                <div className="flex flex-col gap-1">
                  <div>
                    <span className="font-semibold">Thời gian bảo trì:</span> {item.maintenanceTime}
                  </div>
                  <div>
                    <span className="font-semibold">Lịch bảo trì sắp tới:</span> {item.nextSchedule}
                  </div>
                </div>
              </div>
            </div>
          </div>
        ))}
      </div>
    </div>
  );
}
