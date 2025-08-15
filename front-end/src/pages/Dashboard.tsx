import { useEffect, useState } from "react";
import AdminDashboard from "./AdminDashboard/AdminDashboard";
import DispatcherDashboard from "./DispatcherDashboard/DispatcherDashboard";
import FleetDashboard from "./FleetDashboard/FleetDashboard";
import DriverDashboard from "./DriverDashboard/DriverDashboard";
import OperationsDashboard from "./OperationsDashboard/OperationsDashboard";
import type { User } from "../types/User";

interface DashboardProps {
  user?: User;
  onLogout: () => void;
}

export default function Dashboard({
  user: userProp,
  onLogout,
}: DashboardProps) {
  // Lấy user từ localStorage nếu chưa truyền qua props
  const [user, setUser] = useState<User | null>(
    userProp || JSON.parse(localStorage.getItem("user") || "null")
  );
  const [protectedData, setProtectedData] = useState<any>(null);
  const [error, setError] = useState<string>("");

  // Ví dụ fetch API protected khi vào dashboard
  useEffect(() => {
    const token = localStorage.getItem("token");
    if (!token) return;
    fetch("http://localhost:8080/api/protected/profile", {
      headers: {
        Authorization: `Bearer ${token}`,
        "Content-Type": "application/json",
      },
    })
      .then((res) => {
        if (!res.ok)
          throw new Error("Không có quyền truy cập hoặc token hết hạn!");
        return res.json();
      })
      .then(setProtectedData)
      .catch((err) => setError(err.message));
  }, []);

  // Phân quyền giao diện theo role
  if (!user) {
    return (
      <div className="min-h-screen flex items-center justify-center bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
        <div className="bg-white/30 backdrop-blur-lg rounded-2xl p-8 border border-white/30 shadow-xl text-center">
          <div className="text-2xl font-bold text-gray-800 mb-2">⚠️ Chưa đăng nhập</div>
          <div className="text-gray-600">Bạn cần đăng nhập để truy cập dashboard!</div>
        </div>
      </div>
    );
  }

  if (user.role === "ADMIN")
    return (
      <div className="min-h-screen bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
        <AdminDashboard user={user} onLogout={onLogout} />
        {/* <ProtectedInfo data={protectedData} error={error} /> */}
      </div>
    );
  if (user.role === "DISPATCHER")
    return (
      <div className="min-h-screen">
        <DispatcherDashboard user={user} onLogout={onLogout} />
        {/* <ProtectedInfo data={protectedData} error={error} /> */}
      </div>
    );
  if (user.role === "FLEET")
    return (
      <div className="min-h-screen bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
        <FleetDashboard user={user} onLogout={onLogout} />
        {/* <ProtectedInfo data={protectedData} error={error} /> */}
      </div>
    );
  if (user.role === "DRIVER")
    return (
      <div className="min-h-screen bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
        <DriverDashboard user={user} onLogout={onLogout} />
        {/* <ProtectedInfo data={protectedData} error={error} /> */}
      </div>
    );
  if (user.role === "OPERATIONS")
    return (
      <div className="min-h-screen bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
        <OperationsDashboard user={user} onLogout={onLogout} />
        {/* <ProtectedInfo data={protectedData} error={error} /> */}
      </div>
    );

  return (
    <div className="min-h-screen flex items-center justify-center bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
      <div className="bg-white/30 backdrop-blur-lg rounded-2xl p-8 border border-white/30 shadow-xl text-center">
        <div className="text-2xl font-bold text-gray-800 mb-2">❌ Role không hợp lệ</div>
        <div className="text-gray-600">Không xác định được quyền truy cập của bạn!</div>
      </div>
    </div>
  );
}

// Component phụ để hiển thị dữ liệu protected (có thể bỏ nếu không cần)
// function ProtectedInfo({ data, error }: { data: any; error: string }) {
//   if (error)
//     return (
//       <div className="fixed bottom-5 right-5 z-40 bg-red-200/60 backdrop-blur-sm border border-red-300/50 text-red-800 rounded-xl px-4 py-3 shadow-lg max-w-md">
//         <div className="font-semibold mb-1">⚠️ Lỗi API:</div>
//         <div className="text-sm">{error}</div>
//       </div>
//     );
//   if (!data) return null;
//   return (
//     <div className="fixed bottom-5 left-5 z-40 bg-blue-200/60 backdrop-blur-sm border border-blue-300/50 text-blue-900 rounded-xl px-4 py-3 shadow-lg max-w-md">
//       <div className="font-semibold mb-2">🔒 Protected API Data:</div>
//       <pre className="text-xs overflow-auto max-h-32 bg-white/30 rounded p-2">
//         {JSON.stringify(data, null, 2)}
//       </pre>
//     </div>
//   );
// }