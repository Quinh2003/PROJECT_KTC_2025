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
    fetch("http://localhost:8080/api/protected-endpoint", {
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
    return <div style={{ padding: 40 }}>Bạn chưa đăng nhập!</div>;
  }

  const LogoutButton = (
    <div className="flex gap-4">
      <button
        onClick={onLogout}
        className="absolute top-4 right-4 bg-red-100 text-red-700 px-4 py-2 rounded shadow hover:bg-red-200"
        style={{ position: "fixed", top: 20, right: 20, zIndex: 1000 }}
      >
        Đăng xuất
      </button>
    </div>
  );

  if (user.role === "ADMIN")
    return (
      <>
        {LogoutButton}
        <AdminDashboard user={user} onLogout={onLogout} />
        <ProtectedInfo data={protectedData} error={error} />
      </>
    );
  if (user.role === "DISPATCHER")
    return (
      <>
        {LogoutButton}
        <DispatcherDashboard user={user} onLogout={onLogout} />
        <ProtectedInfo data={protectedData} error={error} />
      </>
    );
  if (user.role === "FLEET_MANAGER")
    return (
      <>
        {LogoutButton}
        <FleetDashboard user={user} onLogout={onLogout} />
        <ProtectedInfo data={protectedData} error={error} />
      </>
    );
  if (user.role === "DRIVER")
    return (
      <>
        {LogoutButton}
        <DriverDashboard user={user} onLogout={onLogout} />
        <ProtectedInfo data={protectedData} error={error} />
      </>
    );
  if (user.role === "OPERATIONS_MANAGER")
    return (
      <>
        {LogoutButton}
        <OperationsDashboard user={user} onLogout={onLogout} />
        <ProtectedInfo data={protectedData} error={error} />
      </>
    );

  return <div style={{ padding: 40 }}>Không xác định role!</div>;
}

// Component phụ để hiển thị dữ liệu protected (có thể bỏ nếu không cần)
function ProtectedInfo({ data, error }: { data: any; error: string }) {
  if (error)
    return (
      <div className="bg-red-100 border border-red-300 text-red-700 rounded px-4 py-2 m-4">
        {error}
      </div>
    );
  if (!data) return null;
  return (
    <div className="bg-blue-50 border border-blue-200 text-blue-900 rounded px-4 py-2 m-4">
      <div className="font-bold mb-1">Protected API Data:</div>
      <pre className="text-xs">{JSON.stringify(data, null, 2)}</pre>
    </div>
  );
}
