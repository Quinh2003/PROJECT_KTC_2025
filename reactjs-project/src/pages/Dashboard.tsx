import AdminDashboard from "./AdminDashboard/AdminDashboard";
import DispatcherDashboard from "./DispatcherDashboard/DispatcherDashboard";
import FleetDashboard from "./FleetDashboard/index";
import DriverDashboard from "./DriverDashboard/DriverDashboard";
import OperationsDashboard from "./OperationsDashboard/OperationsDashboard";
import type { User } from "../types/User";

interface DashboardProps {
  user: User;
  onLogout: () => void;
}

export default function Dashboard({ user, onLogout }: DashboardProps) {
  switch (user.role) {
    case "ADMIN":
      return <AdminDashboard user={user} onLogout={onLogout} />;
    case "DISPATCHER":
      return <DispatcherDashboard user={user} onLogout={onLogout} />;
    case "FLEET_MANAGER":
      return <FleetDashboard user={user} onLogout={onLogout} />;
    case "DRIVER":
      return <DriverDashboard user={user} onLogout={onLogout} />;
    case "OPERATIONS_MANAGER":
      return <OperationsDashboard user={user} onLogout={onLogout} />;
    default:
      return <div style={{ padding: 40 }}>Không xác định role!</div>;
  }
}