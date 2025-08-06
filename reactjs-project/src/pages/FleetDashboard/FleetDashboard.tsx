import type { User } from "../../types/User";

interface FleetDashboardProps {
  user: User;
  onLogout: () => void;
}

export default function FleetDashboard({ user, onLogout }: FleetDashboardProps) {
  return (
    <div>
      <h2>Fleet Manager Dashboard</h2>
      <p>Xin chào {user.name} ({user.email})</p>
      <button onClick={onLogout}>Đăng xuất</button>
    </div>
  );
}