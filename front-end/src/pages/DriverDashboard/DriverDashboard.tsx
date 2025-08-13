import type { User } from "../../types/User";

interface DriverDashboardProps {
  user: User;
  onLogout: () => void;
}

export default function DriverDashboard({ user, onLogout }: DriverDashboardProps) {
  return (
    <div>
      <h2>Driver Dashboard</h2>
      <p>Xin chào {user.name} ({user.email})</p>
      <button onClick={onLogout}>Đăng xuất</button>
    </div>
  );
}