import type { User } from "../../types/User";

interface DispatcherDashboardProps {
  user: User;
  onLogout: () => void;
}

export default function DispatcherDashboard({ user, onLogout }: DispatcherDashboardProps) {
  return (
    <div style={{ padding: 40 }}>
      <h2>Dispatcher Dashboard</h2>
      <p>Xin chào <b>{user?.name}</b> ({user?.email})</p>
      <p>Role: <b>{user?.role}</b></p>
      <button onClick={onLogout}>Đăng xuất</button>
    </div>
  );
}