import type { User } from "../../types/User";

interface OperationsDashboardProps {
  user: User;
  onLogout: () => void;
}

export default function OperationsDashboard({ user, onLogout }: OperationsDashboardProps) {
  return (
    <div>
      <h2>Operations Manager Dashboard</h2>
      <p>Xin chào {user.name} ({user.email})</p>
      <button onClick={onLogout}>Đăng xuất</button>
    </div>
  );
}