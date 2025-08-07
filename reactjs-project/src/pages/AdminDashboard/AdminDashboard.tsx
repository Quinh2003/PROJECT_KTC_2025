import type { User } from "../../types/User";

interface AdminDashboardProps {
  user: User;
  onLogout: () => void;
}



export default function AdminDashboard({ user, onLogout }: AdminDashboardProps) {

  return (
    <div>
      <h2>Admin Dashboard</h2>
      <p>Xin chào {user.name} ({user.email})</p>
      <button onClick={onLogout}>Đăng xuất</button>
    </div>
  );
} 