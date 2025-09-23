import type { User } from "../../types/User";
interface AdminDashboardProps {
    user: User;
    onLogout: () => void;
}
export default function AdminDashboard({ user, onLogout }: AdminDashboardProps): import("react/jsx-runtime").JSX.Element;
export {};
