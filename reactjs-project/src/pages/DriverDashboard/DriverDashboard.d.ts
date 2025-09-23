import type { User } from "../../types/User";
interface DriverDashboardProps {
    user: User;
    onLogout: () => void;
}
export default function DriverDashboard({ user, onLogout }: DriverDashboardProps): import("react/jsx-runtime").JSX.Element;
export {};
