import type { User } from "../../types/User";
interface DispatcherDashboardProps {
    user: User;
    onLogout: () => void;
}
export default function DispatcherDashboard({ user, onLogout, }: DispatcherDashboardProps): import("react/jsx-runtime").JSX.Element;
export {};
