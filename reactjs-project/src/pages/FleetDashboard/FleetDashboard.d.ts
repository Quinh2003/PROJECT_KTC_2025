import type { User } from "../../types/User";
interface FleetDashboardProps {
    user: User;
    onLogout: () => void;
}
export default function FleetDashboard({ user, onLogout, }: FleetDashboardProps): import("react/jsx-runtime").JSX.Element;
export {};
