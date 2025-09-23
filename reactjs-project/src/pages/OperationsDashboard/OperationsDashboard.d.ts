import type { User } from "../../types/User";
interface OperationsDashboardProps {
    user: User;
    onLogout: () => void;
}
export default function OperationsDashboard({ user, onLogout }: OperationsDashboardProps): import("react/jsx-runtime").JSX.Element;
export {};
