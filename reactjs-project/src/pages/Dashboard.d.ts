import type { User } from "../types/User";
interface DashboardProps {
    user?: User;
    onLogout: () => void;
}
export default function Dashboard({ user: userProp, onLogout, }: DashboardProps): import("react/jsx-runtime").JSX.Element;
export {};
