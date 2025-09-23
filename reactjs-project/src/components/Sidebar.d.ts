export type DispatcherTab = "orders" | "resources" | "assignment";
export type OperationsTab = "overview" | "performance" | "monitoring" | "staff";
export type AdminTab = "users" | "roles" | "settings" | "logs";
export type FleetTab = "vehicles" | "maintenance" | "schedule";
export type TabType = DispatcherTab | OperationsTab | AdminTab | FleetTab;
export type UserRole = "dispatcher" | "operations" | "admin" | "fleet";
interface SidebarProps<T extends TabType> {
    activeTab: T;
    onTabChange: (tab: T) => void;
    role: UserRole;
}
export default function Sidebar<T extends TabType>({ activeTab, onTabChange, role, }: SidebarProps<T>): import("react/jsx-runtime").JSX.Element;
export {};
