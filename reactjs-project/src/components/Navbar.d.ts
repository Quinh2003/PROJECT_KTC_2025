import type { User } from "../types/User";
interface NavbarProps {
    user: User;
    onLogout: () => void;
    title: string;
    subtitle: string;
}
export default function Navbar({ user, onLogout, title, subtitle }: NavbarProps): import("react/jsx-runtime").JSX.Element;
export {};
