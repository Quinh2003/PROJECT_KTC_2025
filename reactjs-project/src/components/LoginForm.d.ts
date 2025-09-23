import type { User } from "../types/User";
export default function LoginForm({ onLogin }: {
    onLogin: (user: User) => void;
}): import("react/jsx-runtime").JSX.Element;
