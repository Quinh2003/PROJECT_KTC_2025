interface User {
    id?: number;
    name: string;
    email: string;
    role: string;
    status: string;
    lastLogin: string;
    phone?: string;
    password?: string;
}
interface UserFormProps {
    onAdd: (user: User & {
        roleIcon: React.ReactNode;
    }) => void;
    onClose: () => void;
    user?: (User & {
        roleIcon: React.ReactNode;
    }) | null;
}
export default function UserForm({ onAdd, onClose, user }: UserFormProps): import("react/jsx-runtime").JSX.Element;
export {};
