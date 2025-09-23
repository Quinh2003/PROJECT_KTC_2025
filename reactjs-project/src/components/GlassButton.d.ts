import type { ReactNode } from 'react';
interface GlassButtonProps {
    children: ReactNode;
    onClick?: () => void;
    variant?: 'primary' | 'secondary' | 'danger' | 'ocean' | 'green';
    size?: 'sm' | 'md' | 'lg';
    disabled?: boolean;
    className?: string;
}
export default function GlassButton({ children, onClick, variant, size, disabled, className }: GlassButtonProps): import("react/jsx-runtime").JSX.Element;
export {};
