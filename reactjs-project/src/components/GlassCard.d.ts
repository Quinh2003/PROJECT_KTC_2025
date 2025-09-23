import type { ReactNode } from 'react';
interface GlassCardProps {
    children: ReactNode;
    className?: string;
    padding?: 'sm' | 'md' | 'lg';
    hover?: boolean;
}
export default function GlassCard({ children, className, padding, hover }: GlassCardProps): import("react/jsx-runtime").JSX.Element;
export {};
