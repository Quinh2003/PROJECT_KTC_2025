import { jsx as _jsx } from "react/jsx-runtime";
export default function GlassButton({ children, onClick, variant = 'primary', size = 'md', disabled = false, className = '' }) {
    const baseClasses = 'backdrop-blur-lg border rounded-xl font-medium transition-all duration-300 active:scale-95 whitespace-nowrap';
    const variantClasses = {
        primary: 'bg-blue-500/30 border-blue-400/50 text-blue-800 hover:bg-blue-500/40 hover:border-blue-400/60',
        secondary: 'bg-white/30 border-white/40 text-gray-800 hover:bg-white/40 hover:border-white/50',
        danger: 'bg-red-500/30 border-red-400/50 text-red-800 hover:bg-red-500/40 hover:border-red-400/60',
        ocean: 'bg-cyan-500/30 border-cyan-400/50 text-cyan-800 hover:bg-cyan-500/40 hover:border-cyan-400/60',
        green: 'bg-green-500/30 border-green-400/50 text-green-800 hover:bg-green-500/40 hover:border-green-400/60'
    };
    const sizeClasses = {
        sm: 'px-3 py-2 text-sm',
        md: 'px-4 py-2 text-sm',
        lg: 'px-6 py-3 text-base'
    };
    const disabledClasses = disabled ? 'opacity-50 cursor-not-allowed' : 'cursor-pointer';
    return (_jsx("button", { onClick: disabled ? undefined : onClick, className: `
        ${baseClasses}
        ${variantClasses[variant]}
        ${sizeClasses[size]}
        ${disabledClasses}
        ${className}
      `, disabled: disabled, children: children }));
}
