import { jsx as _jsx } from "react/jsx-runtime";
export default function GlassCard({ children, className = '', padding = 'md', hover = false }) {
    const paddingClasses = {
        sm: 'p-4',
        md: 'p-6',
        lg: 'p-8'
    };
    return (_jsx("div", { className: `
        backdrop-blur-lg bg-white/40 border border-white/30 rounded-2xl shadow-xl
        ${paddingClasses[padding]}
        ${hover ? 'transition-all duration-300 hover:bg-white/50 hover:shadow-2xl hover:scale-105' : ''}
        ${className}
      `, children: children }));
}
