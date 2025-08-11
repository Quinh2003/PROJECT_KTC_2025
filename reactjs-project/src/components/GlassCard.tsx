import type { ReactNode } from 'react';

interface GlassCardProps {
  children: ReactNode;
  className?: string;
  padding?: 'sm' | 'md' | 'lg';
  hover?: boolean;
}

export default function GlassCard({ 
  children, 
  className = '', 
  padding = 'md',
  hover = false 
}: GlassCardProps) {
  const paddingClasses = {
    sm: 'p-4',
    md: 'p-6',
    lg: 'p-8'
  };

  return (
    <div 
      className={`
        backdrop-blur-lg bg-white/40 border border-white/30 rounded-2xl shadow-xl
        ${paddingClasses[padding]}
        ${hover ? 'transition-all duration-300 hover:bg-white/50 hover:shadow-2xl hover:scale-105' : ''}
        ${className}
      `}
    >
      {children}
    </div>
  );
}
