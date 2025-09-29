import { type ReactNode } from 'react';

interface StatCardProps {
  title: string;
  value: string | number;
  subtitle?: string;
  trend?: {
    value: number;
    isPositive: boolean;
  };
  icon?: string | ReactNode;
}

export default function StatCard({ title, value, subtitle, trend, icon }: StatCardProps) {
  return (
    <div className="backdrop-blur-lg bg-white/40 border border-white/30 rounded-xl p-6 transition-all duration-300 hover:bg-white/50 hover:shadow-xl">
      <div className="flex items-center justify-between mb-4">
        <h3 className="text-gray-700 text-sm font-medium">{title}</h3>
        {icon && (
          <div className="w-8 h-8 bg-white/30 rounded-lg flex items-center justify-center">
            {typeof icon === 'string' ? (
              <span className="text-gray-700 text-lg">{icon}</span>
            ) : (
              <div className="text-gray-700">{icon}</div>
            )}
          </div>
        )}
      </div>
      
      <div className="space-y-2">
        <p className="text-3xl font-bold text-gray-800">{value}</p>
        
        <div className="flex items-center gap-2">
          {subtitle && (
            <span className="text-gray-600 text-sm">{subtitle}</span>
          )}
          
          {trend && (
            <span className={`text-sm font-medium ${
              trend.isPositive ? 'text-green-600' : 'text-red-600'
            }`}>
              {trend.isPositive ? '+' : ''}{trend.value}%
            </span>
          )}
        </div>
      </div>
    </div>
  );
}
