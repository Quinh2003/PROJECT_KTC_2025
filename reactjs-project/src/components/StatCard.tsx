interface StatCardProps {
  title: string;
  value: string | number;
  subtitle?: string;
  trend?: {
    value: number;
    isPositive: boolean;
  };
  icon?: string;
}

export default function StatCard({ title, value, subtitle, trend, icon }: StatCardProps) {
  return (
    <div className="bg-white/30 backdrop-blur-xl border border-white/10 shadow-lg rounded-2xl px-6 py-5 flex flex-col justify-between h-full transition-all duration-200 hover:scale-[1.03] hover:shadow-xl hover:bg-black/40">
      <div className="flex items-center justify-between mb-4">
        <h3 className="text-gray-700 text-sm font-medium">{title}</h3>
        {icon && (
          <div className="w-8 h-8 bg-white/30 rounded-lg flex items-center justify-center">
            <span className="text-gray-700 text-lg">{icon}</span>
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
