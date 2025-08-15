import StatCard from '../../components/StatCard';

interface StatCardData {
  metric: string;
  current: number;
  target: number;
  trend: number;
}

interface PerformanceStatCardsProps {
  performanceData: StatCardData[];
}

export default function PerformanceStatCards({ performanceData }: PerformanceStatCardsProps) {
  return (
    <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
      {performanceData.map((data, index) => (
        <StatCard
          key={index}
          title={data.metric}
          value={
            data.metric.includes('Chi phí')
              ? `${Math.round(data.current).toLocaleString()}đ`
              : data.metric.includes('thời gian')
              ? `${Math.round(data.current)}min`
              : data.metric.includes('lòng')
              ? `${data.current.toFixed(1)}/5`
              : `${data.current.toFixed(1)}%`
          }
          subtitle={
            data.metric.includes('Chi phí')
              ? `Mục tiêu: ${Math.round(data.target).toLocaleString()}đ`
              : data.metric.includes('thời gian')
              ? `Mục tiêu: ${Math.round(data.target)}min`
              : data.metric.includes('lòng')
              ? `Mục tiêu: ${data.target.toFixed(1)}/5`
              : `Mục tiêu: ${data.target.toFixed(1)}%`
          }
          trend={{
            value: Math.abs(data.trend),
            isPositive: data.trend > 0,
          }}
          icon={index === 0 ? '📦' : index === 1 ? '⏱️' : index === 2 ? '💰' : '⭐'}
        />
      ))}
    </div>
  );
}
