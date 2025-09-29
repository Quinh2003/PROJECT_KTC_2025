import StatCard from '../../components/StatCard';
import { TfiPackage } from "react-icons/tfi";
import { FiClock, FiDollarSign } from "react-icons/fi";
import { MdOutlineDirections } from "react-icons/md";

interface StatCardData {
  metric: string;
  current: number;
  target: number;
  trend: number;
}

interface PerformanceStatCardsProps {
  performanceData: StatCardData[];
}

// Function to format minutes to "xh ym" or "xm" (shortened format)
function formatMinutesToTime(minutes: number): string {
  if (minutes <= 0) return "0m";
  
  const hours = Math.floor(minutes / 60);
  const remainingMinutes = Math.round(minutes % 60);
  
  if (hours > 0) {
    if (remainingMinutes > 0) {
      return `${hours}h ${remainingMinutes}m`;
    } else {
      return `${hours}h`;
    }
  } else {
    return `${remainingMinutes}m`;
  }
}

export default function PerformanceStatCards({ performanceData }: PerformanceStatCardsProps) {
  // Debug log to check metric names and values
  console.log('PerformanceStatCards data:', performanceData);
  
  return (
    <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
      {performanceData.map((data, index) => {
        // Debug individual metric
        console.log(`Metric: ${data.metric}, Current: ${data.current}, Target: ${data.target}`);
        console.log(`Contains 'thời gian': ${data.metric.includes('thời gian')}`);
        
        return (
        <StatCard
          key={index}
          title={
            data.metric.includes('Transportation Cost')
              ? 'Average Transportation Cost/km'
              : data.metric
          }
          value={
            data.metric.includes('Transportation Cost')
              ? `${Math.round(data.current).toLocaleString()}đ`
              : (data.metric.includes('Delivery Time') || data.metric.includes('Average Delivery Time'))
              ? formatMinutesToTime(data.current)
              : data.metric.includes('km Transported')
              ? `${Math.round(data.current).toLocaleString()} km`
              : `${data.current.toFixed(1)}%`
          }
          subtitle={
            data.metric.includes('Transportation Cost')
              ? ''
              : (data.metric.includes('Delivery Time') || data.metric.includes('Average Delivery Time'))
              ? ''
              : data.metric.includes('km Transported')
              ? ''
              : `Target: ${data.target.toFixed(1)}%`
          }
          trend={
            (data.metric.includes('Delivery Time') || data.metric.includes('Average Delivery Time') || data.metric.includes('Transportation Cost') || data.metric.includes('km Transported'))
              ? undefined // Hide trend for time, cost and total km
              : {
                  value: Math.abs(data.trend),
                  isPositive: data.trend > 0,
                }
          }
          icon={
            index === 0 ? <TfiPackage size={24} color="#3b82f6" /> :    // Xanh dương cho đơn hàng
            index === 1 ? <FiClock size={24} color="#f59e0b" /> :       // Vàng cam cho thời gian
            index === 2 ? <FiDollarSign size={24} color="#10b981" /> :  // Xanh lá cho tiền
            <MdOutlineDirections size={24} color="#8b5cf6" />           // Tím cho tuyến đường/km
          }
        />
        );
      })}
    </div>
  );
}
