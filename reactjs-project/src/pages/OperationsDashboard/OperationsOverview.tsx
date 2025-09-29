import { forwardRef, useImperativeHandle } from 'react';
import { useTranslation } from 'react-i18next';
import MonthlyRevenueChart from '../../components/MonthlyRevenueChart';
import { TfiPackage } from 'react-icons/tfi';
import { Truck } from 'lucide-react';
import { FiCheckSquare, FiDollarSign } from 'react-icons/fi';

// Interface cho metrics data
export interface MetricsData {
  todayOrders: {
    count: number;
    changePercent: number;
    trend: 'increase' | 'decrease' | 'stable';
  };
  activeVehicles: {
    active: number;
    total: number;
    percentage: number;
    ratio: string;
  };
  revenueData: {
    amount: string;
    changePercent: number;
    trend: 'increase' | 'decrease' | 'stable';
  };
  performanceData: {
    count: number;
    changePercent: number;
    trend: 'increase' | 'decrease' | 'stable';
  };
  lastUpdated: Date | null;
}

// Interface cho props
interface OperationsOverviewProps {
  metricsData: MetricsData;
  isLoading: boolean;
  onRefresh: () => Promise<void>;
}

// Interface cho ref methods (similar to AdminDashboard)
export interface OperationsOverviewRef {
  updateMetrics: () => void;
}

const OperationsOverview = forwardRef<OperationsOverviewRef, OperationsOverviewProps>(
  ({ metricsData, isLoading, onRefresh }, ref) => {
  const { t } = useTranslation();

  // Expose methods cho parent component (similar to AdminDashboard)
  useImperativeHandle(ref, () => ({
    updateMetrics: onRefresh
  }), [onRefresh]);

  return (
    <div className="space-y-4 md:space-y-6">
      {/* Overview Stats - Mobile optimized */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-3 md:gap-6">
        <div className="bg-white/30 backdrop-blur-lg border border-white/30 rounded-2xl p-3 md:p-6 shadow-xl hover:shadow-2xl transition-all duration-300 hover:scale-105">
          <div className="flex items-center justify-between mb-2 md:mb-4">
              <h3 className="text-gray-700 font-medium text-xs md:text-base">{t('dashboard.operations.overview.todaysOrders', "Today's Orders")}</h3>
            <span className="text-xl md:text-2xl"><TfiPackage size={window.innerWidth < 768 ? 20 : 24} color="#3b82f6" /></span>
          </div>
          {isLoading ? (
            <div className="animate-pulse">
              <div className="h-6 md:h-8 bg-gray-200 rounded mb-1 md:mb-2"></div>
              <div className="h-3 md:h-4 bg-gray-200 rounded w-3/4"></div>
            </div>
          ) : (
            <>
              <p className="text-2xl md:text-3xl font-bold text-gray-800">{metricsData.todayOrders.count}</p>
                <p className={`text-xs md:text-sm mt-1 md:mt-2 ${
                  metricsData.todayOrders.trend === 'increase' ? 'text-green-600' : 
                  metricsData.todayOrders.trend === 'decrease' ? 'text-red-600' : 'text-gray-600'
                }`}>
                  {metricsData.todayOrders.changePercent > 0 ? '+' : ''}{metricsData.todayOrders.changePercent}% {t('dashboard.operations.overview.comparedToYesterday', 'compared to yesterday')}
              </p>
            </>
          )}
        </div>
        
        <div className="bg-white/30 backdrop-blur-lg border border-white/30 rounded-2xl p-3 md:p-6 shadow-xl hover:shadow-2xl transition-all duration-300 hover:scale-105">
          <div className="flex items-center justify-between mb-2 md:mb-4">
              <h3 className="text-gray-700 font-medium text-xs md:text-base">{t('dashboard.operations.overview.activeVehicles', 'Active Vehicles')}</h3>
            <span className="text-xl md:text-2xl"><Truck size={window.innerWidth < 768 ? 20 : 24} color="#f59e0b" /></span>
          </div>
          {isLoading ? (
            <div className="animate-pulse">
              <div className="h-6 md:h-8 bg-gray-200 rounded mb-1 md:mb-2"></div>
              <div className="h-3 md:h-4 bg-gray-200 rounded w-3/4"></div>
            </div>
          ) : (
            <>
              <p className="text-2xl md:text-3xl font-bold text-gray-800">{metricsData.activeVehicles.ratio}</p>
                <p className="text-gray-600 text-xs md:text-sm mt-1 md:mt-2">{metricsData.activeVehicles.percentage}% {t('dashboard.operations.overview.ofTotalVehicles', 'of total vehicles')}</p>
            </>
          )}
        </div>
        
        <div className="bg-white/30 backdrop-blur-lg border border-white/30 rounded-2xl p-3 md:p-6 shadow-xl hover:shadow-2xl transition-all duration-300 hover:scale-105">
          <div className="flex items-center justify-between mb-2 md:mb-4">
              <h3 className="text-gray-700 font-medium text-xs md:text-base">{t('dashboard.operations.overview.todaysRevenue', "Today's Revenue")}</h3>
            <span className="text-xl md:text-2xl"><FiDollarSign size={window.innerWidth < 768 ? 20 : 24} color="#10b981" /></span>
          </div>
          {isLoading ? (
            <div className="animate-pulse">
              <div className="h-6 md:h-8 bg-gray-200 rounded w-20 md:w-24 mb-1 md:mb-2"></div>
              <div className="h-3 md:h-4 bg-gray-200 rounded w-24 md:w-32"></div>
            </div>
          ) : (
            <>
              <p className="text-2xl md:text-3xl font-bold text-gray-800">{metricsData.revenueData.amount}</p>
                <p className={`text-xs md:text-sm mt-1 md:mt-2 ${
                  metricsData.revenueData.trend === 'increase' ? 'text-green-600' : 
                  metricsData.revenueData.trend === 'decrease' ? 'text-red-600' : 'text-gray-600'
                }`}>
                  {metricsData.revenueData.trend === 'increase' ? '+' : 
                   metricsData.revenueData.trend === 'decrease' ? '-' : ''}
                  {metricsData.revenueData.changePercent.toFixed(1)}% {t('dashboard.operations.overview.comparedToYesterday', 'compared to yesterday')}
              </p>
            </>
          )}
        </div>
        
        <div className="bg-white/30 backdrop-blur-lg border border-white/30 rounded-2xl p-3 md:p-6 shadow-xl hover:shadow-2xl transition-all duration-300 hover:scale-105">
          <div className="flex items-center justify-between mb-2 md:mb-4">
              <h3 className="text-gray-700 font-medium text-xs md:text-base">{t('dashboard.operations.overview.completedOrders', 'Completed Orders')}</h3>
            <span className="text-xl md:text-2xl"><FiCheckSquare size={window.innerWidth < 768 ? 20 : 24} color="#8b5cf6"/></span>
          </div>
          {isLoading ? (
            <div className="animate-pulse">
              <div className="h-6 md:h-8 bg-gray-200 rounded w-20 md:w-24 mb-1 md:mb-2"></div>
              <div className="h-3 md:h-4 bg-gray-200 rounded w-24 md:w-32"></div>
            </div>
          ) : (
            <>
              <p className="text-2xl md:text-3xl font-bold text-gray-800">{metricsData.performanceData.count}</p>
                <p className={`text-xs md:text-sm mt-1 md:mt-2 ${
                  metricsData.performanceData.trend === 'increase' ? 'text-green-600' : 
                  metricsData.performanceData.trend === 'decrease' ? 'text-red-600' : 'text-gray-600'
                }`}>
                  {metricsData.performanceData.trend === 'increase' ? '+' : 
                   metricsData.performanceData.trend === 'decrease' ? '-' : ''}
                  {metricsData.performanceData.changePercent.toFixed(1)}% {t('dashboard.operations.overview.comparedToYesterday', 'compared to yesterday')}
              </p>
            </>
          )}
        </div>
      </div>
      
      {/* Monthly Revenue Chart - full width */}
      <div className="w-full">
        <MonthlyRevenueChart onRefreshAll={onRefresh} />
      </div>

      {/* Fleet Status Overview */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">

      </div>
    </div>
  );
});

OperationsOverview.displayName = 'OperationsOverview';

export default OperationsOverview;
