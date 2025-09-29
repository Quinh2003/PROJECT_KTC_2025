import { useState, useRef, useEffect, useCallback } from 'react';
import { useTranslation } from 'react-i18next';
import type { User } from "../../types/User";
import Sidebar, { type OperationsTab } from '../../components/Sidebar';
import Navbar from '../../components/Navbar';
import ResourceMonitoring from './ResourceMonitoring';
import PerformanceAnalytics from './PerformanceAnalytics';
import StaffManagement from './StaffManagement';
import OperationsOverview, { type OperationsOverviewRef, type MetricsData } from './OperationsOverview';
import OperationsMetricsService from '../../services/operationsMetricsService';
import { FiHome, FiBarChart2, FiActivity, FiUsers } from "react-icons/fi";

interface OperationsDashboardProps {
  user: User;
  onLogout: () => void;
}

export default function OperationsDashboard({ user, onLogout }: OperationsDashboardProps) {
  const { t } = useTranslation();
  const [tab, setTab] = useState<OperationsTab>("overview");
  const [previousTab, setPreviousTab] = useState<OperationsTab>("overview");
  
  // Cache metrics data ở parent level (similar to AdminDashboard)
  const [metricsData, setMetricsData] = useState<MetricsData>({
    todayOrders: {
      count: 127,
      changePercent: 8.2,
      trend: 'increase'
    },
    activeVehicles: {
      active: 18,
      total: 24,
      percentage: 75,
      ratio: '18/24'
    },
    revenueData: {
      amount: '0 VND',
      changePercent: 0,
      trend: 'stable'
    },
    performanceData: {
      count: 85,
      changePercent: 2.3,
      trend: 'increase'
    },
    lastUpdated: null
  });
  const [isLoadingMetrics, setIsLoadingMetrics] = useState(true);
  
  // Ref để gọi update metrics từ OperationsOverview (similar to AdminDashboard) 
  const operationsOverviewRef = useRef<OperationsOverviewRef | null>(null);

  // Function để update metrics (similar to AdminDashboard's handleUserCountUpdate)
  const handleMetricsUpdate = useCallback(async () => {
    try {
      setIsLoadingMetrics(true);
      console.log("Updating metrics data...");
      const [todayOrdersResult, activeVehiclesResult, revenueResult, completedOrdersResult] = await Promise.all([
        OperationsMetricsService.getTodayOrdersCount(true), // Force refresh
        OperationsMetricsService.getActiveVehiclesRatio(),
        OperationsMetricsService.getTodayRevenue(),
        OperationsMetricsService.getCompletedOrders()
      ]);
      
      const newMetricsData: MetricsData = {
        todayOrders: todayOrdersResult,
        activeVehicles: activeVehiclesResult,
        revenueData: revenueResult,
        performanceData: completedOrdersResult,
        lastUpdated: new Date()
      };
      
      setMetricsData(newMetricsData);
      console.log("Metrics updated:", todayOrdersResult.count, "orders,", activeVehiclesResult.ratio, "vehicles,", revenueResult.amount, "revenue,", completedOrdersResult.count, "completed orders");
    } catch (error) {
      console.error('Error updating metrics:', error);
    } finally {
      setIsLoadingMetrics(false);
    }
  }, []);

  // SSE removed - using manual refresh only

  // Auto-refresh khi chuyển về tab overview
  useEffect(() => {
    if (tab === "overview" && previousTab !== "overview") {
      console.log("Switched to overview tab, refreshing data...");
      handleMetricsUpdate();
    }
    setPreviousTab(tab);
  }, [tab, previousTab, handleMetricsUpdate]);

  // Initial fetch khi dashboard mount (similar to AdminDashboard)
  useEffect(() => {
    const fetchInitialData = async () => {
      try {
        setIsLoadingMetrics(true);
        await handleMetricsUpdate();
      } catch (error) {
        console.error('Failed to fetch initial metrics:', error);
      } finally {
        setIsLoadingMetrics(false);
      }
    };

    // Register callback để auto-update khi có thay đổi (similar to AdminDashboard)
    OperationsMetricsService.registerUpdateCallback(handleMetricsUpdate);

    // Disable frequent polling to reduce load - chỉ dùng manual refresh
    // OperationsMetricsService.startPolling(10000); // Commented out to reduce API calls

    // Initial fetch only - no auto-refresh (theo AdminDashboard pattern)
    fetchInitialData();

    // Cleanup callback và polling khi unmount
    return () => {
      OperationsMetricsService.unregisterUpdateCallback(handleMetricsUpdate);
      OperationsMetricsService.stopPolling();
    };
  }, [handleMetricsUpdate]);

  return (
    <div className="min-h-screen flex bg-gradient-to-br from-purple-100 via-blue-50 to-indigo-100">
      {/* Sidebar - Hidden on mobile, visible on md+ */}
      <div className="hidden md:block">
        <Sidebar<OperationsTab>
          activeTab={tab}
          onTabChange={tab => setTab(tab as OperationsTab)}
          role="operations"
        />
      </div>

      {/* Main content */}
      <div className="flex-1 flex flex-col min-w-0 w-full md:w-auto">
        <Navbar 
          user={user}
          onLogout={onLogout}
          title={t('dashboard.operations.title', 'Operations Manager Dashboard')}
          subtitle={t('dashboard.operations.subtitle', 'Monitor and manage operational metrics')}
        />
        {/* Mobile Navigation - Tab bar at bottom for mobile */}
        <div className="md:hidden fixed bottom-0 left-0 right-0 bg-white/90 backdrop-blur-lg border-t border-white/30 px-4 py-2 z-50">
          <div className="flex justify-around items-center">
            <button
              onClick={() => setTab("overview")}
              className={`flex flex-col items-center py-2 px-1 ${tab === "overview" ? "text-blue-600" : "text-gray-600"}`}
            >
              <FiHome className="text-xl mb-1" />
              <span className="text-xs">{t('dashboard.operations.tabs.overview', 'Overview')}</span>
            </button>
            <button
              onClick={() => setTab("performance")}
              className={`flex flex-col items-center py-2 px-1 ${tab === "performance" ? "text-blue-600" : "text-gray-600"}`}
            >
              <FiBarChart2 className="text-xl mb-1" />
              <span className="text-xs">{t('dashboard.operations.tabs.performance', 'Performance')}</span>
            </button>
            <button
              onClick={() => setTab("monitoring")}
              className={`flex flex-col items-center py-2 px-1 ${tab === "monitoring" ? "text-blue-600" : "text-gray-600"}`}
            >
              <FiActivity className="text-xl mb-1" />
              <span className="text-xs">{t('dashboard.operations.tabs.monitoring', 'Monitoring')}</span>
            </button>
            <button
              onClick={() => setTab("staff")}
              className={`flex flex-col items-center py-2 px-1 ${tab === "staff" ? "text-blue-600" : "text-gray-600"}`}
            >
              <FiUsers className="text-xl mb-1" />
              <span className="text-xs">{t('dashboard.operations.tabs.staff', 'Staff')}</span>
            </button>
          </div>
        </div>
        <main className="flex-1 p-2 sm:p-3 md:p-6 overflow-hidden pb-16 md:pb-0">
          {tab === "overview" && (
            <OperationsOverview 
              ref={operationsOverviewRef}
              metricsData={metricsData}
              isLoading={isLoadingMetrics}
              onRefresh={handleMetricsUpdate}
            />
          )}
          {tab === "monitoring" && <ResourceMonitoring />}
          {tab === "performance" && <PerformanceAnalytics />}
          {tab === "staff" && <StaffManagement />}
        </main>
      </div>
    </div>
  );
}
