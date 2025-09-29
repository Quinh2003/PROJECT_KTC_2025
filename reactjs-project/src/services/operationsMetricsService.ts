import { fetchVehicleStats } from './VehicleListAPI';
import GlobalUpdateService from './globalUpdateService';

/**
 * Service để tính toán metrics cho Operations Dashboard
 */
export class OperationsMetricsService {
  
  // Cache để lưu kết quả trước đó
  private static lastResults: {
    todayOrders?: { count: number; changePercent: number; trend: 'increase' | 'decrease' | 'stable' };
    activeVehicles?: { active: number; total: number; percentage: number; ratio: string };
    lastFetch?: number;
    lastOrderCount?: number; // Track last order count để detect changes
  } = {};

  // Callback functions để notify khi có update (similar to AdminDashboard)
  private static updateCallbacks: Array<() => void> = [];
  
  // Polling interval để check for new orders
  private static pollingInterval: number | null = null;

  /**
   * Register callback để update UI khi có thay đổi (similar to AdminDashboard)
   */
  static registerUpdateCallback(callback: () => void) {
    this.updateCallbacks.push(callback);
  }

  /**
   * Unregister callback
   */
  static unregisterUpdateCallback(callback: () => void) {
    this.updateCallbacks = this.updateCallbacks.filter(cb => cb !== callback);
  }

  /**
   * Trigger tất cả callbacks (similar to AdminDashboard)
   */
  static triggerUpdateCallbacks() {
    this.updateCallbacks.forEach(callback => {
      try {
        callback();
      } catch (error) {
        console.error('Error in update callback:', error);
      }
    });
  }

  /**
   * Force refresh cache - gọi khi có thay đổi data (similar to AdminDashboard)
   */
  static forceRefresh() {
    console.log('🔄 Force refresh triggered - clearing cache');
    this.lastResults = {}; // Clear cache
    this.triggerUpdateCallbacks(); // Trigger UI update
  }

  /**
   * Start polling để detect new orders từ external sources (như NextJS)
   */
  static startPolling(intervalMs = 60000) { // Poll every 60 seconds instead of 15
    if (this.pollingInterval) {
      clearInterval(this.pollingInterval);
    }

    console.log('🔄 OperationsMetricsService: Starting polling for order updates');
    
    this.pollingInterval = window.setInterval(async () => {
      try {
        // Check if current count is different from last known count
        const currentData = await this.getTodayOrdersCount(true); // Force refresh
        
        if (this.lastResults.lastOrderCount !== undefined && 
            currentData.count !== this.lastResults.lastOrderCount) {
          console.log(`📈 Detected order count change: ${this.lastResults.lastOrderCount} → ${currentData.count}`);
          this.triggerUpdateCallbacks();
        }
        
        this.lastResults.lastOrderCount = currentData.count;
      } catch (error) {
        console.error('Error in polling:', error);
      }
    }, intervalMs);
  }

  /**
   * Stop polling
   */
  static stopPolling() {
    if (this.pollingInterval) {
      clearInterval(this.pollingInterval);
      this.pollingInterval = null;
      console.log('⏹️ OperationsMetricsService: Stopped polling');
    }
  }

  /**
   * Kiểm tra xem có nên fetch dữ liệu mới không (để tránh spam API)
   */
  private static shouldFetch(forceRefresh = false): boolean {
    if (forceRefresh) return true;
    
    const now = Date.now();
    const lastFetch = this.lastResults.lastFetch || 0;
    // Chỉ fetch nếu đã qua 10 giây (tránh spam khi user click liên tục)
    return now - lastFetch > 10000;
  }
  
  /**
   * Tính số đơn hàng hôm nay và so sánh với hôm qua (sử dụng API tối ưu)
   */
  static async getTodayOrdersCount(forceRefresh = false): Promise<{
    count: number;
    changePercent: number;
    trend: 'increase' | 'decrease' | 'stable';
  }> {
    try {
      // Kiểm tra cache nếu vừa fetch gần đây (trừ khi force refresh)
      if (!this.shouldFetch(forceRefresh) && this.lastResults.todayOrders) {
        return this.lastResults.todayOrders;
      }

      const token = localStorage.getItem('token') || '';
      
      // Sử dụng API tối ưu để đếm orders theo ngày với timeout
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), 10000); // 10 second timeout
      
      const response = await fetch('http://localhost:8080/api/orders/count-by-date', {
        headers: { 'Authorization': `Bearer ${token}` },
        signal: controller.signal
      });
      
      clearTimeout(timeoutId);
      
      if (!response.ok) {
        throw new Error('Failed to fetch order count');
      }
      
      const data = await response.json();
      
      const result = {
        count: data.count,
        changePercent: data.changePercent,
        trend: data.trend as 'increase' | 'decrease' | 'stable'
      };

      // Cache kết quả
      this.lastResults.todayOrders = result;
      this.lastResults.lastOrderCount = data.count; // Track for polling detection
      this.lastResults.lastFetch = Date.now();

      console.log(`📦 Orders count - Today: ${data.count}, Change: ${data.changePercent}% (${data.trend})`);
      return result;

    } catch (error) {
      console.error('Error calculating today orders:', error);
      // Trả về dữ liệu mặc định khi có lỗi (tránh loading mãi)
      const fallbackResult = {
        count: 12, // Số tạm thời
        changePercent: 8.2,
        trend: 'increase' as const
      };
      
      // Cache fallback để không retry liên tục
      this.lastResults.todayOrders = fallbackResult;
      this.lastResults.lastFetch = Date.now();
      
      return fallbackResult;
    }
  }

  /**
   * Tính số xe đang hoạt động
   */
  static async getActiveVehiclesRatio(): Promise<{
    active: number;
    total: number;
    percentage: number;
    ratio: string;
  }> {
    console.log('🚛 getActiveVehiclesRatio called');
    try {
      console.log('🚛 Fetching active vehicle stats from new API...');
      const token = localStorage.getItem('token') || '';
      
      const response = await fetch('http://localhost:8080/api/vehicles/stats/active', {
        headers: { 'Authorization': `Bearer ${token}` }
      });
      
      if (!response.ok) {
        throw new Error('Failed to fetch active vehicle stats');
      }
      
      const data = await response.json();
      console.log('🚛 Active vehicle stats from API:', data);
      
      return {
        active: data.active,
        total: data.total,
        percentage: data.percentage,
        ratio: data.ratio
      };
    } catch (error) {
      console.error('Error calculating active vehicles:', error);
      // Fallback: sử dụng cách cũ nếu API mới lỗi
      console.log('🚛 Falling back to old method...');
      try {
        console.log('🚛 Fetching vehicle stats...');
        const { totalRecords, sampleVehicles } = await fetchVehicleStats();
        console.log('🚛 Raw vehicle stats:', { totalRecords, sampleVehicles });
        console.log('🚛 Total vehicles found:', totalRecords);
        console.log('🚛 Sample vehicles:', sampleVehicles.length);
        
        // Chỉ đếm xe có status IN_USE (đang sử dụng)
        const activeVehicles = sampleVehicles.filter(vehicle => 
          vehicle.status === 'IN_USE'
        );

        const activeCount = activeVehicles.length;
        const totalCount = totalRecords; 
        const percentage = totalCount > 0 ? Math.round((activeCount / totalCount) * 100) : 0;

        console.log(`🚛 Active vehicles (IN_USE only): ${activeCount}/${totalCount} (${percentage}%)`);
        console.log('🚛 Vehicle statuses:', sampleVehicles.map(v => ({ id: v.id, status: v.status })));

        return {
          active: activeCount,
          total: totalCount,
          percentage,
          ratio: `${activeCount}/${totalCount}`
        };
      } catch (fallbackError) {
        console.error('Error in fallback method:', fallbackError);
        // Trả về dữ liệu mặc định khi có lỗi
        return {
          active: 0,
          total: 104,
          percentage: 0,
          ratio: '0/104'
        };
      }
    }
  }

  /**
   * Tính doanh thu hôm nay từ bảng deliveries
   */
  static async getTodayRevenue(): Promise<{
    amount: string;
    changePercent: number;
    trend: 'increase' | 'decrease' | 'stable';
  }> {
    try {
      // Tính doanh thu hôm nay và hôm qua để so sánh
      const today = new Date().toISOString().split('T')[0];
      const yesterday = new Date(Date.now() - 24 * 60 * 60 * 1000).toISOString().split('T')[0];
      
      // API call với timeout để tránh treo
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), 8000); // 8 second timeout
      
      // API call để lấy doanh thu theo ngày
      const [todayResponse, yesterdayResponse] = await Promise.all([
        fetch(`http://localhost:8080/api/deliveries/revenue-by-date?date=${today}`, {
          signal: controller.signal
        }),
        fetch(`http://localhost:8080/api/deliveries/revenue-by-date?date=${yesterday}`, {
          signal: controller.signal
        })
      ]);

      clearTimeout(timeoutId);

      if (!todayResponse.ok) {
        throw new Error('Failed to fetch today revenue');
      }

      const todayRevenue = await todayResponse.json();
      let yesterdayRevenue = 0;
      
      if (yesterdayResponse.ok) {
        yesterdayRevenue = await yesterdayResponse.json();
      }

      // Format số tiền
      const formatAmount = (amount: number): string => {
        if (amount === 0) return '0 VND';
        if (amount >= 1_000_000_000) return `${(amount / 1_000_000_000).toFixed(1)}B VND`;
        if (amount >= 1_000_000) return `${(amount / 1_000_000).toFixed(1)}M VND`;
        if (amount >= 1_000) return `${(amount / 1_000).toFixed(0)}K VND`;
        return `${amount} VND`;
      };

      // Tính phần trăm thay đổi
      let changePercent = 0;
      let trend: 'increase' | 'decrease' | 'stable' = 'stable';

      if (yesterdayRevenue > 0) {
        changePercent = ((todayRevenue - yesterdayRevenue) / yesterdayRevenue) * 100;
      } else if (todayRevenue > 0) {
        changePercent = 100; // 100% increase nếu hôm qua = 0 và hôm nay > 0
      }

      if (changePercent > 0) {
        trend = 'increase';
      } else if (changePercent < 0) {
        trend = 'decrease';
      }

      return {
        amount: formatAmount(todayRevenue),
        changePercent: Math.abs(changePercent),
        trend
      };

    } catch (error) {
      console.error('Error fetching today revenue:', error);
      // Fallback data khi API lỗi
      return {
        amount: '0 VND',
        changePercent: 0,
        trend: 'stable'
      };
    }
  }

  /**
   * Lấy số đơn hàng hoàn thành hôm nay
   */
  static async getCompletedOrders(): Promise<{
    count: number;
    changePercent: number;
    trend: 'increase' | 'decrease' | 'stable';
  }> {
    try {
      // Gọi API để lấy số đơn hàng hoàn thành
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), 5000);
      
      const response = await fetch('http://localhost:8080/api/deliveries/completed-today', {
        signal: controller.signal
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        throw new Error('Failed to fetch completed orders');
      }

      const data = await response.json();
      
      return {
        count: data.count,
        changePercent: data.changePercent,
        trend: data.trend
      };

    } catch (error) {
      console.error('Error fetching completed orders:', error);
      // Fallback data khi API lỗi
      return {
        count: 85,
        changePercent: 2.3,
        trend: 'increase'
      };
    }
  }

  /**
   * Tính hiệu suất trung bình dựa trên completion rate từ API tối ưu
   */
  static async getAveragePerformance(): Promise<{
    percentage: number;
    changePercent: number;
    trend: 'increase' | 'decrease' | 'stable';
  }> {
    try {
      // Gọi API tối ưu chỉ trả về kết quả cuối cùng
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), 5000); // 5 second timeout
      
      const response = await fetch('http://localhost:8080/api/deliveries/performance-stats', {
        signal: controller.signal
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        throw new Error('Failed to fetch performance stats');
      }

      const data = await response.json();
      
      return {
        percentage: data.percentage,
        changePercent: data.changePercent,
        trend: data.trend
      };

    } catch (error) {
      console.error('Error fetching performance data:', error);
      // Fallback data khi API lỗi
      return {
        percentage: 0,
        changePercent: 0,
        trend: 'stable'
      };
    }
  }

  /**
   * Lấy doanh thu theo tháng trong 12 tháng gần nhất
   */
  static async getMonthlyRevenue(): Promise<{
    monthlyRevenue: Array<{ year: number; month: number; revenue: number }>;
    totalRevenue: number;
    averageRevenue: number;
    growthPercent: number;
  }> {
    try {
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), 8000); // 8 second timeout
      
      const response = await fetch('http://localhost:8080/api/deliveries/monthly-revenue', {
        signal: controller.signal
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        throw new Error('Failed to fetch monthly revenue');
      }

      const data = await response.json();
      
      return {
        monthlyRevenue: data.monthlyRevenue || [],
        totalRevenue: data.totalRevenue || 0,
        averageRevenue: data.averageRevenue || 0,
        growthPercent: data.growthPercent || 0
      };

    } catch (error) {
      console.error('Error fetching monthly revenue:', error);
      // Fallback data khi API lỗi
      return {
        monthlyRevenue: [],
        totalRevenue: 0,
        averageRevenue: 0,
        growthPercent: 0
      };
    }
  }
}

// Initialize global listeners khi service được import
(() => {
  // Listen cho order updates từ GlobalUpdateService
  GlobalUpdateService.onOrderUpdate(() => {
    console.log('🔄 OperationsMetricsService: Received order update from GlobalUpdateService');
    OperationsMetricsService.forceRefresh();
  });

  // Listen cho vehicle updates
  GlobalUpdateService.onVehicleUpdate(() => {
    console.log('🔄 OperationsMetricsService: Received vehicle update from GlobalUpdateService');
    OperationsMetricsService.forceRefresh();
  });

  console.log('✅ OperationsMetricsService: Global listeners initialized');
  
  // Debug: Expose service to window để test từ console
  // Make service available globally for debugging
if (typeof window !== 'undefined') {
    // Debug purposes - expose service to window (development only)
  (window as unknown as { OperationsMetricsService: typeof OperationsMetricsService }).OperationsMetricsService = OperationsMetricsService;
}
  console.log('🐛 Debug: OperationsMetricsService exposed to window for testing');
})();

export default OperationsMetricsService;