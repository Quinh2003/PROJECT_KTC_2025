import GlobalUpdateService from '../services/globalUpdateService';

/**
 * Utilities để trigger updates từ anywhere in the app
 * Dùng khi có thao tác create/update/delete orders hoặc vehicles
 */

/**
 * Call này sau khi tạo đơn hàng mới thành công
 */
export const triggerOrderCreated = () => {
  console.log('📦 Order created - triggering dashboard update');
  GlobalUpdateService.triggerOrderUpdate();
};

/**
 * Call này sau khi cập nhật đơn hàng
 */
export const triggerOrderUpdated = () => {
  console.log('📝 Order updated - triggering dashboard update');
  GlobalUpdateService.triggerOrderUpdate();
};

/**
 * Call này sau khi xóa đơn hàng
 */
export const triggerOrderDeleted = () => {
  console.log('🗑️ Order deleted - triggering dashboard update');
  GlobalUpdateService.triggerOrderUpdate();
};

/**
 * Call này sau khi cập nhật vehicle status
 */
export const triggerVehicleStatusUpdate = () => {
  console.log('🚛 Vehicle status updated - triggering dashboard update');
  GlobalUpdateService.triggerVehicleUpdate();
};

/**
 * Call này sau khi assign driver cho vehicle
 */
export const triggerDriverAssigned = () => {
  console.log('👨‍✈️ Driver assigned - triggering dashboard update');
  GlobalUpdateService.triggerVehicleUpdate();
};

/**
 * Generic trigger cho bất kỳ thay đổi nào affect metrics
 */
export const triggerDashboardRefresh = () => {
  console.log('🔄 Manual dashboard refresh triggered');
  GlobalUpdateService.triggerOrderUpdate();
  GlobalUpdateService.triggerVehicleUpdate();
};

// Export default object với tất cả functions
export default {
  triggerOrderCreated,
  triggerOrderUpdated,
  triggerOrderDeleted,
  triggerVehicleStatusUpdate,
  triggerDriverAssigned,
  triggerDashboardRefresh
};