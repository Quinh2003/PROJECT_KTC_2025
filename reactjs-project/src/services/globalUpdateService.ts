/**
 * Global Update Service để handle real-time updates giữa các components
 * Similar to AdminDashboard pattern nhưng global scope
 */

export class GlobalUpdateService {
  private static instance: GlobalUpdateService;
  private updateCallbacks: Map<string, Array<() => void>> = new Map();

  static getInstance(): GlobalUpdateService {
    if (!GlobalUpdateService.instance) {
      GlobalUpdateService.instance = new GlobalUpdateService();
    }
    return GlobalUpdateService.instance;
  }

  /**
   * Register callback cho specific event type
   */
  registerCallback(eventType: string, callback: () => void) {
    if (!this.updateCallbacks.has(eventType)) {
      this.updateCallbacks.set(eventType, []);
    }
    this.updateCallbacks.get(eventType)?.push(callback);
    console.log(`📝 Registered callback for ${eventType}`);
  }

  /**
   * Unregister callback
   */
  unregisterCallback(eventType: string, callback: () => void) {
    const callbacks = this.updateCallbacks.get(eventType);
    if (callbacks) {
      const index = callbacks.indexOf(callback);
      if (index > -1) {
        callbacks.splice(index, 1);
        console.log(`❌ Unregistered callback for ${eventType}`);
      }
    }
  }

  /**
   * Trigger tất cả callbacks cho event type
   */
  triggerUpdate(eventType: string, data?: unknown) {
    console.log(`🔄 Triggering update for ${eventType}`, data);
    const callbacks = this.updateCallbacks.get(eventType);
    if (callbacks) {
      callbacks.forEach(callback => {
        try {
          callback();
        } catch (error) {
          console.error(`Error in callback for ${eventType}:`, error);
        }
      });
    }
  }

  /**
   * Trigger khi có đơn hàng mới
   */
  static triggerOrderUpdate() {
    GlobalUpdateService.getInstance().triggerUpdate('ORDER_UPDATED');
  }

  /**
   * Trigger khi có vehicle update
   */
  static triggerVehicleUpdate() {
    GlobalUpdateService.getInstance().triggerUpdate('VEHICLE_UPDATED');
  }

  /**
   * Register để listen order updates
   */
  static onOrderUpdate(callback: () => void) {
    GlobalUpdateService.getInstance().registerCallback('ORDER_UPDATED', callback);
  }

  /**
   * Register để listen vehicle updates
   */
  static onVehicleUpdate(callback: () => void) {
    GlobalUpdateService.getInstance().registerCallback('VEHICLE_UPDATED', callback);
  }

  /**
   * Cleanup callbacks
   */
  static cleanup(eventType: string, callback: () => void) {
    GlobalUpdateService.getInstance().unregisterCallback(eventType, callback);
  }
}

export default GlobalUpdateService;