/**
 * Debug utilities for tracking data consistency
 */

export interface TrackingDebugInfo {
  selectedOrderId?: number;
  deliveryId?: number;
  deliveryOrderId?: number;
  trackingId?: number;
  vehicleId?: number;
  isConsistent: boolean;
  issues: string[];
}

/**
 * Check consistency between selectedOrder, deliveryData, and trackingData
 */
export function checkTrackingDataConsistency(
  selectedOrder: any,
  deliveryData: any,
  trackingData: any
): TrackingDebugInfo {
  const result: TrackingDebugInfo = {
    selectedOrderId: selectedOrder?.id,
    deliveryId: deliveryData?.id,
    deliveryOrderId: deliveryData?.orderId || deliveryData?.order?.id,
    trackingId: trackingData?.id,
    vehicleId: selectedOrder?.vehicle?.id,
    isConsistent: true,
    issues: []
  };

  // Check order ID consistency
  if (result.selectedOrderId && result.deliveryOrderId) {
    if (result.selectedOrderId !== result.deliveryOrderId) {
      result.isConsistent = false;
      result.issues.push(
        `Order ID mismatch: selectedOrder.id=${result.selectedOrderId} vs delivery.orderId=${result.deliveryOrderId}`
      );
    }
  }

  // Check if delivery exists for selected order
  if (result.selectedOrderId && !result.deliveryId) {
    result.isConsistent = false;
    result.issues.push(`No delivery found for order ID: ${result.selectedOrderId}`);
  }

  // Check vehicle ID consistency
  const deliveryVehicleId = deliveryData?.vehicleId || deliveryData?.vehicle?.id;
  if (result.vehicleId && deliveryVehicleId) {
    if (result.vehicleId !== deliveryVehicleId) {
      result.isConsistent = false;
      result.issues.push(
        `Vehicle ID mismatch: selectedOrder.vehicle.id=${result.vehicleId} vs delivery.vehicleId=${deliveryVehicleId}`
      );
    }
  }

  return result;
}

/**
 * Log tracking data consistency in a formatted way
 */
export function logTrackingConsistency(
  context: string,
  selectedOrder: any,
  deliveryData: any,
  trackingData: any
): void {
  const debugInfo = checkTrackingDataConsistency(selectedOrder, deliveryData, trackingData);
  
  console.group(`🔍 [${context}] Tracking Data Consistency Check`);
  console.log('📊 Debug Info:', debugInfo);
  
  if (debugInfo.isConsistent) {
    console.log('✅ Data is consistent!');
  } else {
    console.warn('❌ Data inconsistencies found:');
    debugInfo.issues.forEach(issue => console.warn(`  - ${issue}`));
  }
  
  console.log('📝 Raw Data:');
  console.log('  - selectedOrder:', selectedOrder);
  console.log('  - deliveryData:', deliveryData);
  console.log('  - trackingData:', trackingData);
  console.groupEnd();
}

/**
 * Get the correct order ID to display (always prefer selectedOrder.id)
 */
export function getDisplayOrderId(selectedOrder: any, deliveryData: any): string | number {
  return selectedOrder?.id || deliveryData?.orderId || deliveryData?.order?.id || 'N/A';
}

/**
 * Get the correct delivery ID to display
 */
export function getDisplayDeliveryId(deliveryData: any, fallbackDeliveryId: any): string | number {
  return deliveryData?.id || fallbackDeliveryId || 'N/A';
}

/**
 * Export for browser console debugging
 */
if (typeof window !== 'undefined') {
  (window as any).debugTracking = {
    checkTrackingDataConsistency,
    logTrackingConsistency,
    getDisplayOrderId,
    getDisplayDeliveryId
  };
}