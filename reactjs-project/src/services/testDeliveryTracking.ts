import { createDelivery, createDeliveryTrackingPoint, getStartCoordinatesFromStore, getDeliveryStatusId } from './deliveryTrackingAPI';
import { getStoreById } from './storeAPI';

/**
 * Test function to demonstrate the complete delivery tracking flow
 */
export async function testDeliveryTrackingFlow() {
  try {
    console.log('🚀 Testing delivery tracking flow...');
    
    // Test 1: Get store information
    console.log('📍 Step 1: Getting store information...');
    const store = await getStoreById(1); // Assuming store ID 1 exists
    if (store) {
      console.log('✅ Store found:', {
        id: store.id,
        name: store.storeName,
        address: store.address,
        latitude: store.latitude,
        longitude: store.longitude
      });
    } else {
      console.log('❌ Store not found');
    }
    
    // Test 2: Get store coordinates
    console.log('🗺️ Step 2: Getting store coordinates...');
    const coordinates = await getStartCoordinatesFromStore(1);
    if (coordinates) {
      console.log('✅ Store coordinates:', coordinates);
    } else {
      console.log('❌ Could not get store coordinates');
    }
    
    // Test 3: Create delivery (this would be done during vehicle assignment)
    console.log('🚚 Step 3: Creating delivery record...');
    const deliveryData = {
      orderId: 1, // Example order ID
      vehicleId: 1, // Example vehicle ID
      driverId: 1, // Example driver ID
      transportMode: 'ROAD',
      serviceType: 'STANDARD',
      scheduleDeliveryTime: new Date(Date.now() + 24 * 60 * 60 * 1000).toISOString(),
      deliveryNotes: 'Test delivery for tracking flow'
    };
    
    const delivery = await createDelivery(deliveryData);
    console.log('✅ Delivery created:', {
      id: delivery.id,
      orderId: delivery.orderId,
      vehicleId: delivery.vehicleId
    });
    
    // Test 4: Create initial tracking point
    console.log('📌 Step 4: Creating initial tracking point...');
    if (coordinates && delivery.vehicleId) {
      const trackingData = {
        deliveryId: delivery.id,
        vehicleId: delivery.vehicleId,
        statusId: getDeliveryStatusId('STARTED'),
        latitude: coordinates.latitude,
        longitude: coordinates.longitude,
        location: store?.address || 'Store location',
        notes: 'Delivery started from store location'
      };
      
      const trackingPoint = await createDeliveryTrackingPoint(trackingData);
      console.log('✅ Tracking point created:', {
        id: trackingPoint.id,
        latitude: trackingPoint.latitude,
        longitude: trackingPoint.longitude,
        location: trackingPoint.location
      });
    }
    
    console.log('🎉 Delivery tracking flow test completed successfully!');
    
  } catch (error) {
    console.error('❌ Delivery tracking flow test failed:', error);
    throw error;
  }
}

// Export for use in development/testing
if (typeof window !== 'undefined') {
  (window as any).testDeliveryTrackingFlow = testDeliveryTrackingFlow;
}
