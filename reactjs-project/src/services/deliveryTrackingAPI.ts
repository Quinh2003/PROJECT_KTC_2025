import { getAuthHeaders } from './operationsAPI';
import { getStoreCoordinates } from './storeAPI';

const API_BASE_URL = 'http://localhost:8080';

// Interface for creating a delivery record
export interface CreateDeliveryRequest {
  orderId: number;
  vehicleId: number;
  driverId?: number;
  transportMode?: string;
  serviceType?: string;
  scheduleDeliveryTime?: string;
  deliveryNotes?: string;
}

// Interface for creating delivery tracking points
export interface CreateDeliveryTrackingRequest {
  deliveryId: number;
  vehicleId: number;
  statusId: number; // Status ID from the status table
  latitude: number;
  longitude: number;
  location?: string;
  notes?: string;
}

export interface DeliveryResponse {
  id: number;
  orderId?: number; // Backend returns this from map conversion
  deliveryFee?: number;
  transportMode: string;
  serviceType: string;
  orderDate: string;
  pickupDate?: string;
  scheduleDeliveryTime?: string;
  actualDeliveryTime?: string;
  lateDeliveryRisk?: number;
  trackingId?: number;
  routeId?: number;
  deliveryAttempts?: number;
  deliveryNotes?: string;
  driverId?: number;
  vehicleId?: number;
  createdAt?: string;
  updatedAt?: string;
}

export interface DeliveryTrackingResponse {
  id: number;
  vehicleId: number;
  statusId: number;
  latitude?: number;
  longitude?: number;
  timestamp: string;
  location?: string;
  notes?: string;
  deliveryId: number;
  createdAt: string;
  updatedAt: string;
}

/**
 * Create a delivery record when vehicle is assigned to order
 */
export async function createDelivery(data: CreateDeliveryRequest): Promise<DeliveryResponse> {
  const token = localStorage.getItem("token");
  
  // Helper function to format date for backend
  const formatTimestamp = (date: Date): string => {
    return date.toISOString().replace('T', ' ').replace('Z', '').substring(0, 19);
  };
  
  // Format data to match backend DTO requirements
  const deliveryData = {
    orderId: data.orderId,
    vehicleId: data.vehicleId,
    driverId: data.driverId,
    transportMode: data.transportMode || 'ROAD', // Enum value
    serviceType: data.serviceType || 'STANDARD', // Enum value
    scheduleDeliveryTime: data.scheduleDeliveryTime ? formatTimestamp(new Date(data.scheduleDeliveryTime)) : null,
    deliveryNotes: data.deliveryNotes,
    lateDeliveryRisk: false, // Boolean instead of number
    deliveryFee: null, // Let backend calculate
    orderDate: formatTimestamp(new Date()) // Format as "yyyy-MM-dd HH:mm:ss"
  };

  console.log('Sending delivery data:', deliveryData); // Debug log

  const response = await fetch(`${API_BASE_URL}/api/deliveries`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`,
    },
    body: JSON.stringify(deliveryData),
  });

  if (!response.ok) {
    const errorText = await response.text();
    console.error('Failed to create delivery:', response.status, errorText);
    throw new Error(`Failed to create delivery: ${response.status} ${response.statusText}`);
  }

  return response.json();
}

/**
 * Create delivery tracking point
 */
export async function createDeliveryTrackingPoint(data: CreateDeliveryTrackingRequest): Promise<DeliveryTrackingResponse> {
  const token = localStorage.getItem("token");
  
  // Helper function to format date for backend
  const formatTimestamp = (date: Date): string => {
    return date.toISOString().replace('T', ' ').replace('Z', '').substring(0, 19);
  };
  
  const trackingData = {
    deliveryId: data.deliveryId,
    vehicleId: data.vehicleId,
    statusId: data.statusId,
    latitude: data.latitude,
    longitude: data.longitude,
    location: data.location,
    notes: data.notes,
    timestamp: formatTimestamp(new Date()) // Format as "yyyy-MM-dd HH:mm:ss"
  };

  console.log('Sending tracking data:', trackingData); // Debug log

  const response = await fetch(`${API_BASE_URL}/api/tracking/location`, {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`,
    },
    body: JSON.stringify(trackingData),
  });

  if (!response.ok) {
    const errorText = await response.text();
    console.error('Failed to create delivery tracking:', response.status, errorText);
    throw new Error(`Failed to create delivery tracking: ${response.status} ${response.statusText}`);
  }

  return response.json();
}

/**
 * Get delivery by order ID
 * Uses query parameter to filter by orderId instead of fetching all deliveries
 */
export async function getDeliveryByOrderId(orderId: number): Promise<DeliveryResponse | null> {
  const token = localStorage.getItem("token");
  
  try {
    // Try to use query parameter to filter by orderId (more efficient)
    const response = await fetch(`${API_BASE_URL}/api/deliveries?orderId=${orderId}`, {
      method: 'GET',
      headers: {
        'Authorization': `Bearer ${token}`,
      },
    });

    if (!response.ok) {
      // If query parameter doesn't work, fallback to getting all and filtering
      console.warn('Query by orderId failed, falling back to full fetch');
      return await getDeliveryByOrderIdFallback(orderId);
    }

    const deliveries = await response.json();
    
    // Return first matching delivery (should be only one)
    return Array.isArray(deliveries) ? (deliveries[0] || null) : deliveries;
  } catch (error) {
    console.error('Error getting delivery by orderId:', error);
    // Fallback to old method if query fails
    return await getDeliveryByOrderIdFallback(orderId);
  }
}

/**
 * Fallback method: Get all deliveries and filter (less efficient)
 */
async function getDeliveryByOrderIdFallback(orderId: number): Promise<DeliveryResponse | null> {
  const token = localStorage.getItem("token");
  
  const response = await fetch(`${API_BASE_URL}/api/deliveries`, {
    method: 'GET',
    headers: {
      'Authorization': `Bearer ${token}`,
    },
  });

  if (!response.ok) {
    throw new Error(`Failed to get deliveries: ${response.status} ${response.statusText}`);
  }

  const deliveries = await response.json();
  
  // Find delivery with matching orderId
  const delivery = deliveries.find((d: any) => d.orderId === orderId);
  
  return delivery || null;
}

/**
 * Get delivery tracking points for a delivery
 */
export async function getDeliveryTrackingPoints(deliveryId: number): Promise<DeliveryTrackingResponse[]> {
  const token = localStorage.getItem("token");
  const response = await fetch(`${API_BASE_URL}/api/delivery-tracking/delivery/${deliveryId}`, {
    method: 'GET',
    headers: {
      'Authorization': `Bearer ${token}`,
    },
  });

  if (!response.ok) {
    throw new Error(`Failed to get delivery tracking: ${response.status} ${response.statusText}`);
  }

  return response.json();
}

/**
 * Get deliveries with pagination support
 */
export async function getDeliveriesWithPagination(page: number = 0, size: number = 20, orderId?: number): Promise<{
  content: DeliveryResponse[];
  totalElements: number;
  totalPages: number;
  size: number;
  number: number;
}> {
  const token = localStorage.getItem("token");
  
  // Build query parameters
  const params = new URLSearchParams({
    page: page.toString(),
    size: size.toString(),
  });
  
  if (orderId) {
    params.append('orderId', orderId.toString());
  }
  
  const response = await fetch(`${API_BASE_URL}/api/deliveries?${params.toString()}`, {
    method: 'GET',
    headers: {
      'Authorization': `Bearer ${token}`,
    },
  });

  if (!response.ok) {
    throw new Error(`Failed to get deliveries: ${response.status} ${response.statusText}`);
  }

  return response.json();
}

/**
 * Helper function to get start coordinates from store
 * Returns actual coordinates from store table only - no fallback coordinates
 */
export async function getStartCoordinatesFromStore(storeId: number): Promise<{ latitude: number; longitude: number } | null> {
  try {
    // Only get coordinates from store database - no default values
    const storeCoordinates = await getStoreCoordinates(storeId);
    return storeCoordinates; // Will return null if store has no coordinates
  } catch (error) {
    console.error('Error getting start coordinates from store:', error);
    return null; // Return null instead of default coordinates
  }
}

/**
 * Helper function to get coordinates from address using geocoding service
 * This is a placeholder - you might want to integrate with a real geocoding service
 */
export async function getCoordinatesFromAddress(address: string): Promise<{ latitude: number; longitude: number } | null> {
  try {
    // This is a placeholder implementation
    // You should integrate with a real geocoding service like Google Maps API, OpenStreetMap Nominatim, etc.
    
    // For now, return some default coordinates for common Vietnamese cities
    const addressLower = address.toLowerCase();
    
    if (addressLower.includes('hồ chí minh') || addressLower.includes('ho chi minh') || addressLower.includes('saigon')) {
      return { latitude: 10.8231, longitude: 106.6297 };
    } else if (addressLower.includes('hà nội') || addressLower.includes('ha noi') || addressLower.includes('hanoi')) {
      return { latitude: 21.0285, longitude: 105.8542 };
    } else if (addressLower.includes('đà nẵng') || addressLower.includes('da nang')) {
      return { latitude: 16.0544, longitude: 108.2022 };
    } else if (addressLower.includes('cần thơ') || addressLower.includes('can tho')) {
      return { latitude: 10.0452, longitude: 105.7469 };
    } else if (addressLower.includes('hải phòng') || addressLower.includes('hai phong')) {
      return { latitude: 20.8449, longitude: 106.6881 };
    }
    
    // Default to Ho Chi Minh City if no match found
    return { latitude: 10.8231, longitude: 106.6297 };
  } catch (error) {
    console.error('Error getting coordinates from address:', error);
    return null;
  }
}

/**
 * Helper function to get status ID for delivery tracking
 * These should match the status table in your database
 */
export function getDeliveryStatusId(statusName: string): number {
  const statusMap: Record<string, number> = {
    'STARTED': 1,     // When delivery starts
    'PICKED_UP': 2,   // When item is picked up
    'IN_TRANSIT': 3,  // When vehicle is moving
    'DELIVERED': 4,   // When item is delivered
    'FAILED': 5,      // When delivery fails
    'CANCELLED': 6    // When delivery is cancelled
  };
  
  return statusMap[statusName] || 1; // Default to STARTED
}

// Update delivery (PATCH)
export async function updateDelivery(deliveryId: number, data: Partial<CreateDeliveryRequest>): Promise<DeliveryResponse> {
  const token = localStorage.getItem("token");
  // Helper function to format date for backend
  const formatTimestamp = (date: Date): string => {
    return date.toISOString().replace('T', ' ').replace('Z', '').substring(0, 19);
  };
  const updateData: any = {};
  if (data.vehicleId) updateData.vehicleId = data.vehicleId;
  if (data.driverId) updateData.driverId = data.driverId;
  if (data.transportMode) updateData.transportMode = data.transportMode;
  if (data.serviceType) updateData.serviceType = data.serviceType;
  if (data.scheduleDeliveryTime) updateData.scheduleDeliveryTime = formatTimestamp(new Date(data.scheduleDeliveryTime));
  if (data.deliveryNotes) updateData.deliveryNotes = data.deliveryNotes;
  // PATCH/PUT to /api/deliveries/{id}
  const response = await fetch(`${API_BASE_URL}/api/deliveries/${deliveryId}`, {
    method: 'PUT',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`,
    },
    body: JSON.stringify(updateData),
  });
  if (!response.ok) {
    const errorText = await response.text();
    console.error('Failed to update delivery:', response.status, errorText);
    throw new Error(`Failed to update delivery: ${response.status} ${response.statusText}`);
  }
  return response.json();
}
