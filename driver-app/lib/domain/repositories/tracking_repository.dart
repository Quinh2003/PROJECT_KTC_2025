import 'package:geolocator/geolocator.dart';

import '../models/tracking_model.dart';

abstract class TrackingRepository {
  /// Update current driver location
  Future<LocationUpdateResponse> updateLocation(TrackingPoint trackingPoint);
  
  /// Start tracking a route
  Future<bool> startRouteTracking({
    required String routeId,
    required String driverId,
    String? vehicleId,
  });
  
  /// End tracking a route
  Future<bool> endRouteTracking({
    required String routeId,
  });
  
  /// Start tracking a delivery
  Future<bool> startDeliveryTracking({
    required String deliveryId,
    required String routeId,
  });
  
  /// End tracking a delivery
  Future<bool> endDeliveryTracking({
    required String deliveryId,
    required String status,
    Map<String, dynamic>? metadata,
  });
  
  /// Log a tracking event
  Future<bool> logTrackingEvent(TrackingEvent event);
  
  /// Get tracking history for a driver
  Future<TrackingHistoryResponse> getTrackingHistory({
    required String driverId,
    required DateTime startDate,
    required DateTime endDate,
  });
  
  /// Get route tracking details
  Future<RouteTrackingResponse> getRouteTracking({
    required String routeId,
  });
  
  /// Get order tracking details
  Future<OrderTrackingResponse> getOrderTracking({
    required String orderId,
  });
  
  /// Get delivery tracking details
  Future<DeliveryTrackingResponse> getDeliveryTracking({
    required String deliveryId,
  });
  
  /// Get vehicle's current location
  Future<VehicleLocationResponse> getVehicleLocation({
    required String vehicleId,
  });
  
  /// Get routes assigned to a driver
  Future<RoutesResponse> getDriverRoutes({
    required String driverId,
    int page = 1,
    int limit = 10,
    String? status,
    DateTime? startDate,
    DateTime? endDate,
  });
  
  /// Get a specific route details
  Future<RouteResponse> getRouteDetails({
    required String routeId,
  });
}


