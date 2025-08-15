
// environment.dart
// Cấu hình môi trường kết nối với Spring Boot backend

class Environment {
  // Singleton instance
  static final Environment _instance = Environment._internal();
  
  // Factory constructor to get instance
  factory Environment.getInstance() => _instance;
  
  Environment._internal();
  
  // Base URL settings
  final String apiBaseUrl = 'http://10.0.2.2:8080/api';
  final String socketUrl = 'http://10.0.2.2:8080';
  
  // Map services
  final String mapboxAccessToken = 'pk.your_mapbox_access_token_here';
  
  // API Endpoints - Authentication
  final String loginUrl = 'auth/login';
  final String logoutUrl = 'auth/logout';
  final String registerUrl = 'auth/register';
  
  // API Endpoints - Users & Drivers
  final String userProfileUrl = 'users/profile';
  final String updateUserProfileUrl = 'users/profile';
  
  // API Endpoints - Orders
  final String ordersUrl = 'orders';
  final String orderStatusUrl = 'orders'; // + /{id}/status
  final String orderTrackingUrl = 'orders'; // + /{id}/tracking
  
  // API Endpoints - Deliveries
  final String deliveriesUrl = 'deliveries';
  final String deliveryTrackingUrl = 'deliveries'; // + /{id}/tracking
  
  // API Endpoints - Routes
  final String routesUrl = 'routes';
  final String routeTrackingUrl = 'routes'; // + /{id}/tracking
  
  // API Endpoints - GPS Tracking
  final String updateLocationUrl = 'tracking/update';
  final String vehicleTrackingUrl = 'tracking/vehicles'; // + /{id}
  final String trackingHistoryUrl = 'tracking/history';
  
  // Socket Events
  final String newOrderEvent = 'new_order';
  final String orderUpdatedEvent = 'order_updated';
  final String orderCancelledEvent = 'order_cancelled';
  final String locationUpdateEvent = 'driver_location';
  final String statusUpdateEvent = 'driver_status';
  
  // Order Status
  final String orderStatusPending = 'PENDING';
  final String orderStatusAssigned = 'ASSIGNED';
  final String orderStatusPickedUp = 'PICKED_UP';
  final String orderStatusDelivering = 'DELIVERING';
  final String orderStatusDelivered = 'DELIVERED';
  final String orderStatusCancelled = 'CANCELLED';
  
  // Driver Status
  final String driverStatusOffline = 'OFFLINE';
  final String driverStatusOnline = 'ONLINE';
  final String driverStatusBusy = 'BUSY';
  final String driverStatusResting = 'RESTING';
}
  
  // API Endpoints - Routes
  static const String routesUrl = '$endpointApi/routes';
  static const String routeTrackingUrl = '$endpointApi/routes'; // + /{id}/tracking
  
  // API Endpoints - GPS Tracking
  static const String updateLocationUrl = '$endpointApi/tracking/update';
  static const String vehicleTrackingUrl = '$endpointApi/tracking/vehicles'; // + /{id}
  static const String trackingHistoryUrl = '$endpointApi/tracking/history';
  
  // Socket Events
  static const String newOrderEvent = 'new_order';
  static const String orderUpdatedEvent = 'order_updated';
  static const String orderCancelledEvent = 'order_cancelled';
  static const String locationUpdateEvent = 'driver_location';
  static const String statusUpdateEvent = 'driver_status';
  
  // Order Status
  static const String orderStatusPending = 'PENDING';
  static const String orderStatusAssigned = 'ASSIGNED';
  static const String orderStatusPickedUp = 'PICKED_UP';
  static const String orderStatusDelivering = 'DELIVERING';
  static const String orderStatusDelivered = 'DELIVERED';
  static const String orderStatusCancelled = 'CANCELLED';
  
  // Driver Status
  static const String driverStatusOffline = 'OFFLINE';
  static const String driverStatusOnline = 'ONLINE';
  static const String driverStatusBusy = 'BUSY';
  static const String driverStatusResting = 'RESTING';
}


