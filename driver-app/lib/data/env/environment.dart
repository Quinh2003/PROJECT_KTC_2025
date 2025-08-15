
// environment.dart
// Cấu hình môi trường kết nối với Spring Boot backend

class Environment {
  // Singleton instance
  static final Environment _instance = Environment._internal();
  
  // Factory constructor to get instance
  static Environment getInstance() => _instance;
  
  Environment._internal();
  
  // Base URL settings
  final String apiBaseUrl = 'http://10.0.2.2:8080/api';
  final String socketUrl = 'http://10.0.2.2:8080';
  
  // Compat for older code
  String get endpointApi => apiBaseUrl;
  String get endpointBase => 'http://10.0.2.2:8080/';
  
  // Map services
  final String mapboxAccessToken = 'pk.your_mapbox_access_token_here';
  
  // API Endpoints - Authentication
  String get loginUrl => '$apiBaseUrl/auth/login';
  String get logoutUrl => '$apiBaseUrl/auth/logout';
  String get registerUrl => '$apiBaseUrl/auth/register';
  
  // API Endpoints - Users & Drivers
  String get userProfileUrl => '$apiBaseUrl/users/profile';
  String get updateUserProfileUrl => '$apiBaseUrl/users/profile';
  
  // API Endpoints - Orders
  String get ordersUrl => '$apiBaseUrl/orders';
  String get orderStatusUrl => '$apiBaseUrl/orders';  // + /{id}/status
  String get orderTrackingUrl => '$apiBaseUrl/orders'; // + /{id}/tracking
  
  // API Endpoints - Deliveries
  String get deliveriesUrl => '$apiBaseUrl/deliveries';
  String get deliveryTrackingUrl => '$apiBaseUrl/deliveries'; // + /{id}/tracking
  
  // API Endpoints - Routes
  String get routesUrl => '$apiBaseUrl/routes';
  String get routeTrackingUrl => '$apiBaseUrl/routes'; // + /{id}/tracking
  
  // API Endpoints - GPS Tracking
  String get updateLocationUrl => '$apiBaseUrl/tracking/update';
  String get vehicleTrackingUrl => '$apiBaseUrl/tracking/vehicles'; // + /{id}
  String get trackingHistoryUrl => '$apiBaseUrl/tracking/history';
  
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


