
// environment.dart
// Cấu hình môi trường kết nối với Spring Boot backend

class Environment {
  // Base URL của Spring Boot backend (10.0.2.2 là localhost của máy host khi chạy từ emulator)
  static const String endpointBase = 'http://10.0.2.2:8080/';
  static const String endpointApi = 'http://10.0.2.2:8080/api';
  
  // URL cho WebSocket
  static const String socketUrl = 'http://10.0.2.2:8080';
  
  // API Endpoints - Authentication
  static const String loginUrl = '$endpointApi/auth/login';
  static const String logoutUrl = '$endpointApi/auth/logout';
  static const String registerUrl = '$endpointApi/auth/register';
  static const String googleLoginUrl = '$endpointApi/auth/google-credential';
  
  // API Endpoints - Users & Drivers
  static const String userProfileUrl = '$endpointApi/users/profile';
  static const String updateUserProfileUrl = '$endpointApi/users/profile';
  
  // API Endpoints - Orders
  static const String ordersUrl = '$endpointApi/orders';
  static const String orderStatusUrl = '$endpointApi/orders'; // + /{id}/status
  static const String orderTrackingUrl = '$endpointApi/orders'; // + /{id}/tracking
  
  // API Endpoints - Deliveries
  static const String deliveriesUrl = '$endpointApi/deliveries';
  static const String deliveryTrackingUrl = '$endpointApi/deliveries'; // + /{id}/tracking
  
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


