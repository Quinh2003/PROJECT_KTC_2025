// environment.dart
// Cấu hình môi trường kết nối với Spring Boot backend
import 'package:shared_preferences/shared_preferences.dart';

class Environment {
  // Singleton instance
  static final Environment _instance = Environment._internal();

  // Factory constructor to get instance
  static Environment getInstance() => _instance;

  // Keys for SharedPreferences
  static const String _apiBaseUrlKey = 'api_base_url';
  static const String _isTestModeKey = 'is_test_mode';

  // Default values
  static const String _defaultApiBaseUrl = 'http://localhost:8080/api';
  static const String _defaultSocketUrl = 'http://localhost:8080';

  // Runtime values that can be changed
  String _apiBaseUrl = _defaultApiBaseUrl;
  String _socketUrl = _defaultSocketUrl;
  bool _isTestMode = false;

  Environment._internal() {
    // Load saved values when initialized
    _loadSavedValues();
  }

  // Async load saved values from SharedPreferences
  Future<void> _loadSavedValues() async {
    try {
      final prefs = await SharedPreferences.getInstance();
      _apiBaseUrl = prefs.getString(_apiBaseUrlKey) ?? _defaultApiBaseUrl;
      _isTestMode = prefs.getBool(_isTestModeKey) ?? false;

      // Derive socket URL from API base URL
      _socketUrl = _apiBaseUrl.replaceAll('/api', '');
    } catch (e) {
      print('Error loading environment settings: $e');
      // Use defaults if loading fails
      _apiBaseUrl = _defaultApiBaseUrl;
      _socketUrl = _defaultSocketUrl;
      _isTestMode = false;
    }
  }

  // Save current values to SharedPreferences
  Future<void> saveValues() async {
    try {
      final prefs = await SharedPreferences.getInstance();
      await prefs.setString(_apiBaseUrlKey, _apiBaseUrl);
      await prefs.setBool(_isTestModeKey, _isTestMode);
    } catch (e) {
      print('Error saving environment settings: $e');
    }
  }

  // Update API base URL and save
  Future<void> updateApiBaseUrl(String newApiBaseUrl) async {
    _apiBaseUrl = newApiBaseUrl;
    _socketUrl = newApiBaseUrl.replaceAll('/api', '');
    await saveValues();
  }

  // Toggle between test and live mode
  Future<void> setTestMode(bool isTestMode) async {
    _isTestMode = isTestMode;
    await saveValues();
  }

  // Force reload environment from storage
  Future<void> reloadEnvironment() async {
    await _loadSavedValues();
    print('🔄 Environment force reloaded');
  }

  // Getters
  String get apiBaseUrl => _apiBaseUrl;
  String get socketUrl => _socketUrl;
  bool get isTestMode => _isTestMode;

  // Compat for older code
  String get endpointApi => _apiBaseUrl;
  String get endpointBase => '$_socketUrl/';

  // Map services
  final String mapboxAccessToken = 'pk.your_mapbox_access_token_here';

  // API Endpoints - Authentication
  String get loginUrl => '$_apiBaseUrl/auth/login';
  String get logoutUrl => '$_apiBaseUrl/auth/logout';
  String get registerUrl => '$_apiBaseUrl/auth/register';

  // API Endpoints - Users & Drivers
  String get userProfileUrl => '$_apiBaseUrl/users/profile';
  String get updateUserProfileUrl => '$_apiBaseUrl/users/profile';

  // API Endpoints - Orders
  String get ordersUrl => '$_apiBaseUrl/orders';
  String get orderStatusUrl => '$_apiBaseUrl/orders'; // + /{id}/status
  String get orderTrackingUrl => '$_apiBaseUrl/orders'; // + /{id}/tracking

  // API Endpoints - Deliveries
  String get deliveriesUrl => '$_apiBaseUrl/deliveries';
  String get deliveryTrackingUrl =>
      '$_apiBaseUrl/deliveries'; // + /{id}/tracking

  // API Endpoints - Routes
  String get routesUrl => '$_apiBaseUrl/routes';
  String get routeTrackingUrl => '$_apiBaseUrl/routes'; // + /{id}/tracking

  // API Endpoints - GPS Tracking
  String get updateLocationUrl => '$_apiBaseUrl/tracking/update';
  String get vehicleTrackingUrl => '$_apiBaseUrl/tracking/vehicles'; // + /{id}
  String get trackingHistoryUrl => '$_apiBaseUrl/tracking/history';

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
