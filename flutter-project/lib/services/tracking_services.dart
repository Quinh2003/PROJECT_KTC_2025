import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'package:flutter/foundation.dart';
import 'package:geolocator/geolocator.dart';
import 'package:flutter_local_notifications/flutter_local_notifications.dart';
import 'package:http/http.dart' as http;
import 'package:permission_handler/permission_handler.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:geocoding/geocoding.dart';
import '../data/env/environment.dart';
import '../data/local_secure/secure_storage.dart';
// Removed unused import
import '../domain/models/tracking/driver_location.dart';
// Removed unused import

/// LocationService - Optimized for performance and battery efficiency
/// Manages background tracking, location updates, and server communication
class LocationService {
  // Singleton pattern
  static final LocationService _instance = LocationService._internal();
  factory LocationService() => _instance;

  // Private constructor
  LocationService._internal() {
    // Initialize necessary components
  }

  // Dependencies
  final Environment _env = Environment.getInstance();
  final SecureStorageFrave secureStorage = SecureStorageFrave();
  final FlutterLocalNotificationsPlugin _notificationsPlugin =
      FlutterLocalNotificationsPlugin();

  // Tracking state
  StreamSubscription<Position>? _positionStreamSubscription;
  Timer? _locationUpdateTimer;
  bool _isTracking = false;
  
  // Key cho SharedPreferences để lưu thông tin tracking
  final String _lastTrackingTimeKey = 'last_tracking_time';
  final String _activeDeliveryIdKey = 'active_delivery_id';
  final String _activeDriverIdKey = 'active_driver_id';
  final String _activeVehicleIdKey = 'active_vehicle_id';
  final String _activeStatusIdKey = 'active_status_id';

  // Getters
  String get baseUrl => _env.apiBaseUrl;
  bool get isTracking => _isTracking;

  /// Initialize the location service
  Future<void> initialize() async {
    const androidSettings =
        AndroidInitializationSettings('@mipmap/ic_launcher');
    final iosSettings = DarwinInitializationSettings(
      requestAlertPermission: false,
      requestBadgePermission: false,
      requestSoundPermission: false,
    );

    final initSettings = InitializationSettings(
      android: androidSettings,
      iOS: iosSettings,
    );

    await _notificationsPlugin.initialize(initSettings);
  }

  /// Start tracking with specific parameters
  ///
  /// [latitude] Current latitude of the driver
  /// [longitude] Current longitude of the driver
  /// [speed] Current speed in m/s
  /// [heading] Current heading in degrees
  /// [vehicleStatus] Status of the vehicle (active, idle, etc.)
  /// [updateInterval] How often to update location if continuous tracking fails
  Future<void> startTracking({
    required double latitude,
    required double longitude,
    required double speed,
    required double heading,
    required String vehicleStatus,
    Duration updateInterval = const Duration(minutes: 1),
  }) async {
    if (_isTracking) return;

    await startBackgroundLocationService();

    // Send initial location update
    await _sendLocationToServer(latitude, longitude, speed, heading);

    // Set up periodic updates as fallback if continuous stream isn't available
    if (_positionStreamSubscription == null) {
      _locationUpdateTimer?.cancel();
      _locationUpdateTimer = Timer.periodic(updateInterval, (timer) async {
        try {
          final position = await Geolocator.getCurrentPosition(
              desiredAccuracy: LocationAccuracy.high);
          await _sendLocationToServer(position.latitude, position.longitude,
              position.speed, position.heading);
        } catch (e) {
          // Fallback to last known position
          await _sendLocationToServer(latitude, longitude, speed, heading);
          debugPrint('Using fallback location - Error: $e');
        }
      });
    }
  }

  /// Stop all location tracking and clean up resources
  Future<void> stopBackgroundLocationService() async {
    if (!_isTracking) return;

    _isTracking = false;

    // Cancel position stream
    if (_positionStreamSubscription != null) {
      await _positionStreamSubscription?.cancel();
      _positionStreamSubscription = null;
    }

    // Cancel timer
    if (_locationUpdateTimer != null) {
      _locationUpdateTimer?.cancel();
      _locationUpdateTimer = null;
    }

    // Remove foreground notification on Android
    if (Platform.isAndroid) {
      await _notificationsPlugin.cancel(888);
    }

    debugPrint('Background location service stopped');
  }

  /// Start background location tracking service
  /// Sets up continuous location tracking and periodic updates
  Future<void> startBackgroundLocationService() async {
    if (_isTracking) return;

    if (!await _checkLocationPermission()) {
      debugPrint('Location permission not granted');
      return;
    }

    _isTracking = true;

    // Start foreground notification on Android
    if (Platform.isAndroid) {
      await _showForegroundNotification();
    }

    // Configure location settings based on platform
    final locationSettings = _getPlatformLocationSettings();

    // Start continuous location updates
    _positionStreamSubscription = Geolocator.getPositionStream(
      locationSettings: locationSettings,
    ).listen(
      _handlePositionUpdate,
      onError: (error) => debugPrint('Error getting location: $error'),
    );

    // Start periodic location updates as backup
    _startPeriodicLocationUpdates();
  }

  /// Send location data to server
  /// Updates the driver's current location in the tracking system
  ///
  /// [location] The driver location data to send
  /// Returns true if update was successful
  Future<bool> updateDriverLocation(DriverLocation location) async {
    final driverId = await _getDriverId();
    if (driverId == null) {
      debugPrint('Driver ID not found - cannot update location');
      return false;
    }

    // Prepare payload for direct tracking endpoint
    final prefs = await SharedPreferences.getInstance();
    final deliveryId = prefs.getInt(_activeDeliveryIdKey);
    
    if (deliveryId == null) {
      debugPrint('No active delivery found for tracking update');
      return false;
    }
    
    final token = await secureStorage.readToken();
    if (token == null) {
      debugPrint('Token not found - cannot update location');
      return false;
    }
    
    // Get saved status ID, default to 2 (in progress)
    final statusId = prefs.getInt(_activeStatusIdKey) ?? 2;
    
    final data = {
      'latitude': location.latitude,
      'longitude': location.longitude,
      'location': 'Đang giao hàng',
      'notes': 'Đang giao hàng',
      'statusId': statusId,
      'vehicleId': await getSavedVehicleId() ?? 10
    };
    
    try {
      final resp = await http.post(
          Uri.parse('$baseUrl/drivers/$driverId/deliveries/$deliveryId/tracking'),
          headers: {
            'Accept': 'application/json',
            'Authorization': 'Bearer $token',
            'Content-Type': 'application/json'
          },
          body: json.encode(data));
          
      return resp.statusCode == 200 || resp.statusCode == 201;
    } catch (e) {
      debugPrint('Error updating driver location: $e');
      return false;
    }
  }

  /// Track a specific delivery's location and status
  /// Associates the current location with a specific delivery order
  ///
  /// [deliveryId] ID of the delivery to update
  /// [location] Current location data
  /// [notes] Optional notes about the delivery status
  /// [statusId] Optional status ID for the delivery
  ///
  /// Returns true if update was successful
  Future<bool> updateDeliveryTracking(int deliveryId, DriverLocation location,
      {String? notes, int? statusId}) async {
    final token = await secureStorage.readToken();
    final driverId = await secureStorage.readDriverId();
    
    // Lấy vehicle ID đã lưu, fallback về 10 nếu không có
    final savedVehicleId = await getSavedVehicleId();
    final vehicleId = savedVehicleId ?? 10; // Default vehicle ID

    if (token == null || driverId == null) {
      debugPrint(
          'Token or driver ID not found - cannot update delivery tracking');
      return false;
    }

    final data = {
      'latitude': location.latitude,
      'longitude': location.longitude,
      'location': '',  // Empty location as requested
      'statusId': statusId ?? 2, // Default status (in progress)
      'notes': notes ?? 'Đang giao hàng',
      'vehicleId': vehicleId
    };

    debugPrint('🚛 Using vehicle ID: $vehicleId for tracking update');

    try {
      final resp = await http.post(
          Uri.parse(
              '$baseUrl/drivers/$driverId/deliveries/$deliveryId/tracking'),
          headers: {
            'Accept': 'application/json',
            'Authorization': 'Bearer $token',
            'Content-Type': 'application/json'
          },
          body: json.encode(data));

      final success = resp.statusCode == 200 || resp.statusCode == 201;
      if (!success) {
        debugPrint(
            'Delivery tracking update failed: ${resp.statusCode} - ${resp.body}');
      }
      return success;
    } catch (e) {
      debugPrint('Error updating delivery tracking: $e');
      return false;
    }
  }

  // PRIVATE METHODS

  /// Send current location to server
  /// Creates a DriverLocation object and sends it to the server
  Future<bool> _sendLocationToServer(
      double latitude, double longitude, double speed, double heading) async {
    try {
      final driverLocation = DriverLocation(
        latitude: latitude,
        longitude: longitude,
        timestamp: DateTime.now().toIso8601String(),
        speed: speed,
        heading: heading,
        vehicleStatus: 'active',
      );

      final result = await updateDriverLocation(driverLocation);
      if (kDebugMode) {
        print(
            'Location sent: Lat: $latitude, Long: $longitude, Result: $result');
      }
      return result;
    } catch (e) {
      debugPrint('Error sending location: $e');
      return false;
    }
  }

  // Legacy method removed to streamline code

  /// Get driver ID from secure storage
  Future<int?> _getDriverId() async {
    final driverId = await secureStorage.readDriverId();
    return driverId != null ? int.parse(driverId) : null;
  }

  /// Check if location permissions are granted
  /// Requests permissions if not already granted
  Future<bool> _checkLocationPermission() async {
    try {
      // Check if location services are enabled
      if (!await Geolocator.isLocationServiceEnabled()) {
        debugPrint('Location services are disabled');
        // Hiển thị dialog yêu cầu bật dịch vụ vị trí có thể được thêm ở đây
        return false;
      }

      // Check location permission
      var permission = await Geolocator.checkPermission();
      if (permission == LocationPermission.denied) {
        // Yêu cầu quyền và kiểm tra kết quả
        permission = await Geolocator.requestPermission();
        if (permission == LocationPermission.denied) {
          debugPrint('Location permission denied by user');
          return false;
        }
      }

      if (permission == LocationPermission.deniedForever) {
        debugPrint('Location permission denied permanently, cannot request again');
        // Ở đây có thể hiển thị dialog hướng dẫn người dùng vào settings để bật quyền
        return false;
      }

      // Request background permission on iOS
      if (Platform.isIOS) {
        final status = await Permission.locationAlways.request();
        debugPrint('iOS background location permission status: $status');
        if (status.isDenied || status.isPermanentlyDenied) {
          debugPrint('iOS background location permission not granted');
          // Có thể thực hiện tracking trong foreground thay vì background
          return permission == LocationPermission.whileInUse || 
                 permission == LocationPermission.always;
        }
      }

      return permission == LocationPermission.whileInUse || 
             permission == LocationPermission.always;
    } catch (e) {
      debugPrint('Error checking location permission: $e');
      return false;
    }
  }

  /// Show foreground notification for Android background service
  /// Required for background location tracking on Android
  Future<void> _showForegroundNotification() async {
    const androidDetails = AndroidNotificationDetails(
      'location_channel',
      'Location Updates',
      channelDescription: 'Used for tracking driver location',
      importance: Importance.low,
      priority: Priority.low,
      ongoing: true,
    );

    const notificationDetails = NotificationDetails(android: androidDetails);

    await _notificationsPlugin.show(
      888,
      'KTC Logistics Driver',
      'Tracking your location in background',
      notificationDetails,
    );
  }

  /// Get platform-specific location settings
  /// Returns the appropriate settings for Android or iOS
  LocationSettings _getPlatformLocationSettings() {
    if (Platform.isAndroid) {
      return AndroidSettings(
        accuracy: LocationAccuracy.high,
        distanceFilter: 10, // Update if moved 10 meters
        foregroundNotificationConfig: const ForegroundNotificationConfig(
          notificationTitle: 'KTC Logistics Driver',
          notificationText: 'Tracking your location',
          enableWakeLock: true,
          notificationIcon: AndroidResource(name: 'ic_notification'),
        ),
      );
    } else {
      return AppleSettings(
        accuracy: LocationAccuracy.high,
        activityType: ActivityType.automotiveNavigation,
        distanceFilter: 10,
        pauseLocationUpdatesAutomatically: false,
        showBackgroundLocationIndicator: true,
      );
    }
  }

  /// Handle position updates from location stream
  void _handlePositionUpdate(Position position) {
    final lat = position.latitude;
    final lng = position.longitude;
    final speed = position.speed;
    final heading = position.heading;

    debugPrint(
        'Position Update: $lat, $lng (speed: ${speed.toStringAsFixed(1)} m/s)');

    _sendLocationToServer(lat, lng, speed, heading);
  }

  /// Start periodic location updates
  /// This is a backup in case continuous tracking fails
  void _startPeriodicLocationUpdates() {
    _locationUpdateTimer?.cancel();
    _sendHourlyLocationUpdate();
    _locationUpdateTimer = Timer.periodic(
        const Duration(hours: 1), (_) => _sendHourlyLocationUpdate());
  }

  /// Send hourly location update
  /// Ensures we have at least hourly updates even if continuous tracking fails
  Future<void> _sendHourlyLocationUpdate() async {
    try {
      final position = await Geolocator.getCurrentPosition(
          desiredAccuracy: LocationAccuracy.high);

      await _sendLocationToServer(position.latitude, position.longitude,
          position.speed, position.heading);

      debugPrint(
          'Hourly update sent: ${position.latitude}, ${position.longitude}');
    } catch (e) {
      debugPrint('Error getting hourly location: $e');
    }
  }

  /// Hiển thị thông báo khi quyền vị trí bị từ chối
  void showLocationPermissionDeniedNotification() {
    // Sử dụng local notifications để hiển thị thông báo
    const androidDetails = AndroidNotificationDetails(
      'permission_channel',
      'Permission Alerts',
      channelDescription: 'Alerts about app permissions',
      importance: Importance.high,
      priority: Priority.high,
    );
    
    const notificationDetails = NotificationDetails(android: androidDetails);
    
    _notificationsPlugin.show(
      999,
      'Cần quyền truy cập vị trí',
      'Ứng dụng KTC Logistics cần quyền truy cập vị trí để ghi nhận lộ trình giao hàng. Vui lòng vào Cài đặt để bật quyền.',
      notificationDetails,
    );
  }

  /// Starts tracking for a specific delivery
  /// This method incorporates functionality from the previous driver_tracking_service
  Future<void> startDeliveryTracking({
    required int driverId,
    required int deliveryId,
    required int vehicleId,
    required int statusId,
  }) async {
    // Kiểm tra quyền vị trí trước
    if (!await _checkLocationPermission()) {
      debugPrint('Location permission denied, will continue with limited functionality');
      showLocationPermissionDeniedNotification();
    }
    
    // Lưu thông tin tracking vào SharedPreferences
    final prefs = await SharedPreferences.getInstance();
    await prefs.setInt(_activeDriverIdKey, driverId);
    await prefs.setInt(_activeDeliveryIdKey, deliveryId);
    await prefs.setInt(_activeVehicleIdKey, vehicleId);
    await prefs.setInt(_activeStatusIdKey, statusId);
    await prefs.setString(_lastTrackingTimeKey, DateTime.now().toIso8601String());
    
    if (_isTracking) {
      stopBackgroundLocationService();
    }
    
    // Get current position
    Position position;
    try {
      position = await Geolocator.getCurrentPosition(
          desiredAccuracy: LocationAccuracy.high);
    } catch (e) {
      debugPrint('Error getting position: $e');
      // Fallback to default position if permission denied or other error
      position = Position(
        longitude: 0,
        latitude: 0,
        timestamp: DateTime.now(),
        accuracy: 0,
        altitude: 0,
        altitudeAccuracy: 0,
        heading: 0,
        headingAccuracy: 0,
        speed: 0,
        speedAccuracy: 0,
      );
    }
    
    // Start general tracking
    try {
      await startTracking(
        latitude: position.latitude, 
        longitude: position.longitude,
        speed: position.speed,
        heading: position.heading,
        vehicleStatus: 'ACTIVE',
        updateInterval: const Duration(minutes: 30) // 30 minute interval as requested
      );
    } catch (e) {
      debugPrint('Failed to start tracking: $e');
      // Continue anyway to try to update delivery tracking once
    }
    
    // Send tracking data specifically for this delivery
    try {
      await updateDeliveryTracking(
        deliveryId, 
        DriverLocation(
          latitude: position.latitude,
          longitude: position.longitude,
          speed: position.speed,
          heading: position.heading,
          timestamp: DateTime.now().toIso8601String(),
          vehicleStatus: 'ACTIVE'
        ),
        statusId: statusId
      );
    } catch (e) {
      debugPrint('Failed to update delivery tracking: $e');
    }
    
    debugPrint('🚀 Started driver location tracking for Delivery #$deliveryId');
  }
  
  /// Restore tracking from SharedPreferences after app restart
  Future<void> restoreDeliveryTrackingIfNeeded() async {
    final prefs = await SharedPreferences.getInstance();
    final lastTrackingTime = prefs.getString(_lastTrackingTimeKey);
    final deliveryId = prefs.getInt(_activeDeliveryIdKey);
    final driverId = prefs.getInt(_activeDriverIdKey);
    final vehicleId = prefs.getInt(_activeVehicleIdKey);
    final statusId = prefs.getInt(_activeStatusIdKey);
    
    // Nếu có thông tin tracking đã lưu
    if (lastTrackingTime != null && deliveryId != null && 
        driverId != null && vehicleId != null && statusId != null) {
      
      // Khôi phục tracking
      startDeliveryTracking(
        driverId: driverId,
        deliveryId: deliveryId, 
        vehicleId: vehicleId,
        statusId: statusId
      );
      
      debugPrint('♻️ Restored driver location tracking for Delivery #$deliveryId');
    }
  }
  
  /// Lấy vehicle ID đã lưu từ SharedPreferences
  /// 
  /// Returns saved vehicle ID or null if not found
  Future<int?> getSavedVehicleId() async {
    final prefs = await SharedPreferences.getInstance();
    return prefs.getInt(_activeVehicleIdKey);
  }
  
  /// Lấy địa chỉ từ vị trí (từ location_service)
  Future<Placemark?> getAddressFromPosition(Position position) async {
    try {
      final placemarks = await placemarkFromCoordinates(
        position.latitude,
        position.longitude,
      );

      if (placemarks.isNotEmpty) {
        return placemarks.first;
      }
      return null;
    } catch (e) {
      debugPrint('Error getting address from position: $e');
      return null;
    }
  }
}

/// Singleton instance for app-wide use
final locationService = LocationService();
