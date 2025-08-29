import 'dart:async';
import 'dart:io';
import 'package:flutter/foundation.dart';
import 'package:geolocator/geolocator.dart';
import 'package:flutter_local_notifications/flutter_local_notifications.dart';
import 'package:http/http.dart' as http;
import 'package:permission_handler/permission_handler.dart';

class LocationService {
  static final LocationService _instance = LocationService._internal();
  factory LocationService() => _instance;
  LocationService._internal();

  StreamSubscription<Position>? _positionStreamSubscription;
  Timer? _locationUpdateTimer;
  final FlutterLocalNotificationsPlugin _notificationsPlugin = FlutterLocalNotificationsPlugin();

  // This would be your API endpoint for location updates
  final String _locationUpdateEndpoint = 'https://api.ktc-logistics.com/api/driver/location';
  
  // Mock API call - replace with actual implementation
  Future<void> _sendLocationToServer(double latitude, double longitude) async {
    try {
      // This is a mock implementation. Replace with your actual API call.
      if (kDebugMode) {
        print('Sending location to server: Lat: $latitude, Long: $longitude');
      }
      
      // Uncomment and modify this when you have a real API endpoint
      // final response = await http.post(
      //   Uri.parse(_locationUpdateEndpoint),
      //   headers: {'Content-Type': 'application/json'},
      //   body: jsonEncode({
      //     'latitude': latitude,
      //     'longitude': longitude,
      //     'timestamp': DateTime.now().toIso8601String(),
      //     'driverId': 'YOUR_DRIVER_ID', // Get this from your auth service
      //   }),
      // );
      // 
      // if (response.statusCode != 200) {
      //   throw Exception('Failed to update location on server');
      // }
    } catch (e) {
      debugPrint('Error sending location to server: $e');
    }
  }

  Future<void> initialize() async {
    // Initialize notifications
    const AndroidInitializationSettings androidSettings = 
        AndroidInitializationSettings('@mipmap/ic_launcher');
    
    final DarwinInitializationSettings iosSettings = DarwinInitializationSettings(
      requestAlertPermission: false,
      requestBadgePermission: false,
      requestSoundPermission: false,
    );
    
    final InitializationSettings initSettings = InitializationSettings(
      android: androidSettings,
      iOS: iosSettings,
    );
    
    await _notificationsPlugin.initialize(initSettings);
  }

  Future<bool> _checkLocationPermission() async {
    bool serviceEnabled = await Geolocator.isLocationServiceEnabled();
    if (!serviceEnabled) {
      return false;
    }

    LocationPermission permission = await Geolocator.checkPermission();
    if (permission == LocationPermission.denied) {
      permission = await Geolocator.requestPermission();
      if (permission == LocationPermission.denied) {
        return false;
      }
    }
    
    if (permission == LocationPermission.deniedForever) {
      return false;
    }

    // For background location on iOS
    if (Platform.isIOS) {
      await Permission.locationAlways.request();
    }

    return true;
  }

  // Start background location tracking
  Future<void> startBackgroundLocationService() async {
    if (!await _checkLocationPermission()) {
      debugPrint('Location permission not granted');
      return;
    }

    // Start foreground notification (required for Android background service)
    if (Platform.isAndroid) {
      const AndroidNotificationDetails androidDetails = AndroidNotificationDetails(
        'location_channel',
        'Location Updates',
        channelDescription: 'Used for tracking driver location',
        importance: Importance.low,
        priority: Priority.low,
        ongoing: true,
      );
      
      const NotificationDetails notificationDetails = NotificationDetails(
        android: androidDetails,
      );
      
      await _notificationsPlugin.show(
        888,
        'KTC Logistics Driver',
        'Tracking your location in background',
        notificationDetails,
      );
    }

    // Configure location settings
    LocationSettings locationSettings = Platform.isAndroid
        ? AndroidSettings(
            accuracy: LocationAccuracy.high,
            distanceFilter: 10,
            foregroundNotificationConfig: const ForegroundNotificationConfig(
              notificationTitle: 'KTC Logistics Driver',
              notificationText: 'Tracking your location',
              enableWakeLock: true,
              notificationIcon: AndroidResource(name: 'ic_notification'),
            ),
          )
        : AppleSettings(
            accuracy: LocationAccuracy.high,
            activityType: ActivityType.automotiveNavigation,
            distanceFilter: 10,
            pauseLocationUpdatesAutomatically: false,
            showBackgroundLocationIndicator: true,
          );

    // Start the continuous location stream for immediate updates
    _positionStreamSubscription = Geolocator.getPositionStream(
      locationSettings: locationSettings,
    ).listen(
      (Position position) {
        // This will get continuous updates as the driver moves
        debugPrint('Position Update: ${position.latitude}, ${position.longitude}');
      },
      onError: (error) {
        debugPrint('Error getting location: $error');
      },
    );

    // Start hourly location updates
    _startHourlyLocationUpdates();
  }

  // Hourly location updates to the server
  void _startHourlyLocationUpdates() {
    // Cancel any existing timer
    _locationUpdateTimer?.cancel();
    
    // Initial update
    _sendHourlyLocationUpdate();
    
    // Schedule hourly updates (3600000 milliseconds = 1 hour)
    _locationUpdateTimer = Timer.periodic(
      const Duration(hours: 1), 
      (_) => _sendHourlyLocationUpdate()
    );
  }

  Future<void> _sendHourlyLocationUpdate() async {
    try {
      final position = await Geolocator.getCurrentPosition(
        desiredAccuracy: LocationAccuracy.high
      );
      
      await _sendLocationToServer(position.latitude, position.longitude);
      
      debugPrint('Hourly location update sent: ${position.latitude}, ${position.longitude}');
    } catch (e) {
      debugPrint('Error getting hourly location: $e');
    }
  }

  // Stop the background location service
  Future<void> stopBackgroundLocationService() async {
    await _positionStreamSubscription?.cancel();
    _positionStreamSubscription = null;
    
    _locationUpdateTimer?.cancel();
    _locationUpdateTimer = null;
    
    if (Platform.isAndroid) {
      await _notificationsPlugin.cancel(888);
    }
    
    debugPrint('Background location service stopped');
  }
}
