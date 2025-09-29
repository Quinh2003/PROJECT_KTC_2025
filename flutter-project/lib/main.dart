import 'package:flutter/material.dart';
import 'package:firebase_core/firebase_core.dart';
import 'package:firebase_messaging/firebase_messaging.dart';
import 'package:mapbox_maps_flutter/mapbox_maps_flutter.dart';

// App
import 'app.dart';

// Dependency Injection
import 'injection/dependency_injection.dart';

// Firebase
import 'firebase_options.dart';
import 'services/push_notification_services.dart';

// Services
import 'services/mapbox_services.dart' as mapbox;
import 'services/tracking_services.dart';

// Environment & Secrets
import 'data/env/secrets.dart';

PushNotificationService pushNotificationService = PushNotificationService();

/// Background message handler - must be top-level function
Future<void> _firebaseMessagingBackgroundHandler(RemoteMessage message) async {
  await Firebase.initializeApp(options: DefaultFirebaseOptions.currentPlatform);
  print('📱 Background message received: ${message.messageId}');
}

void main() async {
  WidgetsFlutterBinding.ensureInitialized();
  
  // Initialize core services in parallel for better performance
  await _initializeApp();
  
  runApp(const App());
}

/// Initialize app services in parallel for better startup performance
Future<void> _initializeApp() async {
  final futures = <Future>[
    _initializeMapbox(),
    _initializeFirebase(),
    setupDependencyInjection(),
  ];
  
  // Wait for all core services
  await Future.wait(futures, eagerError: false);
  
  // Initialize location service after dependency injection
  await _initializeLocationService();
}

/// Initialize Mapbox with proper error handling
Future<void> _initializeMapbox() async {
  try {
    final accessToken = await Secrets.getMapboxAccessToken();
    
    if (accessToken.isEmpty) {
      print('❌ Mapbox access token not found in secrets file!');
      return;
    }
    
    MapboxOptions.setAccessToken(accessToken);
    mapbox.MapboxDirectionsService.setAccessToken(accessToken);
    print('🗺️ Mapbox initialized successfully');
  } catch (e) {
    print('❌ Failed to initialize Mapbox: $e');
  }
}

/// Initialize Firebase with proper error handling
Future<void> _initializeFirebase() async {
  try {
    print('🔥 Initializing Firebase...');
    await Firebase.initializeApp(options: DefaultFirebaseOptions.currentPlatform);
    
    // Setup background message handler
    FirebaseMessaging.onBackgroundMessage(_firebaseMessagingBackgroundHandler);
    
    // Initialize Push Notifications
    pushNotificationService.initNotification();
    
    print('✅ Firebase initialization completed');
  } catch (e) {
    print('❌ Firebase initialization failed: $e');
  }
}

/// Initialize location service with proper error handling
Future<void> _initializeLocationService() async {
  try {
    print('📍 Initializing Location Service...');
    await LocationService().initialize();
    
    // Restore tracking service if needed
    await locationService.restoreDeliveryTrackingIfNeeded();
    print('✅ Location Service initialized');
  } catch (e) {
    print('❌ Location Service initialization failed: $e');
  }
}