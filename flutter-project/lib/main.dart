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
import 'services/push_notification_service.dart';

// Services
import 'services/mapbox_services.dart' as mapbox;
import 'services/tracking_service.dart';

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
  
  // Tải token từ file bí mật
  try {
    final accessToken = await Secrets.getMapboxAccessToken();
    
    if (accessToken.isEmpty) {
      print('❌ Mapbox access token not found in secrets file!');
    } else {
      MapboxOptions.setAccessToken(accessToken);
      print('🗺️ Mapbox initialized with private token: ${accessToken.substring(0, 12)}...');
      
      // Đảm bảo MapboxDirectionsService cũng sử dụng token này
      mapbox.MapboxDirectionsService.setAccessToken(accessToken);
    }
  } catch (e) {
    print('❌ Failed to initialize Mapbox: $e');
  }
  
  try {
    // Initialize Firebase
    print('🔥 Initializing Firebase...');
    await Firebase.initializeApp(options: DefaultFirebaseOptions.currentPlatform);
    
    // Setup background message handler
    FirebaseMessaging.onBackgroundMessage(_firebaseMessagingBackgroundHandler);
    
    // Initialize Push Notifications
    pushNotificationService.initNotification();
    
    print('✅ Firebase initialization completed');
    
  } catch (e) {
    print('❌ Firebase initialization failed: $e');
    // Continue without Firebase for now - you might want to handle this differently
  }
  
  // Setup dependency injection
  await setupDependencyInjection();
  
  // Initialize the LocationService for background location tracking
  try {
    print('📍 Initializing Location Service...');
    await LocationService().initialize();
    print('✅ Location Service initialized');
  } catch (e) {
    print('❌ Location Service initialization failed: $e');
  }
  
  runApp(const App());
}