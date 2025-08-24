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

PushNotificationService pushNotificationService = PushNotificationService();

/// Background message handler - must be top-level function
Future<void> _firebaseMessagingBackgroundHandler(RemoteMessage message) async {
  await Firebase.initializeApp(options: DefaultFirebaseOptions.currentPlatform);
  print('📱 Background message received: ${message.messageId}');
}

void main() async {
  WidgetsFlutterBinding.ensureInitialized();
  
  // Initialize Mapbox with access token from environment
  const String accessToken = String.fromEnvironment('ACCESS_TOKEN', 
    defaultValue: 'pk.eyJ1IjoiaHVuZ3BxMyIsImEiOiJjbHR3M3JzdXQwYzE5MnFteDFjYXRlcDEzIn0.GDrXTFKq1wn-FZSiTGrfew');
  
  if (accessToken.isEmpty || accessToken == 'YOUR_MAPBOX_ACCESS_TOKEN') {
    print('❌ Mapbox access token not found! Please run with --dart-define ACCESS_TOKEN=your_token');
  } else {
    MapboxOptions.setAccessToken(accessToken);
    print('🗺️ Mapbox initialized with token');
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
  
  runApp(const App());
}