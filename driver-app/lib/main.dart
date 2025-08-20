import 'package:flutter/material.dart';
import 'package:firebase_core/firebase_core.dart';
import 'package:firebase_messaging/firebase_messaging.dart';

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