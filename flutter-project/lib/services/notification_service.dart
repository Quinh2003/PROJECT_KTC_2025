import 'package:flutter_local_notifications/flutter_local_notifications.dart';
import 'package:flutter/foundation.dart';

/// Notification Service to replace Firebase Cloud Messaging
class NotificationService {
  static final NotificationService _instance = NotificationService._internal();
  factory NotificationService() => _instance;
  NotificationService._internal();

  final FlutterLocalNotificationsPlugin _flutterLocalNotificationsPlugin =
      FlutterLocalNotificationsPlugin();

  Future<void> initialize() async {
    const AndroidInitializationSettings initializationSettingsAndroid =
        AndroidInitializationSettings('@mipmap/ic_launcher');

    const DarwinInitializationSettings initializationSettingsIOS =
        DarwinInitializationSettings(
      requestSoundPermission: true,
      requestBadgePermission: true,
      requestAlertPermission: true,
    );

    const InitializationSettings initializationSettings =
        InitializationSettings(
      android: initializationSettingsAndroid,
      iOS: initializationSettingsIOS,
    );

    await _flutterLocalNotificationsPlugin.initialize(
      initializationSettings,
      onDidReceiveNotificationResponse: _onDidReceiveNotificationResponse,
    );

    // Request permissions for iOS
    await _requestPermissions();
  }

  Future<void> _requestPermissions() async {
    await _flutterLocalNotificationsPlugin
        .resolvePlatformSpecificImplementation<
            IOSFlutterLocalNotificationsPlugin>()
        ?.requestPermissions(
          alert: true,
          badge: true,
          sound: true,
        );

    await _flutterLocalNotificationsPlugin
        .resolvePlatformSpecificImplementation<
            AndroidFlutterLocalNotificationsPlugin>()
        ?.requestNotificationsPermission();
  }

  void _onDidReceiveNotificationResponse(NotificationResponse response) {
    if (kDebugMode) {
      print('Notification received: ${response.payload}');
    }
    // Handle notification tap
  }

  Future<void> showNotification({
    required int id,
    required String title,
    required String body,
    String? payload,
    Priority priority = Priority.defaultPriority,
  }) async {
    AndroidNotificationDetails androidPlatformChannelSpecifics =
        AndroidNotificationDetails(
      'ktc_logistics_driver',
      'KTC Logistics Driver',
      channelDescription: 'Notifications for KTC Logistics Driver App',
      importance: Importance.max,
      priority: Priority.high,
      ticker: 'ticker',
      icon: '@mipmap/ic_launcher',
    );

    const DarwinNotificationDetails iOSPlatformChannelSpecifics =
        DarwinNotificationDetails(
      presentAlert: true,
      presentBadge: true,
      presentSound: true,
    );

    NotificationDetails platformChannelSpecifics = NotificationDetails(
      android: androidPlatformChannelSpecifics,
      iOS: iOSPlatformChannelSpecifics,
    );

    await _flutterLocalNotificationsPlugin.show(
      id,
      title,
      body,
      platformChannelSpecifics,
      payload: payload,
    );
  }

  // Delivery related notifications
  Future<void> showDeliveryNotification({
    required String orderId,
    required String status,
    required String message,
  }) async {
    await showNotification(
      id: orderId.hashCode,
      title: 'Cập nhật đơn hàng #$orderId',
      body: message,
      payload: 'delivery_$orderId',
    );
  }

  // Route notifications
  Future<void> showRouteNotification({
    required String title,
    required String message,
  }) async {
    await showNotification(
      id: DateTime.now().millisecondsSinceEpoch ~/ 1000,
      title: title,
      body: message,
      payload: 'route_update',
    );
  }

  // Emergency notifications
  Future<void> showEmergencyNotification({
    required String title,
    required String message,
  }) async {
    await showNotification(
      id: 9999,
      title: title,
      body: message,
      priority: Priority.max,
      payload: 'emergency',
    );
  }

  Future<void> cancelNotification(int id) async {
    await _flutterLocalNotificationsPlugin.cancel(id);
  }

  Future<void> cancelAllNotifications() async {
    await _flutterLocalNotificationsPlugin.cancelAll();
  }
}


