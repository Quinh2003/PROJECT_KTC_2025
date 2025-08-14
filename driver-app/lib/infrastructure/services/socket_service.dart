import 'dart:async';
import 'dart:convert';
import 'package:web_socket_channel/web_socket_channel.dart';
import 'package:flutter/foundation.dart';

import '../models/socket_message.dart';

class SocketService {
  final String baseUrl;
  WebSocketChannel? _channel;
  StreamController<SocketMessage> _messageController = StreamController.broadcast();
  bool _isConnected = false;
  String? _authToken;
  Timer? _pingTimer;
  int _reconnectAttempts = 0;
  final int _maxReconnectAttempts = 5;
  final Duration _reconnectDelay = Duration(seconds: 3);
  
  SocketService({required this.baseUrl});
  
  bool get isConnected => _isConnected;
  Stream<SocketMessage> get messageStream => _messageController.stream;
  
  Future<bool> connect({String? authToken}) async {
    if (_isConnected) return true;
    
    _authToken = authToken;
    
    try {
      String url = baseUrl;
      if (_authToken != null && _authToken!.isNotEmpty) {
        url += '?token=$_authToken';
      }
      
      _channel = WebSocketChannel.connect(Uri.parse(url));
      
      _channel!.stream.listen(
        _onMessage,
        onError: _onError,
        onDone: _onDisconnect,
      );
      
      _isConnected = true;
      _reconnectAttempts = 0;
      _startPingTimer();
      
      return true;
    } catch (e) {
      debugPrint('Socket connect error: $e');
      _isConnected = false;
      return false;
    }
  }
  
  void _onMessage(dynamic message) {
    try {
      final decodedMessage = jsonDecode(message);
      final socketMessage = SocketMessage.fromJson(decodedMessage);
      _messageController.add(socketMessage);
      
      // Handle special message types
      if (socketMessage.type == 'pong') {
        // Reset ping timer
        _startPingTimer();
      }
    } catch (e) {
      debugPrint('Error parsing socket message: $e');
    }
  }
  
  void _onError(dynamic error) {
    debugPrint('Socket error: $error');
    _isConnected = false;
    _tryReconnect();
  }
  
  void _onDisconnect() {
    debugPrint('Socket disconnected');
    _isConnected = false;
    _pingTimer?.cancel();
    _tryReconnect();
  }
  
  void _tryReconnect() {
    if (_reconnectAttempts >= _maxReconnectAttempts) {
      debugPrint('Max reconnect attempts reached');
      return;
    }
    
    _reconnectAttempts++;
    
    Future.delayed(_reconnectDelay, () {
      debugPrint('Trying to reconnect (Attempt $_reconnectAttempts/$_maxReconnectAttempts)');
      connect(authToken: _authToken);
    });
  }
  
  Future<void> disconnect() async {
    _pingTimer?.cancel();
    await _channel?.sink.close();
    _isConnected = false;
  }
  
  void _startPingTimer() {
    _pingTimer?.cancel();
    _pingTimer = Timer.periodic(const Duration(seconds: 30), (timer) {
      sendMessage('ping', {});
    });
  }
  
  void sendMessage(String type, Map<String, dynamic> data) {
    if (!_isConnected) {
      debugPrint('Socket not connected, cannot send message');
      return;
    }
    
    final message = SocketMessage(
      type: type,
      data: data,
      timestamp: DateTime.now(),
    );
    
    _channel?.sink.add(jsonEncode(message.toJson()));
  }
  
  void updateToken(String? token) {
    if (_authToken != token) {
      _authToken = token;
      if (_isConnected) {
        // Reconnect with new token
        disconnect().then((_) => connect(authToken: token));
      }
    }
  }
  
  void subscribeToDriver(String driverId) {
    sendMessage('subscribe', {'topic': 'driver', 'driverId': driverId});
  }
  
  void subscribeToVehicle(String vehicleId) {
    sendMessage('subscribe', {'topic': 'vehicle', 'vehicleId': vehicleId});
  }
  
  void subscribeToRoute(String routeId) {
    sendMessage('subscribe', {'topic': 'route', 'routeId': routeId});
  }
  
  void subscribeToOrder(String orderId) {
    sendMessage('subscribe', {'topic': 'order', 'orderId': orderId});
  }
  
  void unsubscribeFromDriver(String driverId) {
    sendMessage('unsubscribe', {'topic': 'driver', 'driverId': driverId});
  }
  
  void unsubscribeFromVehicle(String vehicleId) {
    sendMessage('unsubscribe', {'topic': 'vehicle', 'vehicleId': vehicleId});
  }
  
  void unsubscribeFromRoute(String routeId) {
    sendMessage('unsubscribe', {'topic': 'route', 'routeId': routeId});
  }
  
  void unsubscribeFromOrder(String orderId) {
    sendMessage('unsubscribe', {'topic': 'order', 'orderId': orderId});
  }
  
  void updateDriverStatus(String driverId, String status) {
    sendMessage('driver_status', {'driverId': driverId, 'status': status});
  }
  
  void updateDeliveryStatus(String deliveryId, String status) {
    sendMessage('delivery_status', {'deliveryId': deliveryId, 'status': status});
  }
  
  void broadcastLocation(double latitude, double longitude, {
    String? driverId,
    String? vehicleId,
    double? speed,
    double? heading,
  }) {
    sendMessage('location_update', {
      'latitude': latitude,
      'longitude': longitude,
      'driverId': driverId,
      'vehicleId': vehicleId,
      'timestamp': DateTime.now().toIso8601String(),
      'speed': speed,
      'heading': heading,
    });
  }
  
  void dispose() {
    disconnect();
    _messageController.close();
  }
}


