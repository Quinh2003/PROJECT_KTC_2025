// socket_service.dart
// Service để kết nối WebSocket với Spring Boot backend để nhận thông báo realtime

import 'dart:convert';
import 'package:socket_io_client/socket_io_client.dart' as IO;
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/domain/models/notification_model.dart';
import 'package:ktc_logistics_driver/domain/models/order_model.dart';
import 'package:ktc_logistics_driver/domain/models/driver_status_model.dart';

class SocketService {
  // Instance của Socket.IO client
  late IO.Socket _socket;
  
  // Instance của FlutterSecureStorage để lấy token
  final _storage = const FlutterSecureStorage();

  // Environment instance
  final _env = Environment.getInstance();

  // Singleton pattern cho SocketService
  static final SocketService _instance = SocketService._internal();
  factory SocketService() => _instance;
  SocketService._internal();
  
  // Flag để kiểm tra socket đã kết nối chưa
  bool _isConnected = false;
  
  // Getter để kiểm tra trạng thái kết nối
  bool get isConnected => _isConnected;
  
  // Getter để truy cập socket instance
  IO.Socket get socket => _socket;
  
  // Kết nối tới WebSocket server
  Future<void> connect() async {
    // Nếu đã kết nối rồi thì không cần kết nối lại
    if (_isConnected) return;
    
    // Lấy token từ secure storage
    final token = await _storage.read(key: 'token');
    final uid = await _storage.read(key: 'uid');
    
    if (token == null || uid == null) {
      throw Exception('Not authenticated. Cannot connect to socket.');
    }
    
    try {
      // Khởi tạo kết nối socket
      _socket = IO.io(
        _env.socketUrl,
        IO.OptionBuilder()
            .setTransports(['websocket'])
            .disableAutoConnect()
            .setQuery({'token': token, 'driverId': uid})
            .setExtraHeaders({'Authorization': 'Bearer $token'})
            .build(),
      );
      
      // Kết nối tới server
      _socket.connect();
      
      // Đăng ký các event listeners
      _registerEventHandlers();
      
      // Cập nhật trạng thái kết nối
      _socket.onConnect((_) {
        _isConnected = true;
        print('Socket connected');
      });
      
      _socket.onDisconnect((_) {
        _isConnected = false;
        print('Socket disconnected');
      });
      
      _socket.onConnectError((error) {
        print('Socket connection error: $error');
      });
      
      _socket.onError((error) {
        print('Socket error: $error');
      });
      
    } catch (e) {
      print('Socket connection failed: $e');
      _isConnected = false;
      rethrow;
    }
  }
  
  // Ngắt kết nối WebSocket
  void disconnect() {
    if (_isConnected) {
      _socket.disconnect();
      _isConnected = false;
    }
  }
  
  // Đăng ký các event listeners
  void _registerEventHandlers() {
    // Khi nhận được đơn hàng mới
    _socket.on(_env.newOrderEvent, (data) {
      // Xử lý sự kiện đơn hàng mới
      if (data != null) {
        try {
          final Map<String, dynamic> orderData = jsonDecode(data);
          final order = Order.fromJson(orderData);
          
          // Gọi hàm callback nếu được đăng ký
          if (_onNewOrderCallback != null) {
            _onNewOrderCallback!(order);
          }
        } catch (e) {
          print('Error parsing new order data: $e');
        }
      }
    });
    
    // Khi đơn hàng được cập nhật
    _socket.on(_env.orderUpdatedEvent, (data) {
      // Xử lý sự kiện đơn hàng cập nhật
      if (data != null) {
        try {
          final Map<String, dynamic> orderData = jsonDecode(data);
          final order = Order.fromJson(orderData);
          
          // Gọi hàm callback nếu được đăng ký
          if (_onOrderUpdatedCallback != null) {
            _onOrderUpdatedCallback!(order);
          }
        } catch (e) {
          print('Error parsing updated order data: $e');
        }
      }
    });
    
    // Khi đơn hàng bị hủy
    _socket.on(_env.orderCancelledEvent, (data) {
      // Xử lý sự kiện đơn hàng bị hủy
      if (data != null) {
        try {
          final Map<String, dynamic> cancelData = jsonDecode(data);
          final String orderId = cancelData['orderId'] ?? '';
          final String reason = cancelData['reason'] ?? '';
          
          // Gọi hàm callback nếu được đăng ký
          if (_onOrderCancelledCallback != null) {
            _onOrderCancelledCallback!(orderId, reason);
          }
        } catch (e) {
          print('Error parsing cancelled order data: $e');
        }
      }
    });
  }
  
  // Emit vị trí tài xế lên server
  void emitDriverLocation(double latitude, double longitude) {
    if (_isConnected) {
      _socket.emit(_env.locationUpdateEvent, {
        'latitude': latitude,
        'longitude': longitude,
        'timestamp': DateTime.now().toIso8601String(),
      });
    }
  }
  
  // Emit trạng thái tài xế lên server
  void emitDriverStatus(DriverStatus status) {
    if (_isConnected) {
      _socket.emit(_env.statusUpdateEvent, status.toJson());
    }
  }
  
  // Join vào room của đơn hàng để nhận cập nhật
  void joinOrderRoom(String orderId) {
    if (_isConnected) {
      _socket.emit('join_order_room', {'orderId': orderId});
    }
  }
  
  // Leave room của đơn hàng
  void leaveOrderRoom(String orderId) {
    if (_isConnected) {
      _socket.emit('leave_order_room', {'orderId': orderId});
    }
  }
  
  // Callback functions
  Function(Order order)? _onNewOrderCallback;
  Function(Order order)? _onOrderUpdatedCallback;
  Function(String orderId, String reason)? _onOrderCancelledCallback;
  
  // Đăng ký callbacks
  void onNewOrder(Function(Order order) callback) {
    _onNewOrderCallback = callback;
  }
  
  void onOrderUpdated(Function(Order order) callback) {
    _onOrderUpdatedCallback = callback;
  }
  
  void onOrderCancelled(Function(String orderId, String reason) callback) {
    _onOrderCancelledCallback = callback;
  }
  
  void onNewNotification(Function(Notification notification) callback) {
    // Đăng ký event listener cho thông báo mới
    _socket.on('new_notification', (data) {
      if (data != null) {
        try {
          final Map<String, dynamic> notificationData = jsonDecode(data);
          final notification = Notification.fromJson(notificationData);
          callback(notification);
        } catch (e) {
          print('Error parsing notification data: $e');
        }
      }
    });
  }
}


