// service_index.dart
// Cung cấp trung tâm truy cập duy nhất cho tất cả các services

import 'package:ktc_logistics_driver/data/services/unified_api_service.dart';
import 'package:ktc_logistics_driver/data/services/socket_service.dart';

/// Service Locator class để tập trung tất cả services vào một chỗ
/// Theo pattern Service Locator đơn giản
class ServiceLocator {
  static final ServiceLocator _instance = ServiceLocator._internal();
  factory ServiceLocator() => _instance;
  ServiceLocator._internal();

  // Các services
  final apiService = ApiService();
  final socketService = SocketService();
  
  // Có thể mở rộng thêm các services khác ở đây
}

/// Instance global của ServiceLocator
final serviceLocator = ServiceLocator();

/// Hàm tiện ích để truy cập nhanh vào ApiService
ApiService get apiService => serviceLocator.apiService;

/// Hàm tiện ích để truy cập nhanh vào SocketService
SocketService get socketService => serviceLocator.socketService;


