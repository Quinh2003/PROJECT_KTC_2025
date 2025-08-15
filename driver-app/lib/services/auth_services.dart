// auth_services.dart
// Service để xử lý authentication với Spring Boot backend

import '../services/api_service.dart';
import '../domain/models/response/auth_response.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

class AuthServices {
  // Instance của ApiService để gọi API
  final _apiService = ApiService();
  
  // Instance của FlutterSecureStorage để lưu/đọc token
  final _secureStorage = const FlutterSecureStorage();

  // Singleton pattern
  static final AuthServices _instance = AuthServices._internal();
  factory AuthServices() => _instance;
  AuthServices._internal();

  // Đăng nhập với email và password
  Future<AuthResponse> loginController(String email, String password) async {
    return await _apiService.login(email, password);
  }

  // Làm mới token (renew token)
  Future<AuthResponse> renewLoginController() async {
    // Kiểm tra token có hợp lệ không
    final isValid = await _apiService.checkTokenValidity();
    
    if (!isValid) {
      throw Exception('Token invalid or expired');
    }
    
    // Lấy thông tin profile để tự động đăng nhập lại
    return await _apiService.getDriverProfile();
  }
  
  // Kiểm tra đã đăng nhập chưa
  Future<bool> isLoggedIn() async {
    final token = await _secureStorage.read(key: 'token');
    if (token == null) return false;
    
    return await _apiService.checkTokenValidity();
  }
  
  // Đăng xuất
  Future<void> logout() async {
    await _apiService.logout();
  }
}

// Instance global của AuthServices
final authServices = AuthServices();
