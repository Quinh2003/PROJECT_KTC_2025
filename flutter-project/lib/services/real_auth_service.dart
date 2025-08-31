import 'dart:convert';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:http/http.dart' as http;
import '../data/env/environment.dart';
import '../domain/models/auth/auth_response.dart';

/// Service xử lý xác thực thông qua Spring Boot backend thay vì sử dụng mock
class RealAuthService {
  static final RealAuthService _instance = RealAuthService._internal();
  static RealAuthService getInstance() => _instance;
  
  final FlutterSecureStorage _secureStorage = const FlutterSecureStorage();
  final Environment _env = Environment.getInstance();
  
  Map<String, dynamic>? _currentUser;
  String? _authToken;
  
  RealAuthService._internal();

  Map<String, dynamic>? get currentUser => _currentUser;
  bool get isLoggedIn => _authToken != null && _currentUser != null;
  String? get token => _authToken;

  /// Đăng nhập với API thực của Spring Boot
  Future<Map<String, dynamic>?> login(String email, String password) async {
    try {
      final response = await http.post(
        Uri.parse('${_env.apiBaseUrl}/auth/login'),
        headers: {
          'Content-Type': 'application/json',
          'Accept': 'application/json'
        },
        body: jsonEncode({
          'email': email,
          'password': password,
        }),
      );

      if (response.statusCode == 200) {
        final Map<String, dynamic> data = jsonDecode(response.body);
        
        // Lưu token vào secure storage
        _authToken = data['token'];
        await _secureStorage.write(key: 'auth_token', value: _authToken);
        
        // Lấy thông tin người dùng
        await _loadUserProfile();
        
        return _currentUser;
      } else {
        final error = jsonDecode(response.body);
        throw Exception(error['message'] ?? 'Đăng nhập thất bại');
      }
    } catch (e) {
      throw Exception('Lỗi đăng nhập: ${e.toString()}');
    }
  }

  /// Lấy thông tin người dùng từ token
  Future<Map<String, dynamic>?> _loadUserProfile() async {
    if (_authToken == null) {
      return null;
    }

    try {
      final response = await http.get(
        Uri.parse('${_env.apiBaseUrl}/auth/me'),
        headers: {
          'Content-Type': 'application/json',
          'Accept': 'application/json',
          'Authorization': 'Bearer $_authToken',
        },
      );

      if (response.statusCode == 200) {
        _currentUser = jsonDecode(response.body);
        return _currentUser;
      } else {
        throw Exception('Không thể lấy thông tin người dùng');
      }
    } catch (e) {
      throw Exception('Lỗi lấy thông tin người dùng: ${e.toString()}');
    }
  }

  /// Kiểm tra trạng thái đăng nhập và khôi phục session nếu có
  Future<Map<String, dynamic>?> checkLoginStatus() async {
    try {
      _authToken = await _secureStorage.read(key: 'auth_token');
      
      if (_authToken != null) {
        return await _loadUserProfile();
      }
      return null;
    } catch (e) {
      await logout(); // Reset session nếu có lỗi
      return null;
    }
  }

  /// Đăng xuất
  Future<void> logout() async {
    _authToken = null;
    _currentUser = null;
    await _secureStorage.delete(key: 'auth_token');
  }
}
