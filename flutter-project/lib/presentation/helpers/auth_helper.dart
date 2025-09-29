import 'package:flutter/foundation.dart';
import '../../services/auth_services.dart';
import '../../services/mock_auth_service.dart';

/// Lớp helper để xử lý chuyển đổi giữa mock auth và real auth
/// Sử dụng real auth khi có thể, fallback về mock auth khi không có kết nối
class AuthHelper {
  static final AuthHelper _instance = AuthHelper._internal();
  static AuthHelper getInstance() => _instance;
  
  final MockAuthService _mockAuth = MockAuthService();
  final AuthServices _realAuth = AuthServices();
  
  bool _useRealAuth = true;
  bool get useRealAuth => _useRealAuth;
  
  AuthHelper._internal();

  // Thiết lập sử dụng real auth hoặc mock auth
  void setUseRealAuth(bool value) {
    _useRealAuth = value;
  }

  // Auto fallback to mock auth khi gặp lỗi kết nối
  void _handleConnectionError() {
    _useRealAuth = false;
    debugPrint('⚠️ Chuyển sang chế độ offline với mock auth');
  }

  // Đăng nhập với real hoặc mock tùy thuộc vào cài đặt
  Future<Map<String, dynamic>?> login(String email, String password) async {
    if (_useRealAuth) {
      try {
        final loginResponse = await _realAuth.login(email, password);
        return loginResponse.toJson();
      } catch (e) {
        // Thử lại với mock auth nếu real auth thất bại
        debugPrint('⚠️ Đăng nhập real auth thất bại: $e');
        _handleConnectionError();
      }
    }
    
    // Fallback to mock auth
    try {
      return await _mockAuth.login(email, password);
    } catch (e) {
      debugPrint('❌ Đăng nhập mock auth thất bại: $e');
      rethrow;
    }
  }

  // Kiểm tra trạng thái đăng nhập
  Future<Map<String, dynamic>?> checkLoginStatus() async {
    if (_useRealAuth) {
      try {
        final isLoggedIn = await _realAuth.checkAuthStatus();
        if (isLoggedIn) {
          // Trả về dữ liệu giả định vì AuthServices không cung cấp thông tin người dùng
          final token = await _realAuth.secureStorage.readToken();
          final driverId = await _realAuth.secureStorage.readDriverId();
          if (token != null && driverId != null) {
            return {
              'id': driverId,
              'token': token,
              'isAuthenticated': true,
            };
          }
        }
        return null;
      } catch (e) {
        _handleConnectionError();
        return null;
      }
    }
    
    return await _mockAuth.checkLoginStatus();
  }

  // Đăng xuất
  Future<void> logout() async {
    if (_useRealAuth) {
      await _realAuth.logout();
    }
    await _mockAuth.logout();
  }

  // Lấy thông tin người dùng hiện tại
  Map<String, dynamic>? getCurrentUser() {
    if (_useRealAuth) {
      // AuthServices không có thuộc tính currentUser, nên trả về null hoặc dữ liệu giả
      return null;
    }
    return _mockAuth.currentUser;
  }

  // Lấy token xác thực cho API calls
  Future<String?> getAuthToken() async {
    if (_useRealAuth) {
      return await _realAuth.secureStorage.readToken();
    }
    return null;
  }

  // Kiểm tra đã đăng nhập chưa
  Future<bool> isLoggedIn() async {
    if (_useRealAuth) {
      return await _realAuth.checkAuthStatus();
    }
    return _mockAuth.isLoggedIn;
  }
}
