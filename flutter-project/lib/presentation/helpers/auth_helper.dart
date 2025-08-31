import 'package:flutter/foundation.dart';
import '../../services/mock_auth_service.dart';
import '../../services/real_auth_service.dart';

/// Lớp helper để xử lý chuyển đổi giữa mock auth và real auth
/// Sử dụng real auth khi có thể, fallback về mock auth khi không có kết nối
class AuthHelper {
  static final AuthHelper _instance = AuthHelper._internal();
  static AuthHelper getInstance() => _instance;
  
  final MockAuthService _mockAuth = MockAuthService();
  final RealAuthService _realAuth = RealAuthService.getInstance();
  
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
        return await _realAuth.login(email, password);
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
        return await _realAuth.checkLoginStatus();
      } catch (e) {
        _handleConnectionError();
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
    return _useRealAuth ? _realAuth.currentUser : _mockAuth.currentUser;
  }

  // Lấy token xác thực cho API calls
  String? getAuthToken() {
    return _useRealAuth ? _realAuth.token : null;
  }

  // Kiểm tra đã đăng nhập chưa
  bool isLoggedIn() {
    return _useRealAuth ? _realAuth.isLoggedIn : _mockAuth.isLoggedIn;
  }
}
