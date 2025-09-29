import 'dart:convert';
import 'package:flutter/foundation.dart';
import 'package:http/http.dart' as http;
import '../data/env/environment.dart';
import '../data/local_secure/secure_storage.dart';
import '../data/network/http_client.dart';
import '../domain/models/auth/auth_models.dart';

class AuthServices {
  final Environment _env = Environment.getInstance();
  final SecureStorageFrave secureStorage = SecureStorageFrave();
  late final HttpClient _httpClient;
  
  String get baseUrl => _env.apiBaseUrl;
  
  AuthServices() {
    _httpClient = HttpClient(baseUrl: baseUrl, secureStorage: secureStorage);
  }

  // Đăng nhập với API chung
  Future<LoginResponse> login(String email, String password) async {
    try {
      final Map<String, dynamic> requestBody = {
        'email': email,      // Sửa thành email thay vì username
        'password': password,
      };
      
      final data = await _httpClient.post<Map<String, dynamic>>(
        '/auth/login',
        body: requestBody,
      );
      
      final loginResponse = LoginResponse.fromJson(data);
      
      // Lưu token và refresh token vào secure storage
      await secureStorage.persistentToken(loginResponse.accessToken);
      await secureStorage.persistentRefreshToken(loginResponse.refreshToken);
      
      // Lưu ID người dùng - với driver, đây cũng chính là driverId
      if (loginResponse.userId > 0) {
        print('Storing id from API response: ${loginResponse.userId}');
        // Lưu cùng một ID vào cả userId và driverId
        await secureStorage.persistentUserId(loginResponse.userId.toString());
        await secureStorage.persistentDriverId(loginResponse.userId.toString());
        debugPrint('Stored id as both userId and driverId: ${loginResponse.userId}');
      } else {
        debugPrint('Warning: Not storing id because id from API is ${loginResponse.userId}');
      }
      
      return loginResponse;
    } catch (e) {
      // Thử dùng API cũ nếu API mới thất bại
      return _loginLegacy(email, password);
    }
  }
  
  // Phương thức legacy để tương thích với API cũ
  Future<LoginResponse> _loginLegacy(String email, String password) async {
    final resp = await http.post(
      Uri.parse('$baseUrl/auth/login'),
      headers: {
        'Content-Type': 'application/json',
        'Accept': 'application/json'
      },
      body: jsonEncode({'email': email, 'password': password})
    );

    if(resp.statusCode == 200){
      final body = json.decode(resp.body);
      
      // Lấy userId từ response
      final userId = int.tryParse(body['userId']?.toString() ?? body['data']?['userId']?.toString() ?? '0') ?? 0;
      
      // Adapt the legacy response to LoginResponse
      final loginResponse = LoginResponse(
        accessToken: body['token'] ?? body['data']?['token'] ?? '',
        tokenType: 'Bearer',
        expiresIn: 3600,
        refreshToken: body['refreshToken'] ?? body['data']?['refreshToken'] ?? '',
        userId: userId,
        driverId: userId, // Driver ID là User ID
        username: body['username'] ?? body['data']?['username'] ?? '',
        roles: body['roles'] != null ? List<String>.from(body['roles']) : 
              body['data']?['roles'] != null ? List<String>.from(body['data']['roles']) : ['DRIVER'],
      );
      
      await secureStorage.persistentToken(loginResponse.accessToken);
      if (loginResponse.userId > 0) {
        await secureStorage.persistentDriverId(loginResponse.userId.toString()); // Dùng userId
        debugPrint('Stored driverId from legacy login: ${loginResponse.userId}');
      } else {
        debugPrint('Warning: Not storing driverId from legacy login because userId is ${loginResponse.userId}');
      }
      
      return loginResponse;
    }
    
    throw Exception('Invalid Credentials');
  }

  // Làm mới token với API mới
  Future<LoginResponse> refreshToken() async {
    try {
      final refreshToken = await secureStorage.readRefreshToken();
      if (refreshToken == null) {
        throw Exception('Refresh token not found');
      }
      
      final Map<String, dynamic> requestBody = {
        'refreshToken': refreshToken,
      };
      
      final data = await _httpClient.post<Map<String, dynamic>>(
        '/auth/refresh',
        body: requestBody,
      );
      
      final loginResponse = LoginResponse.fromJson(data);
      
      // Lưu token và refresh token mới
      await secureStorage.persistentToken(loginResponse.accessToken);
      await secureStorage.persistentRefreshToken(loginResponse.refreshToken);
      
      return loginResponse;
    } catch (e) {
      throw Exception('Failed to refresh token: $e');
    }
  }

  // Đăng xuất với API mới
  Future<bool> logout() async {
    try {
      await _httpClient.post<bool>(
        '/auth/logout',
        body: {},
      );
      
      // Xóa token và driver ID trong mọi trường hợp
      // Thay vì xóa toàn bộ storage, chỉ xóa các token
      await secureStorage.deleteToken();
      await secureStorage.deleteRefreshToken();
      
      return true;
    } catch (e) {
      // Xóa token và driver ID ngay cả khi API thất bại
      // Thay vì xóa toàn bộ storage, chỉ xóa các token
      await secureStorage.deleteToken();
      await secureStorage.deleteRefreshToken();
      return true;
    }
  }

  // Kiểm tra trạng thái xác thực (token có hiệu lực hay không)
  Future<bool> checkAuthStatus() async {
    try {
      final token = await secureStorage.readToken();
      if (token == null) {
        return false;
      }
      
      // API mới để kiểm tra token
      await _httpClient.get<bool>('/auth/verify');
      return true;
    } catch (e) {
      // Token không hợp lệ hoặc hết hạn
      return false;
    }
  }

  // Đổi mật khẩu với API mới
  Future<bool> changePassword(String oldPassword, String newPassword) async {
    try {
      final Map<String, dynamic> requestBody = {
        'oldPassword': oldPassword,
        'newPassword': newPassword,
      };
      
      return await _httpClient.post<bool>(
        '/auth/change-password',
        body: requestBody,
      );
    } catch (e) {
      throw Exception('Failed to change password: $e');
    }
  }

  // Quên mật khẩu với API mới
  Future<bool> forgotPassword(String email) async {
    try {
      final Map<String, dynamic> requestBody = {
        'email': email,
      };
      
      return await _httpClient.post<bool>(
        '/auth/forgot-password',
        body: requestBody,
      );
    } catch (e) {
      throw Exception('Failed to initiate password reset: $e');
    }
  }

  // Đặt lại mật khẩu với API mới
  Future<bool> resetPassword(String token, String newPassword) async {
    try {
      final Map<String, dynamic> requestBody = {
        'token': token,
        'newPassword': newPassword,
      };
      
      return await _httpClient.post<bool>(
        '/auth/reset-password',
        body: requestBody,
      );
    } catch (e) {
      throw Exception('Failed to reset password: $e');
    }
  }
}
