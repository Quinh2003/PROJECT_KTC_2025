import 'dart:convert';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:http/http.dart' as http;
import '../data/env/environment.dart';
import '../domain/models/auth/auth_response.dart';

class AuthServices {
  final FlutterSecureStorage secureStorage;

  AuthServices({required this.secureStorage});

  /// Login với HTTP request tới Spring Boot backend
  Future<AuthResponse> loginController(String email, String password) async {
    try {
      final env = Environment.getInstance();
      final response = await http.post(
        Uri.parse('${env.endpointApi}/auth/login'),
        headers: {
          'Accept': 'application/json',
          'Content-Type': 'application/json'
        },
        body: jsonEncode({
          'email': email,
          'password': password,
        }),
      );

      final Map<String, dynamic> responseData = jsonDecode(response.body);
      
      // Chuyển đổi từ format cũ sang format mới
      final Map<String, dynamic> convertedData = {
        'success': responseData['resp'] ?? false,
        'token': responseData['token'] ?? '',
        'user': {
          'uid': responseData['user']?['uid']?.toString() ?? '',
          'name': '${responseData['user']?['firstName'] ?? ''} ${responseData['user']?['lastName'] ?? ''}'.trim(),
          'email': responseData['user']?['email'] ?? '',
          'phone': responseData['user']?['phone'] ?? '',
          'image': responseData['user']?['image'] ?? '',
          'role': responseData['user']?['rolId'] == 2 ? 'DRIVER' : 'USER',
          'isActive': true,
          'permissions': ['DRIVE', 'TRACK']
        }
      };

      return AuthResponse.fromJson(convertedData);
    } catch (e) {
      return AuthResponse(
        success: false,
        token: '',
        user: User(
          uid: '',
          name: '',
          email: '',
          phone: '',
          image: '',
          role: '',
          isActive: false,
          permissions: [],
        ),
      );
    }
  }

  /// Renew token
  Future<AuthResponse> renewLoginController() async {
    try {
      final token = await secureStorage.read(key: 'token');
      final env = Environment.getInstance();

      final response = await http.get(
        Uri.parse('${env.endpointApi}/renew-token-login'),
        headers: {
          'Accept': 'application/json',
          'xx-token': token!,
        },
      );

      final Map<String, dynamic> responseData = jsonDecode(response.body);
      
      // Chuyển đổi từ format cũ sang format mới
      final Map<String, dynamic> convertedData = {
        'success': responseData['resp'] ?? false,
        'token': responseData['token'] ?? '',
        'user': {
          'uid': responseData['user']?['uid']?.toString() ?? '',
          'name': '${responseData['user']?['firstName'] ?? ''} ${responseData['user']?['lastName'] ?? ''}'.trim(),
          'email': responseData['user']?['email'] ?? '',
          'phone': responseData['user']?['phone'] ?? '',
          'image': responseData['user']?['image'] ?? '',
          'role': responseData['user']?['rolId'] == 2 ? 'DRIVER' : 'USER',
          'isActive': true,
          'permissions': ['DRIVE', 'TRACK']
        }
      };

      return AuthResponse.fromJson(convertedData);
    } catch (e) {
      return AuthResponse(
        success: false,
        token: '',
        user: User(
          uid: '',
          name: '',
          email: '',
          phone: '',
          image: '',
          role: '',
          isActive: false,
          permissions: [],
        ),
      );
    }
  }

  /// Logout
  Future<void> logout() async {
    await secureStorage.delete(key: 'token');
    await secureStorage.deleteAll();
  }

  /// Check if user is logged in
  Future<bool> isLoggedIn() async {
    final token = await secureStorage.read(key: 'token');
    return token != null;
  }

  /// Get stored token
  Future<String?> getToken() async {
    return await secureStorage.read(key: 'token');
  }
}

final authServices = AuthServices(
  secureStorage: const FlutterSecureStorage(),
);
