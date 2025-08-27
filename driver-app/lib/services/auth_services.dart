import 'dart:convert';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:http/http.dart' as http;
import '../data/env/environment.dart';
import '../domain/models/response/response_login.dart';

class AuthServices {
  final FlutterSecureStorage secureStorage;

  AuthServices({required this.secureStorage});

  /// Login với HTTP request thay vì Firebase Auth
  Future<ResponseLogin> loginController(String email, String password) async {
    try {
      final env = Environment.getInstance();
      final response = await http.post(
        Uri.parse('${env.endpointApi}/login-email-id'),
        headers: {'Accept': 'application/json'},
        body: {
          'email': email,
          'password': password,
        },
      );

      return ResponseLogin.fromJson(jsonDecode(response.body));
    } catch (e) {
      return ResponseLogin(
        resp: false,
        msg: e.toString(),
        token: '',
        user: User(
          uid: 0,
          firstName: '',
          lastName: '',
          email: '',
          phone: '',
          image: '',
          rolId: 0,
          notificationToken: '',
        ),
      );
    }
  }

  /// Renew token
  Future<ResponseLogin> renewLoginController() async {
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

      return ResponseLogin.fromJson(jsonDecode(response.body));
    } catch (e) {
      return ResponseLogin(
        resp: false,
        msg: e.toString(),
        token: '',
        user: User(
          uid: 0,
          firstName: '',
          lastName: '',
          email: '',
          phone: '',
          image: '',
          rolId: 0,
          notificationToken: '',
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
