import 'dart:convert';
import 'package:http/http.dart' as http;
import '../local_secure/secure_storage.dart';

/// API interceptor that handles token refresh
class ApiInterceptor {
  final String baseUrl;
  final SecureStorageFrave secureStorage;
  
  ApiInterceptor({
    required this.baseUrl,
    required this.secureStorage,
  });
  
  /// Try to refresh token using refresh token from secure storage
  /// Returns a new access token if successful, null otherwise
  Future<String?> refreshToken() async {
    try {
      // Get refresh token
      final refreshToken = await secureStorage.readRefreshToken();
      if (refreshToken == null) {
        return null;
      }
      
      // Call refresh token API
      final response = await http.post(
        Uri.parse('$baseUrl/auth/refresh'),
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({'refreshToken': refreshToken}),
      );
      
      // Check response
      if (response.statusCode == 200) {
        final data = json.decode(response.body);
        final newToken = data['accessToken'] ?? data['token'];
        final newRefreshToken = data['refreshToken'];
        
        // Store tokens
        if (newToken != null) {
          await secureStorage.persistentToken(newToken);
          if (newRefreshToken != null) {
            await secureStorage.persistentRefreshToken(newRefreshToken);
          }
          return newToken;
        }
      }
      return null;
    } catch (e) {
      print('Error refreshing token: $e');
      return null;
    }
  }
}