
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

class SecureStorageFrave {
  final secureStorage = FlutterSecureStorage();
  
  // Token storage
  Future<void> persistentToken(String token) async {
    await secureStorage.write(key: 'token', value: token);
  }

  Future<String?> readToken() async {
    return await secureStorage.read(key: 'token');
  }

  Future<void> deleteToken() async {
    await secureStorage.delete(key: 'token');
  }

  // Refresh token storage
  Future<void> persistentRefreshToken(String refreshToken) async {
    await secureStorage.write(key: 'refreshToken', value: refreshToken);
  }

  Future<String?> readRefreshToken() async {
    return await secureStorage.read(key: 'refreshToken');
  }

  Future<void> deleteRefreshToken() async {
    await secureStorage.delete(key: 'refreshToken');
  }
  
  // User ID storage
  Future<void> persistentUserId(String userId) async {
    await secureStorage.write(key: 'userId', value: userId);
  }

  Future<String?> readUserId() async {
    return await secureStorage.read(key: 'userId');
  }

  Future<void> deleteUserId() async {
    await secureStorage.delete(key: 'userId');
  }
  
  // Role storage
  Future<void> persistentRoleId(String role) async {
    await secureStorage.write(key: 'role', value: role);
  }

  Future<String?> readRoleId() async {
    return await secureStorage.read(key: 'role');
  }
  
  // Driver ID storage
  Future<void> persistentDriverId(String driverId) async {
    await secureStorage.write(key: 'driverId', value: driverId);
  }

  Future<String?> readDriverId() async {
    return await secureStorage.read(key: 'driverId');
  }

  Future<void> deleteDriverId() async {
    await secureStorage.delete(key: 'driverId');
  }

  // Delete all
  Future<void> deleteSecureStorage() async {
    await secureStorage.deleteAll();
  }
}

final secureStorage = SecureStorageFrave();
