import 'dart:convert';
import 'package:http/http.dart' as http;
import 'package:flutter/foundation.dart';
import '../data/env/environment.dart';
import '../data/local_secure/secure_storage.dart';
import '../data/network/http_client.dart';
import '../domain/models/auth/auth_response.dart';
import '../domain/models/common/response_default.dart';

class UserServices {
  final Environment _env = Environment.getInstance();
  late final HttpClient _httpClient;

  UserServices() {
    _httpClient = HttpClient(baseUrl: _env.apiBaseUrl, secureStorage: secureStorage);
  }

  /// Get current user by ID from secure token
  Future<User> getUserById() async {
    final token = await secureStorage.readToken();
    final userId = await secureStorage.readUserId(); // Đọc userId từ secure storage
    final driverId = await secureStorage.readDriverId(); // Thử đọc driverId nếu không có userId
    
    // Sử dụng driverId nếu không có userId
    final finalId = (userId == null || userId.isEmpty) ? driverId : userId;
    
    if (finalId == null || finalId.isEmpty) {
      // Trả về user mặc định thay vì throw exception
      return User(
        uid: '0',
        name: 'Guest User',
        email: '',
        phone: '',
        image: '',
        role: 'GUEST',
        isActive: true,
        permissions: [],
        username: '',
      );
    }
    
    debugPrint('Fetching user details with ID: $finalId');
    
    try {
      print('👤 UserService: Getting user via HttpClient...');

      final response = await _httpClient.get<Map<String, dynamic>>(
        '/auth/users/$finalId',
        useCache: false,
        timeout: const Duration(seconds: 45),
      );
      
      print('👤 UserService: User response received');
      debugPrint('User API response: success');
      
      // Chuyển đổi từ API response mới sang model User
      return User(
        uid: response['id'].toString(),
        name: response['fullName'] ?? '',
        email: response['email'] ?? '',
        phone: response['phone'] ?? '',
        image: '', // API không trả về image
        role: response['role'] != null ? response['role']['roleName'] ?? '' : '',
        isActive: response['status'] != null ? response['status']['name'] != 'Inactive' : true,
        permissions: [], // API không trả về permissions
        username: response['username'] ?? '', // Thêm username
      );
    } catch (e) {
      print('❌ UserService: Error getting user: $e');
      // Trả về user mặc định khi có lỗi
      return User(
        uid: finalId,
        name: 'User',
        email: '',
        phone: '',
        image: '',
        role: 'USER',
        isActive: true,
        permissions: [],
        username: '',
      );
    }
  }

  /// Edit user profile information
  Future<ResponseDefault> editProfile(String name, String lastname, String phone) async {
    try {
      print('👤 UserService: Editing profile via HttpClient...');

      final response = await _httpClient.put<Map<String, dynamic>>(
        '/edit-profile',
        body: {
          'firstname': name,
          'lastname': lastname,
          'phone': phone
        },
        fromJson: (json) => json,
      );
      
      print('👤 UserService: Profile edit response received');
      return ResponseDefault.fromJson(response);
    } catch (e) {
      print('❌ UserService: Error editing profile: $e');
      return ResponseDefault(resp: false, msg: 'Error: $e');
    }
  }

  /// Change user password
  Future<ResponseDefault> changePassword(String currentPassword, String newPassword) async {
    try {
      print('👤 UserService: Changing password via HttpClient...');

      final response = await _httpClient.put<Map<String, dynamic>>(
        '/change-password',
        body: {
          'currentPassword': currentPassword,
          'newPassword': newPassword
        },
        fromJson: (json) => json,
      );

      print('👤 UserService: Password change response received');
      return ResponseDefault.fromJson(response);
    } catch (e) {
      print('❌ UserService: Error changing password: $e');
      return ResponseDefault(resp: false, msg: 'Error: $e');
    }
  }

  /// Change user profile image
  Future<ResponseDefault> changeImageProfile(String image) async {
    final token = await secureStorage.readToken();

    var request = http.MultipartRequest('PUT', Uri.parse('${_env.endpointApi}/change-image-profile'))
      ..headers['Accept'] = 'application/json'
      ..headers['xx-token'] = token!
      ..files.add(await http.MultipartFile.fromPath('image', image));

    final response = await request.send();
    var data = await http.Response.fromStream(response);

    return ResponseDefault.fromJson(jsonDecode(data.body));
  }

  /// Register new delivery user
  Future<ResponseDefault> registerDelivery(
    String name, 
    String lastname, 
    String phone, 
    String email, 
    String password, 
    String image
  ) async {
    final token = await secureStorage.readToken();

    var request = http.MultipartRequest('POST', Uri.parse('${_env.endpointApi}/register-delivery'))
      ..headers['Accept'] = 'application/json'
      ..headers['xx-token'] = token!
      ..fields['firstname'] = name
      ..fields['lastname'] = lastname
      ..fields['phone'] = phone
      ..fields['email'] = email
      ..fields['password'] = password
      ..files.add(await http.MultipartFile.fromPath('image', image));

    final response = await request.send();
    var data = await http.Response.fromStream(response);

    return ResponseDefault.fromJson(jsonDecode(data.body));
  }

  /// Register new client user
  Future<ResponseDefault> registerClient(
    String name, 
    String lastname, 
    String phone, 
    String email, 
    String password, 
    String address,
    String reference,
    String image
  ) async {
    var request = http.MultipartRequest('POST', Uri.parse('${_env.endpointApi}/register-client'))
      ..headers['Accept'] = 'application/json'
      ..fields['firstname'] = name
      ..fields['lastname'] = lastname
      ..fields['phone'] = phone
      ..fields['email'] = email
      ..fields['password'] = password
      ..fields['address'] = address
      ..fields['reference'] = reference
      ..files.add(await http.MultipartFile.fromPath('image', image));

    final response = await request.send();
    var data = await http.Response.fromStream(response);

    return ResponseDefault.fromJson(jsonDecode(data.body));
  }

  /// Delete street address
  Future<ResponseDefault> deleteStreetAddress(String idAddress) async {
    final token = await secureStorage.readToken();

    final resp = await http.delete(
      Uri.parse('${_env.endpointApi}/delete-street-address/$idAddress'),
      headers: {'Accept': 'application/json', 'xx-token': token!}
    );

    return ResponseDefault.fromJson(jsonDecode(resp.body));
  }

  /// Add new address location
  Future<ResponseDefault> addNewAddressLocation(
    String street, 
    String reference, 
    String latitude, 
    String longitude
  ) async {
    final token = await secureStorage.readToken();

    final resp = await http.post(
      Uri.parse('${_env.endpointApi}/add-new-address'),
      headers: {'Accept': 'application/json', 'xx-token': token!},
      body: {
        'street': street,
        'reference': reference,
        'latitude': latitude,
        'longitude': longitude
      }
    );

    return ResponseDefault.fromJson(jsonDecode(resp.body));
  }

  /// Update delivery to client
  Future<ResponseDefault> updateDeliveryToClient(String idPerson) async {
    final token = await secureStorage.readToken();

    final resp = await http.put(
      Uri.parse('${_env.endpointApi}/update-delivery-to-client/$idPerson'),
      headers: {'Accept': 'application/json', 'xx-token': token!},
    );

    return ResponseDefault.fromJson(jsonDecode(resp.body));
  }

  /// Get one address - Simple placeholder
  Future<dynamic> getAddressOne() async {
    return {'address': {'id': 1, 'reference': 'Sample Address'}};
  }
}

final userServices = UserServices();
