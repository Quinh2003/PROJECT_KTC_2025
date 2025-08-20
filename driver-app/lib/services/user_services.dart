import 'dart:convert';
import 'package:http/http.dart' as http;
import '../data/env/environment.dart';
import '../data/local_secure/secure_storage.dart';
import '../domain/models/response/auth_response.dart';
import '../domain/models/response/response_default.dart';

class UserServices {
  final Environment _env = Environment.getInstance();

  /// Get current user by ID from secure token
  Future<User> getUserById() async {
    final token = await secureStorage.readToken();
    
    final response = await http.get(
      Uri.parse('${_env.endpointApi}/get-user-by-id'),
      headers: {'Accept': 'application/json', 'xx-token': token!}
    );

    return AuthResponse.fromJson(jsonDecode(response.body)).user;
  }

  /// Edit user profile information
  Future<ResponseDefault> editProfile(String name, String lastname, String phone) async {
    final token = await secureStorage.readToken();

    final response = await http.put(
      Uri.parse('${_env.endpointApi}/edit-profile'),
      headers: {'Accept': 'application/json', 'xx-token': token!},
      body: {
        'firstname': name,
        'lastname': lastname,
        'phone': phone
      }
    );
    
    return ResponseDefault.fromJson(jsonDecode(response.body));
  }

  /// Change user password
  Future<ResponseDefault> changePassword(String currentPassword, String newPassword) async {
    final token = await secureStorage.readToken();

    final response = await http.put(
      Uri.parse('${_env.endpointApi}/change-password'),
      headers: {'Accept': 'application/json', 'xx-token': token!},
      body: {
        'currentPassword': currentPassword,
        'newPassword': newPassword
      }
    );

    return ResponseDefault.fromJson(jsonDecode(response.body));
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
