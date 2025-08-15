import 'dart:convert';
import 'package:http/http.dart' as http;
import 'package:ktc_logistics_driver/data/env/environment.dart';
import 'package:ktc_logistics_driver/data/local_secure/secure_storage.dart';
import 'package:ktc_logistics_driver/domain/models/response/address_one_response.dart';
import 'package:ktc_logistics_driver/domain/models/response/addresses_response.dart';
import 'package:ktc_logistics_driver/domain/models/response/response_default.dart';
import 'package:ktc_logistics_driver/domain/models/response/response_login.dart';
import 'package:ktc_logistics_driver/domain/models/response/user_updated_response.dart';
import 'package:ktc_logistics_driver/services/push_notification.dart';

class UserServices {
  // Environment instance
  final _env = Environment.getInstance();
  final _pushNotification = PushNotification();

  Future<User> getUserById() async {
    final token = await secureStorage.readToken();
    
    final response = await http.get(Uri.parse('${_env.apiBaseUrl}/get-user-by-id'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!}
    );

    return ResponseLogin.fromJson(jsonDecode(response.body)).user;
  }


  Future<ResponseDefault> editProfile(String name, String lastname, String phone) async {
    final token = await secureStorage.readToken();

    final response = await http.put(Uri.parse('${_env.apiBaseUrl}/edit-profile'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!},
      body: {
        'firstname' : name,
        'lastname' : lastname,
        'phone' : phone
      }
    );

    return ResponseDefault.fromJson(jsonDecode(response.body));
  }


  Future<UserUpdated> getUserUpdated() async {
    final token = await secureStorage.readToken();
    
    final response = await http.get(Uri.parse('${_env.apiBaseUrl}/get-user-updated'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!}
    );

    return UserUpdatedResponse.fromJson(jsonDecode(response.body)).user;
  }


  Future<ResponseDefault> changePassword(String currentPassword, String newPassword) async {
    final token = await secureStorage.readToken();

    final response = await http.put(Uri.parse('${_env.apiBaseUrl}/change-password'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!},
      body: {
        'currentPassword' : currentPassword,
        'newPassword' : newPassword
      }
    );

    return ResponseDefault.fromJson(jsonDecode(response.body));
  }


  Future<ResponseDefault> changeImageProfile(String image) async {
    final token = await secureStorage.readToken();

    var request = http.MultipartRequest('PUT', Uri.parse('${_env.apiBaseUrl}/change-image-profile'))
      ..headers['Accept'] = 'application/json'
      ..headers['xx-token'] = token!
      ..files.add(await http.MultipartFile.fromPath('image', image));

    final response = await request.send();
    var data = await http.Response.fromStream(response);

    return ResponseDefault.fromJson(jsonDecode(data.body));
  }


  Future<ResponseDefault> registerDelivery(String name, String lastname, String phone, String email, String password, String image) async {
    final token = await secureStorage.readToken();

    var request = http.MultipartRequest('POST', Uri.parse('${_env.apiBaseUrl}/register-delivery'))
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


  Future<ResponseDefault> registerClient(String name, String lastname, String phone, String email, String password, String address, String reference, String image) async {
    final token = await secureStorage.readToken();

    var request = http.MultipartRequest('POST', Uri.parse('${_env.apiBaseUrl}/register-client'))
      ..headers['Accept'] = 'application/json'
      ..headers['xx-token'] = token!
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


  Future<List<ListAddress>> getAddressesUser() async {
    final token = await secureStorage.readToken();
    
    final response = await http.get(Uri.parse('${_env.apiBaseUrl}/get-addresses'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!}
    );

    return AddressesResponse.fromJson(jsonDecode(response.body)).listAddresses;
  }


  Future<ResponseDefault> deleteStreetAddress(String idAddress) async {
    final token = await secureStorage.readToken();

    final resp = await http.delete(Uri.parse('${_env.apiBaseUrl}/delete-street-address/$idAddress'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!}
    );

    return ResponseDefault.fromJson(jsonDecode(resp.body));
  }


  Future<ResponseDefault> addStreetAddress(String street, String reference) async {
    final token = await secureStorage.readToken();

    final resp = await http.post(Uri.parse('${_env.apiBaseUrl}/add-new-address'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!},
      body: {
        'street': street,
        'reference': reference
      }
    );

    return ResponseDefault.fromJson(jsonDecode(resp.body));
  }


  Future<Address> getAddressOne(String idAddress) async {
    final token = await secureStorage.readToken();

    final resp = await http.get(
      Uri.parse('${_env.apiBaseUrl}/get-address?address=$idAddress'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!},
    );

    return AddressOneResponse.fromJson(jsonDecode(resp.body)).address;
  }


  Future<ResponseDefault> updateNotificationToken() async {
    final token = await secureStorage.readToken();
    final nToken = await _pushNotification.getNotificationToken();

    final resp = await http.put(Uri.parse('${_env.apiBaseUrl}/update-notification-token'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!},
      body: {
        'nToken': nToken
      }
    );

    return ResponseDefault.fromJson(jsonDecode(resp.body));
  }


  Future<List<String>> getAdminsNotificationToken() async {
    final token = await secureStorage.readToken();

    final resp = await http.get(Uri.parse('${_env.apiBaseUrl}/get-admins-notification-token'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!}
    );

    return List<String>.from(jsonDecode(resp.body));
  }


  Future<ResponseDefault> updateDeliveryToClient(String idPerson) async {
    final token = await secureStorage.readToken();

    final resp = await http.put(Uri.parse('${_env.apiBaseUrl}/update-delivery-to-client/$idPerson'),
      headers: {'Accept' : 'application/json', 'xx-token' : token!}
    );

    return ResponseDefault.fromJson(jsonDecode(resp.body));
  }

}
