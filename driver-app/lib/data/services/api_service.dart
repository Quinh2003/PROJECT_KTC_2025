import 'dart:convert';
import 'package:http/http.dart' as http;
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:ktc_logistics_driver/domain/models/response/auth_response.dart';

// Lớp ApiService chịu trách nhiệm giao tiếp với backend Spring Boot
class ApiService {
  // URL cơ sở của backend Spring Boot
  static const String _baseUrl = 'http://192.168.1.10:8080/api'; // Thay đổi IP và port tương ứng với backend của bạn
  
  // Lưu trữ bảo mật cho token JWT
  final _storage = const FlutterSecureStorage();
  
  // Headers cơ bản cho HTTP request
  Future<Map<String, String>> _getHeaders() async {
    // Lấy token JWT từ secure storage
    final token = await _storage.read(key: 'token');
    
    // Headers cơ bản với Content-Type là application/json
    final headers = {
      'Content-Type': 'application/json',
    };
    
    // Nếu có token, thêm Authorization header
    if (token != null) {
      headers['Authorization'] = 'Bearer $token';
    }
    
    return headers;
  }

  // ĐĂNG NHẬP: Gửi thông tin đăng nhập tới backend và nhận JWT token
  Future<AuthResponse> login(String email, String password) async {
    try {
      // Tạo URL đến endpoint đăng nhập
      final url = Uri.parse('$_baseUrl/auth/login');
      
      // Chuẩn bị dữ liệu đăng nhập
      final body = jsonEncode({
        'email': email,
        'password': password,
      });
      
      // Gửi POST request đến backend
      final response = await http.post(
        url,
        headers: await _getHeaders(),
        body: body,
      );
      
      // Kiểm tra response status code
      if (response.statusCode == 200) {
        // Parse response JSON thành AuthResponse object
        final authResponse = AuthResponse.fromJson(jsonDecode(response.body));
        
        // Lưu token vào secure storage
        await _storage.write(key: 'token', value: authResponse.token);
        
        return authResponse;
      } else {
        // Nếu không thành công, throw exception với message từ server
        throw Exception('Login failed: ${response.body}');
      }
    } catch (e) {
      // Xử lý lỗi network hoặc parsing
      throw Exception('Login failed: $e');
    }
  }

  // ĐĂNG XUẤT: Xóa token và thông tin người dùng
  Future<void> logout() async {
    // Xóa token từ secure storage
    await _storage.delete(key: 'token');
  }

  // LẤY DANH SÁCH ĐƠN HÀNG: Lấy các đơn hàng cho tài xế
  Future<List<dynamic>> getOrders() async {
    try {
      // Tạo URL đến endpoint lấy đơn hàng
      final url = Uri.parse('$_baseUrl/delivery/orders');
      
      // Gửi GET request với token trong header
      final response = await http.get(
        url,
        headers: await _getHeaders(),
      );
      
      // Kiểm tra response status
      if (response.statusCode == 200) {
        // Parse response JSON thành List
        final List<dynamic> orders = jsonDecode(response.body);
        return orders;
      } else {
        // Nếu không thành công, throw exception
        throw Exception('Failed to load orders: ${response.body}');
      }
    } catch (e) {
      // Xử lý lỗi network hoặc parsing
      throw Exception('Failed to load orders: $e');
    }
  }

  // CẬP NHẬT TRẠNG THÁI ĐƠN HÀNG: Thay đổi trạng thái (đang giao, đã giao, v.v.)
  Future<bool> updateOrderStatus(String orderId, String status) async {
    try {
      // Tạo URL đến endpoint cập nhật trạng thái
      final url = Uri.parse('$_baseUrl/delivery/orders/$orderId/status');
      
      // Chuẩn bị dữ liệu cập nhật
      final body = jsonEncode({
        'status': status,
      });
      
      // Gửi PUT request với token trong header
      final response = await http.put(
        url,
        headers: await _getHeaders(),
        body: body,
      );
      
      // Kiểm tra response status
      return response.statusCode == 200;
    } catch (e) {
      // Xử lý lỗi network
      throw Exception('Failed to update order status: $e');
    }
  }

  // UPLOAD ẢNH XÁC NHẬN: Tải ảnh xác nhận giao hàng lên server
  Future<bool> uploadDeliveryProof(String orderId, String imagePath) async {
    try {
      // Tạo URL đến endpoint upload ảnh
      final url = Uri.parse('$_baseUrl/delivery-proof/upload');
      
      // Lấy token để thêm vào header
      final token = await _storage.read(key: 'token');
      
      // Tạo multipart request để upload file
      final request = http.MultipartRequest('POST', url);
      
      // Thêm Authorization header
      request.headers['Authorization'] = 'Bearer $token';
      
      // Thêm file ảnh vào request
      request.files.add(await http.MultipartFile.fromPath(
        'image',
        imagePath,
      ));
      
      // Thêm orderId vào request
      request.fields['orderId'] = orderId;
      
      // Gửi request và lấy response
      final response = await request.send();
      
      // Kiểm tra response status
      return response.statusCode == 200;
    } catch (e) {
      // Xử lý lỗi network hoặc IO
      throw Exception('Failed to upload delivery proof: $e');
    }
  }

  // CẬP NHẬT VỊ TRÍ: Gửi vị trí GPS của tài xế lên server
  Future<bool> updateLocation(double latitude, double longitude) async {
    try {
      // Tạo URL đến endpoint cập nhật vị trí
      final url = Uri.parse('$_baseUrl/delivery-tracking/update');
      
      // Chuẩn bị dữ liệu vị trí
      final body = jsonEncode({
        'latitude': latitude,
        'longitude': longitude,
        'timestamp': DateTime.now().toIso8601String(),
      });
      
      // Gửi POST request với token trong header
      final response = await http.post(
        url,
        headers: await _getHeaders(),
        body: body,
      );
      
      // Kiểm tra response status
      return response.statusCode == 200;
    } catch (e) {
      // Xử lý lỗi network
      throw Exception('Failed to update location: $e');
    }
  }

  // LẤY THÔNG TIN ROUTE: Lấy thông tin tuyến đường từ backend
  Future<Map<String, dynamic>> getRouteInfo(String orderId) async {
    try {
      // Tạo URL đến endpoint lấy thông tin route
      final url = Uri.parse('$_baseUrl/route/$orderId');
      
      // Gửi GET request với token trong header
      final response = await http.get(
        url,
        headers: await _getHeaders(),
      );
      
      // Kiểm tra response status
      if (response.statusCode == 200) {
        // Parse response JSON thành Map
        final Map<String, dynamic> routeInfo = jsonDecode(response.body);
        return routeInfo;
      } else {
        // Nếu không thành công, throw exception
        throw Exception('Failed to load route info: ${response.body}');
      }
    } catch (e) {
      // Xử lý lỗi network hoặc parsing
      throw Exception('Failed to load route info: $e');
    }
  }

  // KIỂM TRA TOKEN: Xác minh token còn hạn hay không
  Future<bool> verifyToken() async {
    try {
      // Tạo URL đến endpoint kiểm tra token
      final url = Uri.parse('$_baseUrl/auth/verify');
      
      // Gửi GET request với token trong header
      final response = await http.get(
        url,
        headers: await _getHeaders(),
      );
      
      // Kiểm tra response status - 200 nghĩa là token còn hạn
      return response.statusCode == 200;
    } catch (e) {
      // Xử lý lỗi network, mặc định trả về false
      return false;
    }
  }
}
