// auth_response.dart
// Model class cho response của API đăng nhập

class AuthResponse {
  final bool success;    // Trạng thái thành công
  final String token;    // JWT token từ server
  final User user;       // Thông tin người dùng

  // Constructor với các tham số bắt buộc
  AuthResponse({
    required this.success,
    required this.token,
    required this.user,
  });

  // Factory constructor để parse JSON thành object
  factory AuthResponse.fromJson(Map<String, dynamic> json) {
    return AuthResponse(
      success: json['success'] ?? false,
      token: json['token'] ?? '',
      user: User.fromJson(json['user']),
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'success': success,
      'token': token,
      'user': user.toJson(),
    };
  }
}

// Model cho thông tin người dùng
class User {
  final String uid;        // ID người dùng
  final String name;       // Tên đầy đủ
  final String email;      // Email
  final String phone;      // Số điện thoại
  final String image;      // URL ảnh đại diện
  final String role;       // Vai trò (DRIVER)
  final bool isActive;     // Trạng thái tài khoản
  final List<String> permissions; // Danh sách quyền

  // Constructor với các tham số bắt buộc
  User({
    required this.uid,
    required this.name,
    required this.email,
    required this.phone,
    required this.image,
    required this.role,
    required this.isActive,
    required this.permissions,
  });

  // Factory constructor để parse JSON thành object
  factory User.fromJson(Map<String, dynamic> json) {
    // Parse danh sách permissions
    List<String> permissionsList = [];
    if (json['permissions'] != null) {
      permissionsList = List<String>.from(json['permissions']);
    }

    return User(
      uid: json['uid'] ?? '',
      name: json['name'] ?? '',
      email: json['email'] ?? '',
      phone: json['phone'] ?? '',
      image: json['image'] ?? '',
      role: json['role'] ?? '',
      isActive: json['isActive'] ?? false,
      permissions: permissionsList,
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'uid': uid,
      'name': name,
      'email': email,
      'phone': phone,
      'image': image,
      'role': role,
      'isActive': isActive,
      'permissions': permissions,
    };
  }
}


