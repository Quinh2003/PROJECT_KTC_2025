// Model cho response khi đăng nhập/xác thực từ backend Spring Boot
class AuthResponse {
  // Token JWT được trả về từ server khi đăng nhập thành công
  final String token;
  
  // ID của người dùng (tài xế)
  final String userId;
  
  // Thông tin chi tiết của người dùng
  final UserDetails user;
  
  // Constructor
  AuthResponse({
    required this.token,
    required this.userId,
    required this.user,
  });
  
  // Factory constructor để parse từ JSON sang object
  factory AuthResponse.fromJson(Map<String, dynamic> json) {
    return AuthResponse(
      token: json['token'] as String,
      userId: json['userId'] as String,
      user: UserDetails.fromJson(json['user']),
    );
  }
  
  // Chuyển object sang JSON
  Map<String, dynamic> toJson() {
    return {
      'token': token,
      'userId': userId,
      'user': user.toJson(),
    };
  }
}

// Model thông tin chi tiết của người dùng
class UserDetails {
  // ID của người dùng
  final String id;
  
  // Email của người dùng
  final String email;
  
  // Tên đầy đủ của người dùng
  final String fullName;
  
  // Vai trò của người dùng (driver, admin, ...)
  final String role;
  
  // URL ảnh đại diện (nếu có)
  final String? avatar;
  
  // Số điện thoại
  final String? phone;
  
  // Constructor
  UserDetails({
    required this.id,
    required this.email,
    required this.fullName,
    required this.role,
    this.avatar,
    this.phone,
  });
  
  // Factory constructor để parse từ JSON sang object
  factory UserDetails.fromJson(Map<String, dynamic> json) {
    return UserDetails(
      id: json['id'] as String,
      email: json['email'] as String,
      fullName: json['fullName'] as String,
      role: json['role'] as String,
      avatar: json['avatar'] as String?,
      phone: json['phone'] as String?,
    );
  }
  
  // Chuyển object sang JSON
  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'email': email,
      'fullName': fullName,
      'role': role,
      'avatar': avatar,
      'phone': phone,
    };
  }
}
