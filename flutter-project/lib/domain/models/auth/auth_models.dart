// auth_models.dart
// Models cho xác thực và đăng nhập

class LoginRequest {
  final String email;
  final String password;

  LoginRequest({
    required this.email,
    required this.password,
  });

  Map<String, dynamic> toJson() {
    return {
      'email': email,
      'password': password,
    };
  }
}

class LoginResponse {
  final String accessToken;
  final String tokenType;
  final int expiresIn;
  final String refreshToken;
  final int userId;
  final int driverId;
  final String username;
  final List<String> roles;

  LoginResponse({
    required this.accessToken,
    required this.tokenType,
    required this.expiresIn,
    required this.refreshToken,
    required this.userId,
    required this.driverId,
    required this.username,
    required this.roles,
  });

  factory LoginResponse.fromJson(Map<String, dynamic> json) {
    // Extract user info if it exists in the response
    final userInfo = json['user'] as Map<String, dynamic>?;
    
    // Debugging user info
    print('User info from login: $userInfo');
    
    // Get id from API response - lấy từ 'id' trong đối tượng user
    final id = userInfo?['id'] ?? json['userId'] ?? 0;
    
    // Debug ID extraction
    print('Extracted id from API response: $id');
    
    return LoginResponse(
      accessToken: json['accessToken'] ?? json['token'] ?? '',
      tokenType: json['tokenType'] ?? 'Bearer',
      expiresIn: json['expiresIn'] ?? 0,
      refreshToken: json['refreshToken'] ?? '',
      userId: id,          // Sử dụng id làm userId
      driverId: id,        // Sử dụng cùng id làm driverId
      username: json['username'] ?? userInfo?['username'] ?? '',
      roles: json['roles'] != null ? List<String>.from(json['roles']) : 
             userInfo?['role'] != null ? [userInfo?['role'] as String] : [],
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'accessToken': accessToken,
      'tokenType': tokenType,
      'expiresIn': expiresIn,
      'refreshToken': refreshToken,
      'userId': userId,
      'driverId': driverId,
      'username': username,
      'roles': roles,
    };
  }
}
