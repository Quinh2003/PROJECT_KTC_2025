/// Mock Authentication Service
/// Cung cấp dữ liệu giả lập cho chức năng xác thực và thông tin tài xế
class MockAuthService {
  static final MockAuthService _instance = MockAuthService._internal();
  factory MockAuthService() => _instance;
  MockAuthService._internal();

  // Dữ liệu tài khoản giả lập
  final Map<String, Map<String, dynamic>> _mockUsers = {
    'driver_01@fr.com': {
      'id': 'DRV001',
      'email': 'driver_01@fr.com',
      'password': '123456',
      'name': 'Nguyễn Văn Tài',
      'phone': '0912345678',
      'avatar': 'https://images.unsplash.com/photo-1507003211169-0a1dd7228f2d?w=150&h=150&fit=crop&crop=face',
      'license': 'B2',
      'vehicle': {
        'id': 'VH001',
        'type': 'Xe tải nhỏ',
        'plate': '29A-12345',
        'capacity': '1.5 tấn',
        'brand': 'Hyundai Porter',
      },
      'status': 'active',
      'rating': 4.8,
      'totalDeliveries': 1247,
      'joinDate': '2023-01-15',
    },
    'driver_offline@ktc.com': {
      'id': 'DRV999',
      'email': 'driver_offline@ktc.com',
      'password': '123456',
      'name': 'Driver Offline Mode',
      'phone': '+84 902 345 678',
      'avatar': 'https://images.unsplash.com/photo-1494790108755-2616b332c108?w=150&h=150&fit=crop&crop=face',
      'license': 'B2',
      'vehicle': {
        'id': 'VH002',
        'type': 'Xe máy',
        'plate': '29B1-67890',
        'capacity': '30 kg',
        'brand': 'Honda Lead',
      },
      'status': 'active',
      'rating': 4.9,
      'totalDeliveries': 856,
      'joinDate': '2023-03-20',
    },
  };

  Map<String, dynamic>? _currentUser;

  Map<String, dynamic>? get currentUser => _currentUser;
  bool get isLoggedIn => _currentUser != null;

  // Đăng nhập
  Future<Map<String, dynamic>> login(String email, String password) async {
    await Future.delayed(const Duration(seconds: 1));
    final user = _mockUsers[email];
    if (user == null || user['password'] != password) {
      throw Exception('Email hoặc mật khẩu không đúng');
    }
    _currentUser = Map<String, dynamic>.from(user);
    _currentUser!.remove('password');
    return _currentUser!;
  }

  // Đăng xuất
  Future<void> logout() async {
    await Future.delayed(const Duration(milliseconds: 500));
    _currentUser = null;
  }

  // Kiểm tra trạng thái đăng nhập
  Future<Map<String, dynamic>?> checkLoginStatus() async {
    await Future.delayed(const Duration(milliseconds: 500));
    return _currentUser;
  }

  // Cập nhật thông tin tài xế
  Future<Map<String, dynamic>> updateProfile(Map<String, dynamic> updates) async {
    await Future.delayed(const Duration(seconds: 1));
    if (_currentUser == null) throw Exception('Chưa đăng nhập');
    _currentUser!.addAll(updates);
    return _currentUser!;
  }

  // Đổi mật khẩu
  Future<void> changePassword(String oldPassword, String newPassword) async {
    await Future.delayed(const Duration(seconds: 1));
    if (_currentUser == null) throw Exception('Chưa đăng nhập');
    final email = _currentUser!['email'];
    final storedUser = _mockUsers[email];
    if (storedUser == null || storedUser['password'] != oldPassword) {
      throw Exception('Mật khẩu cũ không đúng');
    }
    storedUser['password'] = newPassword;
  }

  // Thống kê tài xế
  Future<Map<String, dynamic>> getDriverStats() async {
    await Future.delayed(const Duration(milliseconds: 800));
    if (_currentUser == null) throw Exception('Chưa đăng nhập');
    return {
      'totalDeliveries': _currentUser!['totalDeliveries'] ?? 0,
      'completedToday': 12,
      'totalEarnings': 45600000,
      'earningsThisMonth': 8900000,
      'rating': _currentUser!['rating'] ?? 5.0,
      'onTimeDeliveryRate': 98.5,
      'customerSatisfaction': 4.8,
    };
  }

  // Thông tin phương tiện
  Future<Map<String, dynamic>> getVehicleInfo() async {
    await Future.delayed(const Duration(milliseconds: 500));
    if (_currentUser == null) throw Exception('Chưa đăng nhập');
    return _currentUser!['vehicle'] ?? {};
  }
}


