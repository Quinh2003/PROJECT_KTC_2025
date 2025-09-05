/// Mock Authentication Service to replace Firebase Auth
class MockAuthService {
  static final MockAuthService _instance = MockAuthService._internal();
  factory MockAuthService() => _instance;
  MockAuthService._internal();

  // Fake user accounts for testing
  final Map<String, Map<String, dynamic>> _mockUsers = {
    'driver@ktc.com': {
      'id': 'DRV001',
      'email': 'driver@ktc.com',
      'password': '123456',
      'name': 'Nguyễn Văn An',
      'phone': '+84 901 234 567',
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
    'driver2@ktc.com': {
      'id': 'DRV002',
      'email': 'driver2@ktc.com',
      'password': '123456',
      'name': 'Trần Thị Lan',
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

  Future<Map<String, dynamic>> login(String email, String password) async {
    // Simulate network delay
    await Future.delayed(const Duration(seconds: 2));

    final user = _mockUsers[email];
    if (user == null || user['password'] != password) {
      throw Exception('Email hoặc mật khẩu không đúng');
    }

    _currentUser = Map<String, dynamic>.from(user);
    _currentUser!.remove('password'); // Don't store password

    return _currentUser!;
  }

  Future<void> logout() async {
    await Future.delayed(const Duration(milliseconds: 500));
    _currentUser = null;
  }

  Future<Map<String, dynamic>?> checkLoginStatus() async {
    await Future.delayed(const Duration(milliseconds: 500));
    return _currentUser;
  }

  Future<Map<String, dynamic>> updateProfile(Map<String, dynamic> updates) async {
    await Future.delayed(const Duration(seconds: 1));
    
    if (_currentUser == null) {
      throw Exception('Chưa đăng nhập');
    }

    _currentUser!.addAll(updates);
    return _currentUser!;
  }

  Future<void> changePassword(String oldPassword, String newPassword) async {
    await Future.delayed(const Duration(seconds: 1));
    
    if (_currentUser == null) {
      throw Exception('Chưa đăng nhập');
    }

    final email = _currentUser!['email'];
    final storedUser = _mockUsers[email];
    
    if (storedUser == null || storedUser['password'] != oldPassword) {
      throw Exception('Mật khẩu cũ không đúng');
    }

    storedUser['password'] = newPassword;
  }

  // Mock method to get driver statistics
  Future<Map<String, dynamic>> getDriverStats() async {
    await Future.delayed(const Duration(milliseconds: 800));
    
    if (_currentUser == null) {
      throw Exception('Chưa đăng nhập');
    }

    return {
      'totalDeliveries': _currentUser!['totalDeliveries'] ?? 0,
      'completedToday': 12,
      'totalEarnings': 45600000, // VND
      'earningsThisMonth': 8900000,
      'rating': _currentUser!['rating'] ?? 5.0,
      'onTimeDeliveryRate': 98.5,
      'customerSatisfaction': 4.8,
    };
  }

  // Mock method to get vehicle info
  Future<Map<String, dynamic>> getVehicleInfo() async {
    await Future.delayed(const Duration(milliseconds: 500));
    
    if (_currentUser == null) {
      throw Exception('Chưa đăng nhập');
    }

    return _currentUser!['vehicle'] ?? {};
  }
}


