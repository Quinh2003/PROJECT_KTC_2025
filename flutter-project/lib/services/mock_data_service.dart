/// Mock Data Service for simulating backend responses
class MockDataService {
  static final MockDataService _instance = MockDataService._internal();
  factory MockDataService() => _instance;
  MockDataService._internal();

  // Mock Orders Data
  final List<Map<String, dynamic>> _mockOrders = [
    {
      'id': 'KTC-2025-001',
      'customerName': 'Nguyễn Văn Minh',
      'customerPhone': '+84 901 234 567',
      'customerEmail': 'nguyen.minh@email.com',
      'pickupAddress': {
        'street': '123 Đường Lê Duẩn',
        'district': 'Quận 1',
        'city': 'TP. Hồ Chí Minh',
        'coordinates': {'lat': 10.7769, 'lng': 106.7009},
      },
      'deliveryAddress': {
        'street': '456 Đường Nguyễn Huệ',
        'district': 'Quận 1', 
        'city': 'TP. Hồ Chí Minh',
        'coordinates': {'lat': 10.7744, 'lng': 106.7045},
      },
      'items': [
        {'name': 'Điện thoại iPhone 15', 'quantity': 1, 'weight': 0.5},
        {'name': 'Tai nghe AirPods', 'quantity': 1, 'weight': 0.1},
      ],
      'totalWeight': 0.6,
      'totalValue': 35000000,
      'status': 'pending',
      'priority': 'high',
      'estimatedDeliveryTime': '2025-08-14T16:00:00Z',
      'notes': 'Giao hàng trong giờ hành chính',
      'paymentMethod': 'cod',
      'shippingFee': 50000,
      'createdAt': '2025-08-14T09:00:00Z',
    },
    {
      'id': 'KTC-2025-002',
      'customerName': 'Trần Thị Lan Anh',
      'customerPhone': '+84 902 345 678',
      'customerEmail': 'tran.lananh@email.com',
      'pickupAddress': {
        'street': '789 Đường Pasteur',
        'district': 'Quận 3',
        'city': 'TP. Hồ Chí Minh',
        'coordinates': {'lat': 10.7700, 'lng': 106.6950},
      },
      'deliveryAddress': {
        'street': '321 Đường Võ Văn Tần',
        'district': 'Quận 3',
        'city': 'TP. Hồ Chí Minh', 
        'coordinates': {'lat': 10.7720, 'lng': 106.6920},
      },
      'items': [
        {'name': 'Laptop Dell Inspiron', 'quantity': 1, 'weight': 2.5},
        {'name': 'Chuột không dây', 'quantity': 1, 'weight': 0.2},
      ],
      'totalWeight': 2.7,
      'totalValue': 18000000,
      'status': 'pickup',
      'priority': 'medium',
      'estimatedDeliveryTime': '2025-08-14T18:00:00Z',
      'notes': 'Kiểm tra hàng trước khi giao',
      'paymentMethod': 'paid',
      'shippingFee': 75000,
      'createdAt': '2025-08-14T10:30:00Z',
    },
    {
      'id': 'KTC-2025-003',
      'customerName': 'Lê Hoàng Nam',
      'customerPhone': '+84 903 456 789',
      'customerEmail': 'le.hoangnam@email.com',
      'pickupAddress': {
        'street': '147 Đường Hàm Nghi',
        'district': 'Quận 1',
        'city': 'TP. Hồ Chí Minh',
        'coordinates': {'lat': 10.7750, 'lng': 106.7000},
      },
      'deliveryAddress': {
        'street': '258 Đường Điện Biên Phủ',
        'district': 'Quận Bình Thạnh',
        'city': 'TP. Hồ Chí Minh',
        'coordinates': {'lat': 10.8010, 'lng': 106.7100},
      },
      'items': [
        {'name': 'Máy ảnh Canon EOS', 'quantity': 1, 'weight': 1.2},
        {'name': 'Lens 50mm', 'quantity': 1, 'weight': 0.8},
      ],
      'totalWeight': 2.0,
      'totalValue': 25000000,
      'status': 'in_transit',
      'priority': 'high',
      'estimatedDeliveryTime': '2025-08-14T20:00:00Z',
      'notes': 'Hàng dễ vỡ, cần cẩn thận',
      'paymentMethod': 'cod',
      'shippingFee': 65000,
      'createdAt': '2025-08-14T11:45:00Z',
    },
  ];

  // Mock Route Data
  final Map<String, dynamic> _mockRoute = {
    'id': 'ROUTE-2025-0814-001',
    'driverId': 'DRV001',
    'date': '2025-08-14',
    'status': 'active',
    'totalOrders': 3,
    'completedOrders': 1,
    'estimatedDistance': 15.7, // km
    'estimatedDuration': 120, // minutes
    'actualDistance': 8.5,
    'actualDuration': 65,
    'optimizedWaypoints': [
      {'lat': 10.7769, 'lng': 106.7009, 'orderId': 'KTC-2025-001'},
      {'lat': 10.7700, 'lng': 106.6950, 'orderId': 'KTC-2025-002'},
      {'lat': 10.7750, 'lng': 106.7000, 'orderId': 'KTC-2025-003'},
    ],
    'startTime': '2025-08-14T08:00:00Z',
    'endTime': null,
  };

  // Mock Delivery History
  final List<Map<String, dynamic>> _mockDeliveryHistory = [
    {
      'id': 'KTC-2025-H001',
      'customerName': 'Phạm Văn Tài',
      'deliveryAddress': '99 Đường Nguyễn Thái Học, Q.1',
      'completedAt': '2025-08-13T17:30:00Z',
      'rating': 5,
      'feedback': 'Giao hàng nhanh, shipper thân thiện!',
      'earnings': 45000,
    },
    {
      'id': 'KTC-2025-H002', 
      'customerName': 'Nguyễn Thị Hoa',
      'deliveryAddress': '55 Đường Hai Bà Trưng, Q.3',
      'completedAt': '2025-08-13T15:15:00Z',
      'rating': 4,
      'feedback': 'Tốt, giao đúng giờ',
      'earnings': 35000,
    },
    {
      'id': 'KTC-2025-H003',
      'customerName': 'Võ Minh Đức',
      'deliveryAddress': '77 Đường Cách Mạng Tháng 8, Q.10',
      'completedAt': '2025-08-13T12:45:00Z',
      'rating': 5,
      'feedback': 'Xuất sắc!',
      'earnings': 55000,
    },
  ];

  Future<List<Map<String, dynamic>>> getActiveOrders() async {
    await Future.delayed(const Duration(seconds: 1));
    return _mockOrders.where((order) => 
      order['status'] != 'delivered' && order['status'] != 'cancelled'
    ).toList();
  }

  Future<Map<String, dynamic>> getOrderById(String orderId) async {
    await Future.delayed(const Duration(milliseconds: 500));
    final order = _mockOrders.firstWhere(
      (order) => order['id'] == orderId,
      orElse: () => throw Exception('Không tìm thấy đơn hàng'),
    );
    return order;
  }

  Future<Map<String, dynamic>> getCurrentRoute() async {
    await Future.delayed(const Duration(milliseconds: 800));
    return _mockRoute;
  }

  Future<List<Map<String, dynamic>>> getDeliveryHistory({
    int limit = 20,
    int offset = 0,
  }) async {
    await Future.delayed(const Duration(milliseconds: 600));
    final end = (offset + limit).clamp(0, _mockDeliveryHistory.length);
    return _mockDeliveryHistory.sublist(offset, end);
  }

  Future<void> updateOrderStatus(String orderId, String newStatus) async {
    await Future.delayed(const Duration(seconds: 1));
    final orderIndex = _mockOrders.indexWhere((order) => order['id'] == orderId);
    if (orderIndex != -1) {
      _mockOrders[orderIndex]['status'] = newStatus;
      _mockOrders[orderIndex]['updatedAt'] = DateTime.now().toIso8601String();
    }
  }

  Future<Map<String, dynamic>> confirmPickup(String orderId) async {
    await Future.delayed(const Duration(seconds: 1));
    await updateOrderStatus(orderId, 'picked_up');
    return {'success': true, 'message': 'Đã xác nhận lấy hàng thành công'};
  }

  Future<Map<String, dynamic>> confirmDelivery(
    String orderId, {
    String? signature,
    String? photo,
    String? notes,
  }) async {
    await Future.delayed(const Duration(seconds: 1));
    await updateOrderStatus(orderId, 'delivered');
    return {
      'success': true, 
      'message': 'Đã xác nhận giao hàng thành công',
      'deliveredAt': DateTime.now().toIso8601String(),
    };
  }

  Future<Map<String, dynamic>> reportProblem(
    String orderId, {
    required String problemType,
    required String description,
    String? photo,
  }) async {
    await Future.delayed(const Duration(seconds: 1));
    return {
      'success': true,
      'message': 'Đã báo cáo sự cố thành công',
      'ticketId': 'TICKET-${DateTime.now().millisecondsSinceEpoch}',
    };
  }

  Future<Map<String, dynamic>> getDriverLocation() async {
    await Future.delayed(const Duration(milliseconds: 300));
    return {
      'lat': 10.7769,
      'lng': 106.7009,
      'accuracy': 5.0,
      'timestamp': DateTime.now().toIso8601String(),
    };
  }

  Future<List<Map<String, dynamic>>> getOptimizedRoute(
    List<Map<String, dynamic>> waypoints,
  ) async {
    await Future.delayed(const Duration(seconds: 2));
    // Return optimized waypoints (mock)
    return waypoints..shuffle();
  }

  Future<Map<String, dynamic>> getDashboardData() async {
    await Future.delayed(const Duration(milliseconds: 1000));
    return {
      'todayStats': {
        'totalOrders': 3,
        'completedOrders': 1,
        'pendingOrders': 2,
        'earnings': 135000,
        'distance': 8.5,
        'workingHours': 3.5,
      },
      'monthlyStats': {
        'totalOrders': 156,
        'completedOrders': 148,
        'earnings': 6750000,
        'rating': 4.8,
        'onTimeRate': 95.2,
      },
      'recentActivity': [
        {
          'type': 'delivery_completed',
          'orderId': 'KTC-2025-H001',
          'timestamp': '2025-08-13T17:30:00Z',
          'message': 'Hoàn thành giao hàng #KTC-2025-H001',
        },
        {
          'type': 'pickup_confirmed', 
          'orderId': 'KTC-2025-002',
          'timestamp': '2025-08-14T10:30:00Z',
          'message': 'Đã xác nhận lấy hàng #KTC-2025-002',
        },
      ],
    };
  }
}


