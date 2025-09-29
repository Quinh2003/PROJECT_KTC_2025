import 'package:ktc_logistics_driver/domain/models/delivery/driver_delivery.dart';

/// Mock Data Service - Cung cấp dữ liệu giả lập cho toàn bộ ứng dụng
class MockDataService {
  static final MockDataService _instance = MockDataService._internal();
  factory MockDataService() => _instance;
  MockDataService._internal();

  // Dữ liệu đơn hàng dạng Map
  final List<Map<String, dynamic>> _mockOrders = [
    {
      'id': 'KTC-2025-001',
      'customerName': 'Nguyễn Văn Minh',
      'customerPhone': '+84 901 234 567',
      'pickupAddress': {'fullAddress': '123 Đường Lê Duẩn, Quận 1, TP.HCM'},
      'deliveryAddress': {'fullAddress': '456 Đường Nguyễn Huệ, Quận 1, TP.HCM'},
      'items': [
        {'name': 'Điện thoại iPhone 15', 'quantity': 1},
        {'name': 'Tai nghe AirPods', 'quantity': 1},
      ],
      'totalValue': 35000000,
      'status': 'pending',
      'priority': 'high',
      'shippingFee': 50000,
    },
    {
      'id': 'KTC-2025-002',
      'customerName': 'Trần Thị Lan Anh',
      'customerPhone': '+84 902 345 678',
      'pickupAddress': {'fullAddress': '789 Đường Pasteur, Quận 3, TP.HCM'},
      'deliveryAddress': {'fullAddress': '321 Đường Võ Văn Tần, Quận 3, TP.HCM'},
      'items': [
        {'name': 'Laptop Dell Inspiron', 'quantity': 1},
        {'name': 'Chuột không dây', 'quantity': 1},
      ],
      'totalValue': 18000000,
      'status': 'pickup',
      'priority': 'medium',
      'shippingFee': 75000,
    },
    {
      'id': 'KTC-2025-003',
      'customerName': 'Lê Hoàng Nam',
      'customerPhone': '+84 903 456 789',
      'pickupAddress': {'fullAddress': '147 Đường Hàm Nghi, Quận 1, TP.HCM'},
      'deliveryAddress': {'fullAddress': '258 Đường Điện Biên Phủ, Quận Bình Thạnh, TP.HCM'},
      'items': [
        {'name': 'Máy ảnh Canon EOS', 'quantity': 1},
        {'name': 'Lens 50mm', 'quantity': 1},
      ],
      'totalValue': 25000000,
      'status': 'in_transit',
      'priority': 'high',
      'shippingFee': 65000,
    },
  ];

  // Lịch sử giao hàng đã hoàn thành
  final List<Map<String, dynamic>> _mockDeliveryHistory = [
    {
      'id': 'KTC-2025-H001',
      'customerName': 'Phạm Văn Tài',
      'deliveryAddress': '99 Đường Nguyễn Thái Học, Q.1',
      'completedAt': '2025-08-13T17:30:00Z',
      'rating': 5,
      'earnings': 45000,
    },
    {
      'id': 'KTC-2025-H002', 
      'customerName': 'Nguyễn Thị Hoa',
      'deliveryAddress': '55 Đường Hai Bà Trưng, Q.3',
      'completedAt': '2025-08-13T15:15:00Z',
      'rating': 4,
      'earnings': 35000,
    },
  ];

  // Danh sách giao hàng dạng model - dùng cho DeliveriesScreen
  final List<DriverDelivery> _mockDeliveries = [
    DriverDelivery(
      id: 1001,
      orderId: 5001,
      orderNumber: 'KTC-5001',
      orderDescription: 'Electronics Package',
      statusId: 2,
      deliveryStatus: 'assigned',
      deliveryFee: 45000,
      transportMode: 'motorcycle',
      serviceType: 'express',
      pickupDate: '2025-09-09T08:30:00',
      scheduleDeliveryTime: '2025-09-09T10:30:00',
      actualDeliveryTime: null,
      lateDeliveryRisk: '0',
      deliveryAttempts: 0,
      deliveryNotes: 'Call customer before arriving',
      orderDate: '2025-09-08T15:45:00',
      vehicleId: 3001,
      vehicleLicensePlate: '59P1-12345',
      vehicleType: 'Honda Wave',
      driverId: 2001,
      driverName: 'Nguyễn Văn A',
      driverPhone: '0901234567',
      deliveryAddress: '123 Nguyễn Huệ, Quận 1, TP.HCM',
      trackingId: 'TRK-1001',
      currentLocation: '10.7731,106.7031',
      routeId: 4001,
      routeName: 'Route A-1',
      estimatedDistance: '5.2 km',
      estimatedDuration: '15 mins',
      createdAt: '2025-09-08T15:45:00',
      updatedAt: '2025-09-09T08:30:00',
      completed: false,
      pending: false,
      onTime: true,
      highRisk: false,
      serviceTypeDisplay: 'Express',
      transportModeDisplay: 'Motorcycle',
      delayed: false,
      inTransit: false,
      statusDisplay: 'Assigned',
      vehicleDisplay: 'Honda Wave (59P1-12345)',
      driverDisplay: 'Nguyễn Văn A',
      formattedFee: '45,000 VND',
      estimatedInfo: '5.2 km, 15 mins',
    ),
    DriverDelivery(
      id: 1002,
      orderId: 5002,
      orderNumber: 'KTC-5002',
      orderDescription: 'Furniture Delivery',
      statusId: 3,
      deliveryStatus: 'in_progress',
      deliveryFee: 60000,
      transportMode: 'van',
      serviceType: 'standard',
      pickupDate: '2025-09-09T09:15:00',
      scheduleDeliveryTime: '2025-09-09T14:00:00',
      actualDeliveryTime: null,
      lateDeliveryRisk: '25',
      deliveryAttempts: 0,
      deliveryNotes: 'Fragile items',
      orderDate: '2025-09-08T10:30:00',
      vehicleId: 3002,
      vehicleLicensePlate: '59F2-56789',
      vehicleType: 'Ford Transit',
      driverId: 2001,
      driverName: 'Nguyễn Văn A',
      driverPhone: '0901234567',
      deliveryAddress: '456 Lê Lợi, Quận 3, TP.HCM',
      trackingId: 'TRK-1002',
      currentLocation: '10.7800,106.6880',
      routeId: 4002,
      routeName: 'Route A-2',
      estimatedDistance: '8.7 km',
      estimatedDuration: '30 mins',
      createdAt: '2025-09-08T10:30:00',
      updatedAt: '2025-09-09T09:15:00',
      completed: false,
      pending: false,
      onTime: false,
      highRisk: false,
      serviceTypeDisplay: 'Standard',
      transportModeDisplay: 'Van',
      delayed: true,
      inTransit: true,
      statusDisplay: 'In Progress',
      vehicleDisplay: 'Ford Transit (59F2-56789)',
      driverDisplay: 'Nguyễn Văn A',
      formattedFee: '60,000 VND',
      estimatedInfo: '8.7 km, 30 mins',
    ),
    DriverDelivery(
      id: 1003,
      orderId: 5003,
      orderNumber: 'KTC-5003',
      orderDescription: 'Document Package',
      statusId: 5,
      deliveryStatus: 'completed',
      deliveryFee: 35000,
      transportMode: 'motorcycle',
      serviceType: 'express',
      pickupDate: '2025-09-08T13:00:00',
      scheduleDeliveryTime: '2025-09-08T14:30:00',
      actualDeliveryTime: '2025-09-08T14:15:00',
      lateDeliveryRisk: '0',
      deliveryAttempts: 1,
      deliveryNotes: null,
      orderDate: '2025-09-08T09:00:00',
      vehicleId: 3001,
      vehicleLicensePlate: '59P1-12345',
      vehicleType: 'Honda Wave',
      driverId: 2001,
      driverName: 'Nguyễn Văn A',
      driverPhone: '0901234567',
      deliveryAddress: '789 Điện Biên Phủ, Quận Bình Thạnh, TP.HCM',
      trackingId: 'TRK-1003',
      currentLocation: '10.8011,106.7183',
      routeId: 4003,
      routeName: 'Route A-3',
      estimatedDistance: '3.5 km',
      estimatedDuration: '12 mins',
      createdAt: '2025-09-08T09:00:00',
      updatedAt: '2025-09-08T14:15:00',
      completed: true,
      pending: false,
      onTime: true,
      highRisk: false,
      serviceTypeDisplay: 'Express',
      transportModeDisplay: 'Motorcycle',
      delayed: false,
      inTransit: false,
      statusDisplay: 'Completed',
      vehicleDisplay: 'Honda Wave (59P1-12345)',
      driverDisplay: 'Nguyễn Văn A',
      formattedFee: '35,000 VND',
      estimatedInfo: '3.5 km, 12 mins',
    ),
  ];

  // API cho DeliveriesScreen
  Future<List<DriverDelivery>> getMockDriverDeliveries() async {
    await Future.delayed(const Duration(milliseconds: 800));
    return _mockDeliveries;
  }

  Future<DriverDelivery> getDriverDeliveryById(int deliveryId) async {
    await Future.delayed(const Duration(milliseconds: 500));
    return _mockDeliveries.firstWhere(
      (delivery) => delivery.id == deliveryId,
      orElse: () => throw Exception('Delivery not found'),
    );
  }

  // API cho thông tin đơn hàng
  Future<Map<String, dynamic>> getOrderById(String orderId) async {
    await Future.delayed(const Duration(milliseconds: 500));
    return _mockOrders.firstWhere(
      (order) => order['id'] == orderId,
      orElse: () => throw Exception('Không tìm thấy đơn hàng'),
    );
  }

  // API cho lịch sử giao hàng
  Future<List<Map<String, dynamic>>> getDeliveryHistory({int limit = 20, int offset = 0}) async {
    await Future.delayed(const Duration(milliseconds: 600));
    final end = (offset + limit).clamp(0, _mockDeliveryHistory.length);
    return _mockDeliveryHistory.sublist(offset, end);
  }

  // API cập nhật trạng thái đơn hàng
  Future<Map<String, dynamic>> updateOrderStatus(String orderId, int statusId, String notes) async {
    await Future.delayed(const Duration(seconds: 1));
    final orderIndex = _mockOrders.indexWhere((order) => order['id'] == orderId);
    if (orderIndex != -1) {
      String newStatus;
      // Chuyển đổi statusId sang trạng thái tương ứng
      switch (statusId) {
        case 2: newStatus = 'assigned'; break;
        case 3: newStatus = 'in_progress'; break;
        case 4: newStatus = 'delivered'; break;
        case 5: newStatus = 'completed'; break;
        default: newStatus = 'pending';
      }
      _mockOrders[orderIndex]['status'] = newStatus;
      _mockOrders[orderIndex]['notes'] = notes;
    }
    return {'success': true, 'message': 'Cập nhật trạng thái thành công'};
  }

  // API xác nhận lấy hàng
  Future<Map<String, dynamic>> confirmPickup(String orderId) async {
    await Future.delayed(const Duration(seconds: 1));
    // Sử dụng mã trạng thái 3 (in_progress) cho đã lấy hàng
    return updateOrderStatus(orderId, 3, 'Đã lấy hàng thành công');
  }

  // API xác nhận giao hàng
  Future<Map<String, dynamic>> confirmDelivery(String orderId, {String? signature, String? photo}) async {
    await Future.delayed(const Duration(seconds: 1));
    // Sử dụng mã trạng thái 5 (completed) cho đã giao hàng
    return updateOrderStatus(orderId, 5, 'Đã giao hàng thành công');
  }

  // API báo cáo sự cố
  Future<Map<String, dynamic>> reportProblem(String orderId, {required String problemType, required String description}) async {
    await Future.delayed(const Duration(seconds: 1));
    return {
      'success': true,
      'message': 'Đã báo cáo sự cố thành công',
      'ticketId': 'TICKET-${DateTime.now().millisecondsSinceEpoch}',
    };
  }

  // API lấy dữ liệu dashboard
  Future<Map<String, dynamic>> getDashboardData() async {
    await Future.delayed(const Duration(milliseconds: 1000));
    return {
      'todayStats': {
        'totalOrders': 3,
        'completedOrders': 1,
        'pendingOrders': 2,
        'earnings': 135000,
      },
      'monthlyStats': {
        'totalOrders': 156,
        'completedOrders': 148,
        'earnings': 6750000,
        'rating': 4.8,
      },
    };
  }
}


