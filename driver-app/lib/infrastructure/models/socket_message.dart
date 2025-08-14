class SocketMessage {
  final String type;
  final Map<String, dynamic> data;
  final DateTime timestamp;
  
  SocketMessage({
    required this.type,
    required this.data,
    required this.timestamp,
  });
  
  factory SocketMessage.fromJson(Map<String, dynamic> json) {
    return SocketMessage(
      type: json['type'],
      data: json['data'] ?? {},
      timestamp: DateTime.parse(json['timestamp']),
    );
  }
  
  Map<String, dynamic> toJson() {
    return {
      'type': type,
      'data': data,
      'timestamp': timestamp.toIso8601String(),
    };
  }
}

class DriverStatusMessage extends SocketMessage {
  String get driverId => data['driverId'];
  String get status => data['status'];
  
  DriverStatusMessage({
    required String driverId,
    required String status,
    DateTime? timestamp,
  }) : super(
    type: 'driver_status',
    data: {
      'driverId': driverId,
      'status': status,
    },
    timestamp: timestamp ?? DateTime.now(),
  );
}

class DeliveryStatusMessage extends SocketMessage {
  String get deliveryId => data['deliveryId'];
  String get status => data['status'];
  
  DeliveryStatusMessage({
    required String deliveryId,
    required String status,
    DateTime? timestamp,
  }) : super(
    type: 'delivery_status',
    data: {
      'deliveryId': deliveryId,
      'status': status,
    },
    timestamp: timestamp ?? DateTime.now(),
  );
}

class LocationUpdateMessage extends SocketMessage {
  double get latitude => data['latitude'];
  double get longitude => data['longitude'];
  String? get driverId => data['driverId'];
  String? get vehicleId => data['vehicleId'];
  double? get speed => data['speed'];
  double? get heading => data['heading'];
  
  LocationUpdateMessage({
    required double latitude,
    required double longitude,
    String? driverId,
    String? vehicleId,
    double? speed,
    double? heading,
    DateTime? timestamp,
  }) : super(
    type: 'location_update',
    data: {
      'latitude': latitude,
      'longitude': longitude,
      'driverId': driverId,
      'vehicleId': vehicleId,
      'speed': speed,
      'heading': heading,
    },
    timestamp: timestamp ?? DateTime.now(),
  );
}

class RouteAssignmentMessage extends SocketMessage {
  String get routeId => data['routeId'];
  String get driverId => data['driverId'];
  String? get vehicleId => data['vehicleId'];
  
  RouteAssignmentMessage({
    required String routeId,
    required String driverId,
    String? vehicleId,
    DateTime? timestamp,
  }) : super(
    type: 'route_assignment',
    data: {
      'routeId': routeId,
      'driverId': driverId,
      'vehicleId': vehicleId,
    },
    timestamp: timestamp ?? DateTime.now(),
  );
}

class OrderUpdateMessage extends SocketMessage {
  String get orderId => data['orderId'];
  String get status => data['status'];
  Map<String, dynamic>? get orderDetails => data['orderDetails'];
  
  OrderUpdateMessage({
    required String orderId,
    required String status,
    Map<String, dynamic>? orderDetails,
    DateTime? timestamp,
  }) : super(
    type: 'order_update',
    data: {
      'orderId': orderId,
      'status': status,
      'orderDetails': orderDetails,
    },
    timestamp: timestamp ?? DateTime.now(),
  );
}

class DeliveryAssignmentMessage extends SocketMessage {
  String get deliveryId => data['deliveryId'];
  String get routeId => data['routeId'];
  String? get orderId => data['orderId'];
  int? get sequenceNumber => data['sequenceNumber'];
  
  DeliveryAssignmentMessage({
    required String deliveryId,
    required String routeId,
    String? orderId,
    int? sequenceNumber,
    DateTime? timestamp,
  }) : super(
    type: 'delivery_assignment',
    data: {
      'deliveryId': deliveryId,
      'routeId': routeId,
      'orderId': orderId,
      'sequenceNumber': sequenceNumber,
    },
    timestamp: timestamp ?? DateTime.now(),
  );
}

class MessageNotification extends SocketMessage {
  String get title => data['title'];
  String get body => data['body'];
  String? get senderId => data['senderId'];
  Map<String, dynamic>? get payload => data['payload'];
  
  MessageNotification({
    required String title,
    required String body,
    String? senderId,
    Map<String, dynamic>? payload,
    DateTime? timestamp,
  }) : super(
    type: 'notification',
    data: {
      'title': title,
      'body': body,
      'senderId': senderId,
      'payload': payload,
    },
    timestamp: timestamp ?? DateTime.now(),
  );
}

class ErrorMessage extends SocketMessage {
  String get code => data['code'];
  String get message => data['message'];
  
  ErrorMessage({
    required String code,
    required String message,
    DateTime? timestamp,
  }) : super(
    type: 'error',
    data: {
      'code': code,
      'message': message,
    },
    timestamp: timestamp ?? DateTime.now(),
  );
}

class PingMessage extends SocketMessage {
  PingMessage({
    DateTime? timestamp,
  }) : super(
    type: 'ping',
    data: {},
    timestamp: timestamp ?? DateTime.now(),
  );
}

class PongMessage extends SocketMessage {
  PongMessage({
    DateTime? timestamp,
  }) : super(
    type: 'pong',
    data: {},
    timestamp: timestamp ?? DateTime.now(),
  );
}

class AuthMessage extends SocketMessage {
  String get token => data['token'];
  
  AuthMessage({
    required String token,
    DateTime? timestamp,
  }) : super(
    type: 'auth',
    data: {
      'token': token,
    },
    timestamp: timestamp ?? DateTime.now(),
  );
}


