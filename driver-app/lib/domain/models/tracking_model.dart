class TrackingPoint {
  final double latitude;
  final double longitude;
  final String driverId;
  final String? vehicleId;
  final DateTime timestamp;
  final double? speed;
  final double? heading;
  final double? accuracy;
  final double? altitude;
  final String? status;
  
  TrackingPoint({
    required this.latitude,
    required this.longitude,
    required this.driverId,
    this.vehicleId,
    required this.timestamp,
    this.speed,
    this.heading,
    this.accuracy,
    this.altitude,
    this.status,
  });
  
  factory TrackingPoint.fromJson(Map<String, dynamic> json) {
    return TrackingPoint(
      latitude: json['latitude'],
      longitude: json['longitude'],
      driverId: json['driverId'],
      vehicleId: json['vehicleId'],
      timestamp: DateTime.parse(json['timestamp']),
      speed: json['speed'],
      heading: json['heading'],
      accuracy: json['accuracy'],
      altitude: json['altitude'],
      status: json['status'],
    );
  }
  
  Map<String, dynamic> toJson() {
    return {
      'latitude': latitude,
      'longitude': longitude,
      'driverId': driverId,
      'vehicleId': vehicleId,
      'timestamp': timestamp.toIso8601String(),
      'speed': speed,
      'heading': heading,
      'accuracy': accuracy,
      'altitude': altitude,
      'status': status,
    };
  }
}

class LocationUpdateResponse {
  final bool success;
  final String message;
  final TrackingPoint? trackingPoint;
  
  LocationUpdateResponse({
    required this.success,
    required this.message,
    this.trackingPoint,
  });
  
  factory LocationUpdateResponse.fromJson(Map<String, dynamic> json) {
    return LocationUpdateResponse(
      success: json['success'],
      message: json['message'],
      trackingPoint: json['trackingPoint'] != null
          ? TrackingPoint.fromJson(json['trackingPoint'])
          : null,
    );
  }
}

class VehicleLocationResponse {
  final String vehicleId;
  final String driverId;
  final String driverName;
  final double latitude;
  final double longitude;
  final DateTime timestamp;
  final double? speed;
  final String? status;
  
  VehicleLocationResponse({
    required this.vehicleId,
    required this.driverId,
    required this.driverName,
    required this.latitude,
    required this.longitude,
    required this.timestamp,
    this.speed,
    this.status,
  });
  
  factory VehicleLocationResponse.fromJson(Map<String, dynamic> json) {
    return VehicleLocationResponse(
      vehicleId: json['vehicleId'],
      driverId: json['driverId'],
      driverName: json['driverName'],
      latitude: json['latitude'],
      longitude: json['longitude'],
      timestamp: DateTime.parse(json['timestamp']),
      speed: json['speed'],
      status: json['status'],
    );
  }
}

class TrackingHistoryResponse {
  final List<TrackingPoint> trackingPoints;
  final int totalPoints;
  final String timespan;
  
  TrackingHistoryResponse({
    required this.trackingPoints,
    required this.totalPoints,
    required this.timespan,
  });
  
  factory TrackingHistoryResponse.fromJson(Map<String, dynamic> json) {
    final List<dynamic> points = json['trackingPoints'];
    
    return TrackingHistoryResponse(
      trackingPoints: points.map((point) => TrackingPoint.fromJson(point)).toList(),
      totalPoints: json['totalPoints'],
      timespan: json['timespan'],
    );
  }
}

class RouteTrackingResponse {
  final String routeId;
  final String status;
  final DateTime startTime;
  final DateTime? endTime;
  final List<TrackingPoint> trackingPoints;
  final double totalDistance;
  final Duration totalDuration;
  final List<String> deliveryIds;
  
  RouteTrackingResponse({
    required this.routeId,
    required this.status,
    required this.startTime,
    this.endTime,
    required this.trackingPoints,
    required this.totalDistance,
    required this.totalDuration,
    required this.deliveryIds,
  });
  
  factory RouteTrackingResponse.fromJson(Map<String, dynamic> json) {
    final List<dynamic> points = json['trackingPoints'];
    final List<dynamic> deliveries = json['deliveryIds'];
    
    return RouteTrackingResponse(
      routeId: json['routeId'],
      status: json['status'],
      startTime: DateTime.parse(json['startTime']),
      endTime: json['endTime'] != null ? DateTime.parse(json['endTime']) : null,
      trackingPoints: points.map((point) => TrackingPoint.fromJson(point)).toList(),
      totalDistance: json['totalDistance'],
      totalDuration: Duration(seconds: json['totalDurationSeconds']),
      deliveryIds: deliveries.map((id) => id.toString()).toList(),
    );
  }
}

class OrderTrackingResponse {
  final String orderId;
  final String status;
  final DateTime createdAt;
  final DateTime? completedAt;
  final List<TrackingPoint> trackingPoints;
  final List<TrackingEvent> events;
  
  OrderTrackingResponse({
    required this.orderId,
    required this.status,
    required this.createdAt,
    this.completedAt,
    required this.trackingPoints,
    required this.events,
  });
  
  factory OrderTrackingResponse.fromJson(Map<String, dynamic> json) {
    final List<dynamic> points = json['trackingPoints'];
    final List<dynamic> eventsList = json['events'];
    
    return OrderTrackingResponse(
      orderId: json['orderId'],
      status: json['status'],
      createdAt: DateTime.parse(json['createdAt']),
      completedAt: json['completedAt'] != null ? DateTime.parse(json['completedAt']) : null,
      trackingPoints: points.map((point) => TrackingPoint.fromJson(point)).toList(),
      events: eventsList.map((event) => TrackingEvent.fromJson(event)).toList(),
    );
  }
}

class TrackingEvent {
  final String eventType;
  final DateTime timestamp;
  final String description;
  final Map<String, dynamic> metadata;
  
  TrackingEvent({
    required this.eventType,
    required this.timestamp,
    required this.description,
    required this.metadata,
  });
  
  factory TrackingEvent.fromJson(Map<String, dynamic> json) {
    return TrackingEvent(
      eventType: json['eventType'],
      timestamp: DateTime.parse(json['timestamp']),
      description: json['description'],
      metadata: json['metadata'],
    );
  }
  
  Map<String, dynamic> toJson() {
    return {
      'eventType': eventType,
      'timestamp': timestamp.toIso8601String(),
      'description': description,
      'metadata': metadata,
    };
  }
}

class DeliveryTrackingResponse {
  final String deliveryId;
  final String status;
  final DateTime startTime;
  final DateTime? endTime;
  final List<TrackingPoint> trackingPoints;
  final List<TrackingEvent> events;
  
  DeliveryTrackingResponse({
    required this.deliveryId,
    required this.status,
    required this.startTime,
    this.endTime,
    required this.trackingPoints,
    required this.events,
  });
  
  factory DeliveryTrackingResponse.fromJson(Map<String, dynamic> json) {
    final List<dynamic> points = json['trackingPoints'];
    final List<dynamic> eventsList = json['events'];
    
    return DeliveryTrackingResponse(
      deliveryId: json['deliveryId'],
      status: json['status'],
      startTime: DateTime.parse(json['startTime']),
      endTime: json['endTime'] != null ? DateTime.parse(json['endTime']) : null,
      trackingPoints: points.map((point) => TrackingPoint.fromJson(point)).toList(),
      events: eventsList.map((event) => TrackingEvent.fromJson(event)).toList(),
    );
  }
}

class RoutesResponse {
  final List<RouteInfo> routes;
  final int totalRoutes;
  final int currentPage;
  final int totalPages;
  
  RoutesResponse({
    required this.routes,
    required this.totalRoutes,
    required this.currentPage,
    required this.totalPages,
  });
  
  factory RoutesResponse.fromJson(Map<String, dynamic> json) {
    final List<dynamic> routesList = json['routes'];
    
    return RoutesResponse(
      routes: routesList.map((route) => RouteInfo.fromJson(route)).toList(),
      totalRoutes: json['totalRoutes'],
      currentPage: json['currentPage'],
      totalPages: json['totalPages'],
    );
  }
}

class RouteInfo {
  final String routeId;
  final String name;
  final String status;
  final DateTime startTime;
  final DateTime? endTime;
  final int deliveryCount;
  final double totalDistance;
  final Duration estimatedDuration;
  
  RouteInfo({
    required this.routeId,
    required this.name,
    required this.status,
    required this.startTime,
    this.endTime,
    required this.deliveryCount,
    required this.totalDistance,
    required this.estimatedDuration,
  });
  
  factory RouteInfo.fromJson(Map<String, dynamic> json) {
    return RouteInfo(
      routeId: json['routeId'],
      name: json['name'],
      status: json['status'],
      startTime: DateTime.parse(json['startTime']),
      endTime: json['endTime'] != null ? DateTime.parse(json['endTime']) : null,
      deliveryCount: json['deliveryCount'],
      totalDistance: json['totalDistance'],
      estimatedDuration: Duration(seconds: json['estimatedDurationSeconds']),
    );
  }
}

class RouteResponse {
  final RouteDetail route;
  
  RouteResponse({
    required this.route,
  });
  
  factory RouteResponse.fromJson(Map<String, dynamic> json) {
    return RouteResponse(
      route: RouteDetail.fromJson(json['route']),
    );
  }
}

class RouteDetail {
  final String routeId;
  final String name;
  final String status;
  final DateTime startTime;
  final DateTime? endTime;
  final List<String> deliveryIds;
  final double totalDistance;
  final Duration estimatedDuration;
  final String? vehicleId;
  final String? driverId;
  final List<RoutePoint> waypoints;
  
  RouteDetail({
    required this.routeId,
    required this.name,
    required this.status,
    required this.startTime,
    this.endTime,
    required this.deliveryIds,
    required this.totalDistance,
    required this.estimatedDuration,
    this.vehicleId,
    this.driverId,
    required this.waypoints,
  });
  
  factory RouteDetail.fromJson(Map<String, dynamic> json) {
    final List<dynamic> deliveries = json['deliveryIds'];
    final List<dynamic> points = json['waypoints'];
    
    return RouteDetail(
      routeId: json['routeId'],
      name: json['name'],
      status: json['status'],
      startTime: DateTime.parse(json['startTime']),
      endTime: json['endTime'] != null ? DateTime.parse(json['endTime']) : null,
      deliveryIds: deliveries.map((id) => id.toString()).toList(),
      totalDistance: json['totalDistance'],
      estimatedDuration: Duration(seconds: json['estimatedDurationSeconds']),
      vehicleId: json['vehicleId'],
      driverId: json['driverId'],
      waypoints: points.map((point) => RoutePoint.fromJson(point)).toList(),
    );
  }
}

class RoutePoint {
  final double latitude;
  final double longitude;
  final String type;
  final String? deliveryId;
  final int order;
  final double? distanceFromPrevious;
  final Duration? durationFromPrevious;
  
  RoutePoint({
    required this.latitude,
    required this.longitude,
    required this.type,
    this.deliveryId,
    required this.order,
    this.distanceFromPrevious,
    this.durationFromPrevious,
  });
  
  factory RoutePoint.fromJson(Map<String, dynamic> json) {
    return RoutePoint(
      latitude: json['latitude'],
      longitude: json['longitude'],
      type: json['type'],
      deliveryId: json['deliveryId'],
      order: json['order'],
      distanceFromPrevious: json['distanceFromPrevious'],
      durationFromPrevious: json['durationFromPrevious'] != null 
          ? Duration(seconds: json['durationFromPreviousSeconds']) 
          : null,
    );
  }
}


