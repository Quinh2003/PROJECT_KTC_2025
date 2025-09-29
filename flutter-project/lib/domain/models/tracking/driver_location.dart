// driver_location.dart
// Model cho thông tin vị trí của tài xế

class DriverLocation {
  final double latitude;
  final double longitude;
  final String timestamp;
  final double speed;
  final double heading;
  final String vehicleStatus;

  DriverLocation({
    required this.latitude,
    required this.longitude,
    required this.timestamp,
    required this.speed,
    required this.heading,
    required this.vehicleStatus,
  });

  factory DriverLocation.fromJson(Map<String, dynamic> json) {
    return DriverLocation(
      latitude: json['latitude'] ?? 0.0,
      longitude: json['longitude'] ?? 0.0,
      timestamp: json['timestamp'] ?? DateTime.now().toIso8601String(),
      speed: json['speed'] ?? 0.0,
      heading: json['heading'] ?? 0.0,
      vehicleStatus: json['vehicleStatus'] ?? 'IDLE',
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'latitude': latitude,
      'longitude': longitude,
      'timestamp': timestamp,
      'speed': speed,
      'heading': heading,
      'vehicleStatus': vehicleStatus,
    };
  }
  
  static List<DriverLocation> fromJsonList(List<dynamic> jsonList) {
    return jsonList.map((item) => DriverLocation.fromJson(item)).toList();
  }
}
