// driver_analytics.dart
// Model cho thông tin phân tích của tài xế

class DriverAnalytics {
  final int totalDeliveries;
  final int completedDeliveries;
  final int canceledDeliveries;
  final int lateDeliveries;
  final double averageRating;
  final double averageDeliveryTime;
  final double fuelConsumption;
  final double distanceCovered;
  final double totalEarnings;
  final Map<String, double>? performanceByDay;
  final Map<String, int>? deliveriesByStatus;

  DriverAnalytics({
    required this.totalDeliveries,
    required this.completedDeliveries,
    required this.canceledDeliveries,
    required this.lateDeliveries,
    required this.averageRating,
    required this.averageDeliveryTime,
    required this.fuelConsumption,
    required this.distanceCovered,
    required this.totalEarnings,
    this.performanceByDay,
    this.deliveriesByStatus,
  });

  factory DriverAnalytics.fromJson(Map<String, dynamic> json) {
    Map<String, double>? performanceByDay;
    if (json['performanceByDay'] != null) {
      performanceByDay = {};
      json['performanceByDay'].forEach((key, value) {
        performanceByDay![key] = value.toDouble();
      });
    }

    Map<String, int>? deliveriesByStatus;
    if (json['deliveriesByStatus'] != null) {
      deliveriesByStatus = {};
      json['deliveriesByStatus'].forEach((key, value) {
        deliveriesByStatus![key] = value as int;
      });
    }

    return DriverAnalytics(
      totalDeliveries: json['totalDeliveries'] ?? 0,
      completedDeliveries: json['completedDeliveries'] ?? 0,
      canceledDeliveries: json['canceledDeliveries'] ?? 0,
      lateDeliveries: json['lateDeliveries'] ?? 0,
      averageRating: json['averageRating']?.toDouble() ?? 0.0,
      averageDeliveryTime: json['averageDeliveryTime']?.toDouble() ?? 0.0,
      fuelConsumption: json['fuelConsumption']?.toDouble() ?? 0.0,
      distanceCovered: json['distanceCovered']?.toDouble() ?? 0.0,
      totalEarnings: json['totalEarnings']?.toDouble() ?? 0.0,
      performanceByDay: performanceByDay,
      deliveriesByStatus: deliveriesByStatus,
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'totalDeliveries': totalDeliveries,
      'completedDeliveries': completedDeliveries,
      'canceledDeliveries': canceledDeliveries,
      'lateDeliveries': lateDeliveries,
      'averageRating': averageRating,
      'averageDeliveryTime': averageDeliveryTime,
      'fuelConsumption': fuelConsumption,
      'distanceCovered': distanceCovered,
      'totalEarnings': totalEarnings,
      'performanceByDay': performanceByDay,
      'deliveriesByStatus': deliveriesByStatus,
    };
  }
}
