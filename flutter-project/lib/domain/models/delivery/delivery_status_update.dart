// delivery_status_update.dart
// Model cho cập nhật trạng thái giao hàng

class DeliveryStatusUpdate {
  final String status;
  final String? notes;
  final String? proofOfDeliveryUrl;
  final String? reasonForFailure;
  final String timestamp;
  final double? latitude;
  final double? longitude;

  DeliveryStatusUpdate({
    required this.status,
    this.notes,
    this.proofOfDeliveryUrl,
    this.reasonForFailure,
    required this.timestamp,
    this.latitude,
    this.longitude,
  });

  Map<String, dynamic> toJson() {
    final Map<String, dynamic> data = {
      'status': status,
      'timestamp': timestamp,
    };

    if (notes != null) data['notes'] = notes;
    if (proofOfDeliveryUrl != null) data['proofOfDeliveryUrl'] = proofOfDeliveryUrl;
    if (reasonForFailure != null) data['reasonForFailure'] = reasonForFailure;
    if (latitude != null) data['latitude'] = latitude;
    if (longitude != null) data['longitude'] = longitude;

    return data;
  }
  
  factory DeliveryStatusUpdate.fromJson(Map<String, dynamic> json) {
    return DeliveryStatusUpdate(
      status: json['status'],
      notes: json['notes'],
      proofOfDeliveryUrl: json['proofOfDeliveryUrl'],
      reasonForFailure: json['reasonForFailure'],
      timestamp: json['timestamp'],
      latitude: json['latitude']?.toDouble(),
      longitude: json['longitude']?.toDouble(),
    );
  }
}

// Enum cho các trạng thái giao hàng
class DeliveryStatus {
  static const String ASSIGNED = 'ASSIGNED';
  static const String IN_TRANSIT = 'IN_TRANSIT';
  static const String DELIVERED = 'DELIVERED';
  static const String FAILED = 'FAILED';
  static const String CANCELLED = 'CANCELLED';
  static const String RETURNED = 'RETURNED';
}
