// driver_delivery.dart
// Model for driver deliveries list API response
import 'package:ktc_logistics_driver/domain/models/delivery/delivery.dart';

class DriverDelivery {
  final int id;
  final int orderId;
  final String? orderNumber;
  final String? orderDescription;
  final int? statusId;
  final String? deliveryStatus;
  final double deliveryFee;
  final String? transportMode;
  final String? serviceType;
  final String? pickupDate;
  final String? scheduleDeliveryTime;
  final String? actualDeliveryTime;
  final String? lateDeliveryRisk;
  final int? deliveryAttempts;
  final String? deliveryNotes;
  final String? orderDate;
  final int? vehicleId;
  final String? vehicleLicensePlate;
  final String? vehicleType;
  final int? driverId;
  final String? driverName;
  final String? driverPhone;
  final String? deliveryAddress;
  final String? trackingId;
  final String? currentLocation;
  final int? routeId;
  final String? routeName;
  final String? estimatedDistance;
  final String? estimatedDuration;
  final String? createdAt;
  final String? updatedAt;
  final bool completed;
  final bool pending;
  final bool onTime;
  final bool highRisk;
  final String serviceTypeDisplay;
  final String transportModeDisplay;
  final bool delayed;
  final bool inTransit;
  final String statusDisplay;
  final String vehicleDisplay;
  final String driverDisplay;
  final String formattedFee;
  final String estimatedInfo;

  DriverDelivery({
    required this.id,
    required this.orderId,
    this.orderNumber,
    this.orderDescription,
    this.statusId,
    this.deliveryStatus,
    required this.deliveryFee,
    this.transportMode,
    this.serviceType,
    this.pickupDate,
    this.scheduleDeliveryTime,
    this.actualDeliveryTime,
    this.lateDeliveryRisk,
    this.deliveryAttempts,
    this.deliveryNotes,
    this.orderDate,
    this.vehicleId,
    this.vehicleLicensePlate,
    this.vehicleType,
    this.driverId,
    this.driverName,
    this.driverPhone,
    this.deliveryAddress,
    this.trackingId,
    this.currentLocation,
    this.routeId,
    this.routeName,
    this.estimatedDistance,
    this.estimatedDuration,
    this.createdAt,
    this.updatedAt,
    required this.completed,
    required this.pending,
    required this.onTime,
    required this.highRisk,
    required this.serviceTypeDisplay,
    required this.transportModeDisplay,
    required this.delayed,
    required this.inTransit,
    required this.statusDisplay,
    required this.vehicleDisplay,
    required this.driverDisplay,
    required this.formattedFee,
    required this.estimatedInfo,
  });

  // Factory constructor for JSON parsing
  factory DriverDelivery.fromJson(Map<String, dynamic> json) {
    return DriverDelivery(
      id: json['id'] ?? 0,
      orderId: json['orderId'] ?? 0,
      orderNumber: json['orderNumber'],
      orderDescription: json['orderDescription'],
      statusId: json['statusId'],
      deliveryStatus: json['deliveryStatus'],
      deliveryFee: json['deliveryFee'] != null
          ? double.parse(json['deliveryFee'].toString())
          : 0.0,
      transportMode: json['transportMode'],
      serviceType: json['serviceType'],
      pickupDate: json['pickupDate'],
      scheduleDeliveryTime: json['scheduleDeliveryTime'],
      actualDeliveryTime: json['actualDeliveryTime'],
      lateDeliveryRisk: json['lateDeliveryRisk'],
      deliveryAttempts: json['deliveryAttempts'],
      deliveryNotes: json['deliveryNotes'],
      orderDate: json['orderDate'],
      vehicleId: json['vehicleId'],
      vehicleLicensePlate: json['vehicleLicensePlate'],
      vehicleType: json['vehicleType'],
      driverId: json['driverId'],
      driverName: json['driverName'],
      driverPhone: json['driverPhone'],
      deliveryAddress: json['deliveryAddress'],
      trackingId: json['trackingId'],
      currentLocation: json['currentLocation'],
      routeId: json['routeId'],
      routeName: json['routeName'],
      estimatedDistance: json['estimatedDistance'],
      estimatedDuration: json['estimatedDuration'],
      createdAt: json['createdAt'],
      updatedAt: json['updatedAt'],
      completed: json['completed'] ?? false,
      pending: json['pending'] ?? false,
      onTime: json['onTime'] ?? false,
      highRisk: json['highRisk'] ?? false,
      serviceTypeDisplay: json['serviceTypeDisplay'] ?? 'Standard',
      transportModeDisplay: json['transportModeDisplay'] ?? 'Road',
      delayed: json['delayed'] ?? false,
      inTransit: json['inTransit'] ?? false,
      statusDisplay: json['statusDisplay'] ?? 'Unknown',
      vehicleDisplay: json['vehicleDisplay'] ?? 'Unknown',
      driverDisplay: json['driverDisplay'] ?? 'Unknown',
      formattedFee: json['formattedFee'] ?? "\$0.00",
      estimatedInfo: json['estimatedInfo'] ?? 'No estimate',
    );
  }

  // Create a DriverDelivery from a Delivery model
  static DriverDelivery fromDelivery(Delivery delivery) {
    // Extract driver information
    final driverName = delivery.driverName ?? 'Unknown';
    final driverPhone = delivery.driverPhone ?? '';

    // Extract vehicle information
    final vehicleLicensePlate = delivery.vehicleLicensePlate ?? '';
    final vehicleType = delivery.vehicleType ?? '';
    final vehicleDisplay = '$vehicleType ($vehicleLicensePlate)'.trim();

    // Extract order information
    final orderNumber = delivery.orderNumber ?? '';
    final orderDescription = delivery.orderDescription ?? '';
    final deliveryAddress = delivery.deliveryAddress ?? '';

    // Extract route information
    final routeName = delivery.routeName ?? '';
    final estimatedDistance = delivery.estimatedDistance ?? '';
    final estimatedDuration = delivery.estimatedDuration ?? '';

    // Determine status display
    String statusDisplay = 'Pending';
    if (delivery.deliveryStatus != null) {
      switch (delivery.deliveryStatus!.toLowerCase()) {
        case 'assigned':
          statusDisplay = 'Assigned';
          break;
        case 'in_progress':
        case 'in-progress':
          statusDisplay = 'In Progress';
          break;
        case 'completed':
          statusDisplay = 'Completed';
          break;
        case 'cancelled':
          statusDisplay = 'Cancelled';
          break;
        case 'failed':
          statusDisplay = 'Failed';
          break;
        case 'started':
          statusDisplay = 'Started';
          break;
        default:
          statusDisplay = 'Pending';
      }
    }

    // Determine if completed based on status
    final completed = statusDisplay == 'Completed' ||
        statusDisplay == 'Cancelled' ||
        statusDisplay == 'Failed';

    // Format the fee
    final formattedFee = '${delivery.deliveryFee.toStringAsFixed(0)} VND';

    // Create the estimated info
    final estimatedInfo =
        estimatedDistance.isNotEmpty && estimatedDuration.isNotEmpty
            ? '$estimatedDistance, $estimatedDuration'
            : 'No estimate';

    return DriverDelivery(
      id: delivery.id,
      orderId: delivery.orderId,
      orderNumber: orderNumber,
      orderDescription: orderDescription,
      statusId: delivery.statusId ?? 0, // Default
      deliveryStatus: delivery.deliveryStatus,
      deliveryFee: delivery.deliveryFee,
      transportMode: delivery.transportMode,
      serviceType: delivery.serviceType,
      pickupDate: delivery.pickupDate,
      scheduleDeliveryTime: delivery.scheduleDeliveryTime,
      actualDeliveryTime: delivery.actualDeliveryTime,
      lateDeliveryRisk: delivery.lateDeliveryRisk?.toString(),
      deliveryAttempts: delivery.deliveryAttempts,
      deliveryNotes: delivery.deliveryNotes,
      orderDate: delivery.orderDate,
      vehicleId: delivery.vehicleId,
      vehicleLicensePlate: vehicleLicensePlate,
      vehicleType: vehicleType,
      driverId: delivery.driverId,
      driverName: driverName,
      driverPhone: driverPhone,
      deliveryAddress: deliveryAddress,
      trackingId: '', // Not provided in the Delivery model
      currentLocation: '', // Not provided in the Delivery model
      routeId: delivery.routeId,
      routeName: routeName,
      estimatedDistance: estimatedDistance,
      estimatedDuration: estimatedDuration,
      createdAt: '', // Not provided in the Delivery model
      updatedAt: '', // Not provided in the Delivery model
      completed: completed,
      pending: statusDisplay == 'Pending',
      onTime: delivery.lateDeliveryRisk == 0,
      highRisk:
          delivery.lateDeliveryRisk != null && delivery.lateDeliveryRisk! > 50,
      serviceTypeDisplay: delivery.serviceTypeDisplay,
      transportModeDisplay: delivery.transportModeDisplay,
      delayed: delivery.delayed,
      inTransit: statusDisplay == 'In Progress' || statusDisplay == 'Started',
      statusDisplay: statusDisplay,
      vehicleDisplay: vehicleDisplay,
      driverDisplay: driverName,
      formattedFee: formattedFee,
      estimatedInfo: estimatedInfo,
    );
  }

  // Convert to JSON
  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'orderId': orderId,
      'orderNumber': orderNumber,
      'orderDescription': orderDescription,
      'statusId': statusId,
      'deliveryStatus': deliveryStatus,
      'deliveryFee': deliveryFee,
      'transportMode': transportMode,
      'serviceType': serviceType,
      'pickupDate': pickupDate,
      'scheduleDeliveryTime': scheduleDeliveryTime,
      'actualDeliveryTime': actualDeliveryTime,
      'lateDeliveryRisk': lateDeliveryRisk,
      'deliveryAttempts': deliveryAttempts,
      'deliveryNotes': deliveryNotes,
      'orderDate': orderDate,
      'vehicleId': vehicleId,
      'vehicleLicensePlate': vehicleLicensePlate,
      'vehicleType': vehicleType,
      'driverId': driverId,
      'driverName': driverName,
      'driverPhone': driverPhone,
      'deliveryAddress': deliveryAddress,
      'trackingId': trackingId,
      'currentLocation': currentLocation,
      'routeId': routeId,
      'routeName': routeName,
      'estimatedDistance': estimatedDistance,
      'estimatedDuration': estimatedDuration,
      'createdAt': createdAt,
      'updatedAt': updatedAt,
      'completed': completed,
      'pending': pending,
      'onTime': onTime,
      'highRisk': highRisk,
      'serviceTypeDisplay': serviceTypeDisplay,
      'transportModeDisplay': transportModeDisplay,
      'delayed': delayed,
      'inTransit': inTransit,
      'statusDisplay': statusDisplay,
      'vehicleDisplay': vehicleDisplay,
      'driverDisplay': driverDisplay,
      'formattedFee': formattedFee,
      'estimatedInfo': estimatedInfo,
    };
  }

  static List<DriverDelivery> fromJsonList(List<dynamic> jsonList) {
    return jsonList.map((item) => DriverDelivery.fromJson(item)).toList();
  }
}
