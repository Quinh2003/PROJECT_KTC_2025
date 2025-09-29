class MaintenanceRequest {
  final int id;
  final String description;
  final String maintenanceType;
  final double? cost;
  final DateTime? maintenanceDate;
  final DateTime? nextDueDate;
  final String? notes;
  final DateTime createdAt;
  final DateTime? updatedAt;

  // Vehicle information
  final VehicleBasicInfo? vehicle;
  
  // Creator information
  final UserBasicInfo? createdBy;
  
  // Status information
  final StatusInfo? status;

  MaintenanceRequest({
    required this.id,
    required this.description,
    required this.maintenanceType,
    this.cost,
    this.maintenanceDate,
    this.nextDueDate,
    this.notes,
    required this.createdAt,
    this.updatedAt,
    this.vehicle,
    this.createdBy,
    this.status,
  });

  factory MaintenanceRequest.fromJson(Map<String, dynamic> json) {
    return MaintenanceRequest(
      id: json['id'],
      description: json['description'],
      maintenanceType: json['maintenanceType'],
      cost: json['cost']?.toDouble(),
      maintenanceDate: json['maintenanceDate'] != null 
          ? DateTime.parse(json['maintenanceDate']) 
          : null,
      nextDueDate: json['nextDueDate'] != null 
          ? DateTime.parse(json['nextDueDate']) 
          : null,
      notes: json['notes'],
      createdAt: DateTime.parse(json['createdAt']),
      updatedAt: json['updatedAt'] != null 
          ? DateTime.parse(json['updatedAt']) 
          : null,
      vehicle: json['vehicle'] != null 
          ? VehicleBasicInfo.fromJson(json['vehicle']) 
          : null,
      createdBy: json['createdBy'] != null 
          ? UserBasicInfo.fromJson(json['createdBy']) 
          : null,
      status: json['status'] != null 
          ? StatusInfo.fromJson(json['status']) 
          : null,
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'description': description,
      'maintenanceType': maintenanceType,
      'cost': cost,
      'maintenanceDate': maintenanceDate?.toIso8601String(),
      'nextDueDate': nextDueDate?.toIso8601String(),
      'notes': notes,
      'createdAt': createdAt.toIso8601String(),
      'updatedAt': updatedAt?.toIso8601String(),
      'vehicle': vehicle?.toJson(),
      'createdBy': createdBy?.toJson(),
      'status': status?.toJson(),
    };
  }

  // Helper getters for UI display
  String get formattedCost {
    if (cost == null) return 'Chưa xác định';
    return '${cost!.toStringAsFixed(0)} VND';
  }

  String get formattedMaintenanceDate {
    if (maintenanceDate == null) return 'Chưa lên lịch';
    return '${maintenanceDate!.day}/${maintenanceDate!.month}/${maintenanceDate!.year}';
  }

  String get formattedNextDueDate {
    if (nextDueDate == null) return 'Chưa xác định';
    return '${nextDueDate!.day}/${nextDueDate!.month}/${nextDueDate!.year}';
  }

  String get statusName => status?.name ?? 'Chưa xác định';

  String get vehicleInfo => vehicle?.licensePlate ?? 'N/A';

  String get createdByName => createdBy?.fullName ?? 'N/A';

  // Copy with method for updates
  MaintenanceRequest copyWith({
    int? id,
    String? description,
    String? maintenanceType,
    double? cost,
    DateTime? maintenanceDate,
    DateTime? nextDueDate,
    String? notes,
    DateTime? createdAt,
    DateTime? updatedAt,
    VehicleBasicInfo? vehicle,
    UserBasicInfo? createdBy,
    StatusInfo? status,
  }) {
    return MaintenanceRequest(
      id: id ?? this.id,
      description: description ?? this.description,
      maintenanceType: maintenanceType ?? this.maintenanceType,
      cost: cost ?? this.cost,
      maintenanceDate: maintenanceDate ?? this.maintenanceDate,
      nextDueDate: nextDueDate ?? this.nextDueDate,
      notes: notes ?? this.notes,
      createdAt: createdAt ?? this.createdAt,
      updatedAt: updatedAt ?? this.updatedAt,
      vehicle: vehicle ?? this.vehicle,
      createdBy: createdBy ?? this.createdBy,
      status: status ?? this.status,
    );
  }
}

class VehicleBasicInfo {
  final int id;
  final String licensePlate;
  final String vehicleType;

  VehicleBasicInfo({
    required this.id,
    required this.licensePlate,
    required this.vehicleType,
  });

  factory VehicleBasicInfo.fromJson(Map<String, dynamic> json) {
    return VehicleBasicInfo(
      id: json['id'],
      licensePlate: json['licensePlate'],
      vehicleType: json['vehicleType'],
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'licensePlate': licensePlate,
      'vehicleType': vehicleType,
    };
  }
}

class UserBasicInfo {
  final int id;
  final String fullName;
  final String email;

  UserBasicInfo({
    required this.id,
    required this.fullName,
    required this.email,
  });

  factory UserBasicInfo.fromJson(Map<String, dynamic> json) {
    return UserBasicInfo(
      id: json['id'],
      fullName: json['fullName'],
      email: json['email'],
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'fullName': fullName,
      'email': email,
    };
  }
}

class StatusInfo {
  final int id;
  final String name;

  StatusInfo({
    required this.id,
    required this.name,
  });

  factory StatusInfo.fromJson(Map<String, dynamic> json) {
    return StatusInfo(
      id: json['id'],
      name: json['name'],
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'name': name,
    };
  }
}

// Simplified enums based on database values
enum MaintenanceType {
  routine('Bảo dưỡng định kỳ'),
  repair('Sửa chữa'),
  inspection('Kiểm tra'),
  emergency('Khẩn cấp');

  const MaintenanceType(this.displayName);
  final String displayName;
}