// driver_profile.dart
// Model cho thông tin tài xế

class DriverProfile {
  final int id;
  final String firstName;
  final String lastName;
  final String email;
  final String phone;
  final String? licenseNumber;
  final String? licenseExpiry;
  final String? licenseType;
  final String? address;
  final String? profilePicture;
  final String status;
  final double? rating;
  final String? notificationToken;
  final int? companyId;
  final int? userId;
  final int? vehicleId;
  final Map<String, dynamic>? user;
  final Map<String, dynamic>? vehicle;
  final Map<String, dynamic>? company;

  DriverProfile({
    required this.id,
    required this.firstName,
    required this.lastName,
    required this.email,
    required this.phone,
    this.licenseNumber,
    this.licenseExpiry,
    this.licenseType,
    this.address,
    this.profilePicture,
    required this.status,
    this.rating,
    this.notificationToken,
    this.companyId,
    this.userId,
    this.vehicleId,
    this.user,
    this.vehicle,
    this.company,
  });

  String get fullName => '$firstName $lastName';

  factory DriverProfile.fromJson(Map<String, dynamic> json) {
    return DriverProfile(
      id: json['id'],
      firstName: json['firstName'] ?? '',
      lastName: json['lastName'] ?? '',
      email: json['email'] ?? '',
      phone: json['phone'] ?? '',
      licenseNumber: json['licenseNumber'],
      licenseExpiry: json['licenseExpiry'],
      licenseType: json['licenseType'],
      address: json['address'],
      profilePicture: json['profilePicture'],
      status: json['status'] != null ? json['status']['name'] : 'INACTIVE',
      rating: json['rating']?.toDouble(),
      notificationToken: json['notificationToken'],
      companyId: json['companyId'],
      userId: json['userId'],
      vehicleId: json['vehicleId'],
      user: json['user'],
      vehicle: json['vehicle'],
      company: json['company'],
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'firstName': firstName,
      'lastName': lastName,
      'email': email,
      'phone': phone,
      'licenseNumber': licenseNumber,
      'licenseExpiry': licenseExpiry,
      'licenseType': licenseType,
      'address': address,
      'profilePicture': profilePicture,
      'status': status,
      'rating': rating,
      'notificationToken': notificationToken,
      'companyId': companyId,
      'userId': userId,
      'vehicleId': vehicleId,
      'user': user,
      'vehicle': vehicle,
      'company': company,
    };
  }
}
