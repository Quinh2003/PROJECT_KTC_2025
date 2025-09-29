// User model for Auth Bloc
class UserModel {
  final String id;
  final String email;
  final String name;
  final String phone;
  final String? avatar;
  final String? vehicle;
  final double rating;
  final int totalDeliveries;

  UserModel({
    required this.id, 
    required this.email,
    required this.name,
    required this.phone,
    this.avatar,
    this.vehicle,
    this.rating = 0.0,
    this.totalDeliveries = 0,
  });
}
