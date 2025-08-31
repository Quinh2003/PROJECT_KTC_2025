// delivery.dart
// Model cho thông tin giao hàng

class Delivery {
  final int personId;             // ID tài xế
  final String nameDelivery;      // Tên tài xế
  final String phone;             // Số điện thoại
  final String image;             // URL ảnh đại diện
  final String notificationToken; // Token cho notification

  // Constructor với các tham số bắt buộc
  Delivery({
    required this.personId,
    required this.nameDelivery,
    required this.phone,
    required this.image,
    required this.notificationToken,
  });

  // Factory constructor để parse JSON thành object
  factory Delivery.fromJson(Map<String, dynamic> json) {
    return Delivery(
      personId: json['personId'] ?? 0,
      nameDelivery: json['nameDelivery'] ?? '',
      phone: json['phone'] ?? '',
      image: json['image'] ?? '',
      notificationToken: json['notificationToken'] ?? '',
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'personId': personId,
      'nameDelivery': nameDelivery,
      'phone': phone,
      'image': image,
      'notificationToken': notificationToken,
    };
  }
}
