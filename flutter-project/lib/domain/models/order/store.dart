// store.dart
// Model cho thông tin cửa hàng

class Store {
  final int id;
  final String storeName;
  final String? email;
  final String? phone;
  final String address;
  final double? latitude;
  final double? longitude;
  final bool? isActive;
  final String? notes;
  
  Store({
    required this.id,
    required this.storeName,
    this.email,
    this.phone,
    required this.address,
    this.latitude,
    this.longitude,
    this.isActive,
    this.notes,
  });
  
  factory Store.fromJson(Map<String, dynamic> json) => Store(
    id: json["id"] ?? 0,
    storeName: json["storeName"] ?? json["displayName"] ?? "Unknown Store",
    email: json["email"],
    phone: json["phone"],
    address: json["address"] ?? "No address",
    latitude: json["latitude"] != null 
        ? double.tryParse(json["latitude"].toString()) 
        : null,
    longitude: json["longitude"] != null 
        ? double.tryParse(json["longitude"].toString()) 
        : null,
    isActive: json["isActive"] ?? json["activeStore"],
    notes: json["notes"],
  );
  
  Map<String, dynamic> toJson() => {
    "id": id,
    "storeName": storeName,
    "email": email,
    "phone": phone,
    "address": address,
    "latitude": latitude,
    "longitude": longitude,
    "isActive": isActive,
    "notes": notes,
  };
}
