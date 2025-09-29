// address.dart
// Model cho thông tin địa chỉ

class Address {
  final int id;
  final String? addressType;
  final String address;
  final double? latitude;
  final double? longitude;
  final String? city;
  final String? state;
  final String? country;
  final String? postalCode;
  final String? contactName;
  final String? contactPhone;
  final String? contactEmail;
  
  Address({
    required this.id,
    this.addressType,
    required this.address,
    this.latitude,
    this.longitude,
    this.city,
    this.state,
    this.country,
    this.postalCode,
    this.contactName,
    this.contactPhone,
    this.contactEmail,
  });
  
  factory Address.fromJson(Map<String, dynamic> json) => Address(
    id: json["id"] ?? 0,
    addressType: json["addressType"],
    address: json["address"] ?? json["fullAddress"] ?? "No address",
    latitude: json["latitude"] != null 
        ? double.tryParse(json["latitude"].toString()) 
        : null,
    longitude: json["longitude"] != null 
        ? double.tryParse(json["longitude"].toString()) 
        : null,
    city: json["city"],
    state: json["state"],
    country: json["country"],
    postalCode: json["postalCode"],
    contactName: json["contactName"],
    contactPhone: json["contactPhone"],
    contactEmail: json["contactEmail"],
  );
  
  Map<String, dynamic> toJson() => {
    "id": id,
    "addressType": addressType,
    "address": address,
    "latitude": latitude,
    "longitude": longitude,
    "city": city,
    "state": state,
    "country": country,
    "postalCode": postalCode,
    "contactName": contactName,
    "contactPhone": contactPhone,
    "contactEmail": contactEmail,
  };
}
