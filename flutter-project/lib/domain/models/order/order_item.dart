// order_item.dart
// Model cho thông tin chi tiết sản phẩm trong đơn hàng

class OrderItem {
  final int id;
  final String productName;
  final int quantity;
  final double? unitPrice;
  final double? subtotal;
  final double? shippingFee;
  final double? volume;
  final double? weight;
  final bool? isFragile;
  final String? notes;
  final String? productImage;

  OrderItem({
    required this.id,
    required this.productName,
    required this.quantity,
    this.unitPrice,
    this.subtotal,
    this.shippingFee,
    this.volume,
    this.weight,
    this.isFragile,
    this.notes,
    this.productImage,
  });

  factory OrderItem.fromJson(Map<String, dynamic> json) => OrderItem(
    id: json["id"] ?? 0,
    productName: json["productName"] ?? "Unknown Product",
    quantity: json["quantity"] ?? 1,
    unitPrice: json["unitPrice"] != null 
        ? double.tryParse(json["unitPrice"].toString()) 
        : null,
    subtotal: json["subtotal"] != null 
        ? double.tryParse(json["subtotal"].toString()) 
        : null,
    shippingFee: json["shippingFee"] != null 
        ? double.tryParse(json["shippingFee"].toString()) 
        : null,
    volume: json["volume"] != null 
        ? double.tryParse(json["volume"].toString()) 
        : null,
    weight: json["weight"] != null 
        ? double.tryParse(json["weight"].toString()) 
        : null,
    isFragile: json["isFragile"],
    notes: json["notes"],
    productImage: json["productImage"],
  );

  Map<String, dynamic> toJson() => {
    "id": id,
    "productName": productName,
    "quantity": quantity,
    "unitPrice": unitPrice,
    "subtotal": subtotal,
    "shippingFee": shippingFee,
    "volume": volume,
    "weight": weight,
    "isFragile": isFragile,
    "notes": notes,
    "productImage": productImage,
  };
}
