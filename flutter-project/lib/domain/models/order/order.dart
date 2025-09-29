// order.dart
// Model cho thông tin đơn hàng

class Order {
  final int id;
  final String orderDate;
  final String? deliveryDate;
  final int customerId;
  final double totalAmount;
  final double? totalWeight;
  final double? totalVolume;
  final String? customerAddress;
  final double? customerLatitude;
  final double? customerLongitude;
  final String? notes;
  final String orderStatus;
  final Map<String, dynamic>? customer;
  final List<dynamic>? orderItems;

  Order({
    required this.id,
    required this.orderDate,
    this.deliveryDate,
    required this.customerId,
    required this.totalAmount,
    this.totalWeight,
    this.totalVolume,
    this.customerAddress,
    this.customerLatitude,
    this.customerLongitude,
    this.notes,
    required this.orderStatus,
    this.customer,
    this.orderItems,
  });

  factory Order.fromJson(Map<String, dynamic> json) {
    return Order(
      id: json['id'],
      orderDate: json['orderDate'],
      deliveryDate: json['deliveryDate'],
      customerId: json['customerId'],
      totalAmount: json['totalAmount'].toDouble(),
      totalWeight: json['totalWeight']?.toDouble(),
      totalVolume: json['totalVolume']?.toDouble(),
      customerAddress: json['customerAddress'],
      customerLatitude: json['customerLatitude']?.toDouble(),
      customerLongitude: json['customerLongitude']?.toDouble(),
      notes: json['notes'],
      orderStatus: json['orderStatus'] != null ? json['orderStatus']['name'] : 'PENDING',
      customer: json['customer'],
      orderItems: json['orderItems'],
    );
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'orderDate': orderDate,
      'deliveryDate': deliveryDate,
      'customerId': customerId,
      'totalAmount': totalAmount,
      'totalWeight': totalWeight,
      'totalVolume': totalVolume,
      'customerAddress': customerAddress,
      'customerLatitude': customerLatitude,
      'customerLongitude': customerLongitude,
      'notes': notes,
      'orderStatus': orderStatus,
      'customer': customer,
      'orderItems': orderItems,
    };
  }
  
  static List<Order> fromJsonList(List<dynamic> jsonList) {
    return jsonList.map((item) => Order.fromJson(item)).toList();
  }
}
