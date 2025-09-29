
class OrdersByStatusResponse {

  final bool resp;
  final String msg;
  final List<OrdersResponse> ordersResponse;

  OrdersByStatusResponse({
    required this.resp,
    required this.msg,
    required this.ordersResponse,
  });

  factory OrdersByStatusResponse.fromJson(Map<String, dynamic> json) => OrdersByStatusResponse(
    resp: json["resp"],
    msg: json["msg"],
    ordersResponse: json["ordersResponse"] != null ? List<OrdersResponse>.from(json["ordersResponse"].map((x) => OrdersResponse.fromJson(x))) : [],
  );
}

class OrdersResponse {

  final int orderId;
  final int deliveryId;
  final String delivery;
  final String deliveryImage;
  final int clientId;
  final String cliente;
  final String clientImage;
  final String clientPhone;
  final int addressId;
  final String street;
  final String reference;
  final String latitude;
  final String longitude;
  final String status;
  final String payType;
  final double amount;
  final DateTime currentDate;
  // Các trường bổ sung từ API response
  final String? orderCode;
  final String? pickupAddress;
  final DateTime? scheduledTime;
  final double? deliveryFee;
  final String? paymentMethod;
  final String? priority;

  OrdersResponse({
    required this.orderId,
    required this.deliveryId,
    required this.delivery,
    required this.deliveryImage,
    required this.clientId,
    required this.cliente,
    required this.clientImage,
    required this.clientPhone,
    required this.addressId,
    required this.street,
    required this.reference,
    required this.latitude,
    required this.longitude,
    required this.status,
    required this.payType,
    required this.amount,
    required this.currentDate,
    this.orderCode,
    this.pickupAddress,
    this.scheduledTime,
    this.deliveryFee,
    this.paymentMethod,
    this.priority,
  });

  factory OrdersResponse.fromJson(Map<String, dynamic> json) => OrdersResponse(
    orderId: json["order_id"] ?? json["id"] ?? 0,
    deliveryId: json["delivery_id"] ?? 0,
    delivery: json["delivery"] ?? '',
    deliveryImage: json["deliveryImage"] ?? '',
    clientId: json["client_id"] ?? 0,
    cliente: json["cliente"] ?? json["customerName"] ?? 'Unknown',
    clientImage: json["clientImage"] ?? '',
    clientPhone: json["clientPhone"] ?? json["customerPhone"] ?? '',
    addressId: json["address_id"] ?? 0,
    street: json["street"] ?? json["deliveryAddress"] ?? '',
    reference: json["reference"] ?? '',
    latitude: json["Latitude"] ?? '0',
    longitude: json["Longitude"] ?? '0',
    status: json["status"] ?? 'Pending',
    payType: json["pay_type"] ?? json["paymentMethod"] ?? 'Unknown',
    amount: json["amount"] != null ? json["amount"].toDouble() : 0.0,
    currentDate: json["currentDate"] != null 
      ? DateTime.parse(json["currentDate"]) 
      : DateTime.now(),
    // Các trường bổ sung
    orderCode: json["orderCode"],
    pickupAddress: json["pickupAddress"],
    scheduledTime: json["scheduledTime"] != null 
      ? DateTime.parse(json["scheduledTime"]) 
      : null,
    deliveryFee: json["deliveryFee"] != null 
      ? double.tryParse(json["deliveryFee"].toString()) 
      : null,
    paymentMethod: json["paymentMethod"],
    priority: json["priority"],
  );
}


