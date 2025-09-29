// order_detail.dart
// Models for order details API response

// Order detail model
class OrderDetail {
  final int id;
  final String status;
  final String description;
  final String? notes;
  final Address address;
  final Store store;
  final DeliverySummary delivery;
  final List<OrderItemDetail> orderItems;

  OrderDetail({
    required this.id,
    required this.status,
    required this.description,
    this.notes,
    required this.address,
    required this.store,
    required this.delivery,
    required this.orderItems,
  });

  factory OrderDetail.fromJson(Map<String, dynamic> json) {
    return OrderDetail(
      id: json['id'] ?? 0,
      status: json['status'] ?? 'Unknown',
      description: json['description'] ?? '',
      notes: json['notes'],
      address: json['address'] != null ? Address.fromJson(json['address']) : Address.empty(),
      store: json['store'] != null ? Store.fromJson(json['store']) : Store.empty(),
      delivery: json['delivery'] != null ? DeliverySummary.fromJson(json['delivery']) : DeliverySummary.empty(),
      orderItems: json['orderItems'] != null
          ? List<OrderItemDetail>.from(json['orderItems'].map((x) => OrderItemDetail.fromJson(x)))
          : [],
    );
  }
}

// Address model
class Address {
  final int id;
  final String? orderNumber;
  final String? addressType;
  final String address;
  final double latitude;
  final double longitude;
  final String? city;
  final String? state;
  final String? country;
  final String? region;
  final String? postalCode;
  final String contactName;
  final String contactPhone;
  final String? contactEmail;
  final String? floorNumber;
  final String? createdAt;
  final String? updatedAt;
  final String coordinatesString;
  final String? addressTypeDisplay;
  final String fullAddress;

  Address({
    required this.id,
    this.orderNumber,
    this.addressType,
    required this.address,
    required this.latitude,
    required this.longitude,
    this.city,
    this.state,
    this.country,
    this.region,
    this.postalCode,
    required this.contactName,
    required this.contactPhone,
    this.contactEmail,
    this.floorNumber,
    this.createdAt,
    this.updatedAt,
    required this.coordinatesString,
    this.addressTypeDisplay,
    required this.fullAddress,
  });

  factory Address.fromJson(Map<String, dynamic> json) {
    return Address(
      id: json['id'] ?? 0,
      orderNumber: json['orderNumber'],
      addressType: json['addressType'],
      address: json['address'] ?? '',
      latitude: json['latitude'] != null ? double.parse(json['latitude'].toString()) : 0.0,
      longitude: json['longitude'] != null ? double.parse(json['longitude'].toString()) : 0.0,
      city: json['city'],
      state: json['state'],
      country: json['country'],
      region: json['region'],
      postalCode: json['postalCode'],
      contactName: json['contactName'] ?? '',
      contactPhone: json['contactPhone'] ?? '',
      contactEmail: json['contactEmail'],
      floorNumber: json['floorNumber'],
      createdAt: json['createdAt'],
      updatedAt: json['updatedAt'],
      coordinatesString: json['coordinatesString'] ?? '',
      addressTypeDisplay: json['addressTypeDisplay'],
      fullAddress: json['fullAddress'] ?? '',
    );
  }

  factory Address.empty() {
    return Address(
      id: 0,
      address: '',
      latitude: 0.0,
      longitude: 0.0,
      contactName: '',
      contactPhone: '',
      coordinatesString: '',
      fullAddress: '',
    );
  }
}

// Store model
class Store {
  final int id;
  final String storeName;
  final String? email;
  final String phone;
  final String address;
  final double latitude;
  final double longitude;
  final bool? isActive;
  final String? notes;
  final int? createdByUserId;
  final String? createdByUserName;
  final String? createdAt;
  final String? updatedAt;
  final int? totalOrders;
  final int? activeOrders;
  final int? totalProducts;
  final String displayName;
  final String coordinatesString;
  final bool activeStore;
  final String orderSummary;
  final String fullContactInfo;
  final String statusDisplay;

  Store({
    required this.id,
    required this.storeName,
    this.email,
    required this.phone,
    required this.address,
    required this.latitude,
    required this.longitude,
    this.isActive,
    this.notes,
    this.createdByUserId,
    this.createdByUserName,
    this.createdAt,
    this.updatedAt,
    this.totalOrders,
    this.activeOrders,
    this.totalProducts,
    required this.displayName,
    required this.coordinatesString,
    required this.activeStore,
    required this.orderSummary,
    required this.fullContactInfo,
    required this.statusDisplay,
  });

  factory Store.fromJson(Map<String, dynamic> json) {
    return Store(
      id: json['id'] ?? 0,
      storeName: json['storeName'] ?? '',
      email: json['email'],
      phone: json['phone'] ?? '',
      address: json['address'] ?? '',
      latitude: json['latitude'] != null ? double.parse(json['latitude'].toString()) : 0.0,
      longitude: json['longitude'] != null ? double.parse(json['longitude'].toString()) : 0.0,
      isActive: json['isActive'],
      notes: json['notes'],
      createdByUserId: json['createdByUserId'],
      createdByUserName: json['createdByUserName'],
      createdAt: json['createdAt'],
      updatedAt: json['updatedAt'],
      totalOrders: json['totalOrders'],
      activeOrders: json['activeOrders'],
      totalProducts: json['totalProducts'],
      displayName: json['displayName'] ?? '',
      coordinatesString: json['coordinatesString'] ?? '',
      activeStore: json['activeStore'] ?? false,
      orderSummary: json['orderSummary'] ?? '',
      fullContactInfo: json['fullContactInfo'] ?? '',
      statusDisplay: json['statusDisplay'] ?? '',
    );
  }

  factory Store.empty() {
    return Store(
      id: 0,
      storeName: '',
      phone: '',
      address: '',
      latitude: 0.0,
      longitude: 0.0,
      displayName: '',
      coordinatesString: '',
      activeStore: false,
      orderSummary: '',
      fullContactInfo: '',
      statusDisplay: '',
    );
  }
}

// Delivery Summary model (for use within Order)
class DeliverySummary {
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
  final String? deliveryAttempts;
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

  DeliverySummary({
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

  factory DeliverySummary.fromJson(Map<String, dynamic> json) {
    return DeliverySummary(
      id: json['id'] ?? 0,
      orderId: json['orderId'] ?? 0,
      orderNumber: json['orderNumber'],
      orderDescription: json['orderDescription'],
      statusId: json['statusId'],
      deliveryStatus: json['deliveryStatus'],
      deliveryFee: json['deliveryFee'] != null ? double.parse(json['deliveryFee'].toString()) : 0.0,
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
      vehicleDisplay: json['vehicleDisplay'] ?? 'No vehicle assigned',
      driverDisplay: json['driverDisplay'] ?? 'No driver assigned',
      formattedFee: json['formattedFee'] ?? "\$0.00",
      estimatedInfo: json['estimatedInfo'] ?? 'No estimate',
    );
  }

  factory DeliverySummary.empty() {
    return DeliverySummary(
      id: 0,
      orderId: 0,
      deliveryFee: 0.0,
      completed: false,
      pending: false,
      onTime: false,
      highRisk: false,
      serviceTypeDisplay: 'Standard',
      transportModeDisplay: 'Road',
      delayed: false,
      inTransit: false,
      statusDisplay: 'Unknown',
      vehicleDisplay: 'No vehicle assigned',
      driverDisplay: 'No driver assigned',
      formattedFee: "\$0.00",
      estimatedInfo: 'No estimate',
    );
  }
}

// Order Item Detail model
class OrderItemDetail {
  final double volume;
  final double weight;
  final int id;
  final int? orderId;
  final String? orderNumber;
  final int? productId;
  final String productName;
  final String? productDescription;
  final String? productImage;
  final bool? isFragile;
  final int quantity;
  final double? unitPrice;
  final double? subtotal;
  final double shippingFee;
  final double? totalAmount;
  final double? productWeight;
  final double? productVolume;
  final String? notes;
  final String? createdAt;
  final String? updatedAt;
  final int? availableStock;
  final bool? inStock;
  final String displayName;
  final String formattedShippingFee;
  final String formattedUnitPrice;
  final String fragileIndicator;
  final String formattedSubtotal;
  final bool fragileItem;
  final bool inStockItem;
  final String quantityDisplay;
  final String stockStatus;
  final bool outOfStock;
  final String volumeDisplay;
  final String formattedTotal;
  final String weightDisplay;

  OrderItemDetail({
    required this.volume,
    required this.weight,
    required this.id,
    this.orderId,
    this.orderNumber,
    this.productId,
    required this.productName,
    this.productDescription,
    this.productImage,
    this.isFragile,
    required this.quantity,
    this.unitPrice,
    this.subtotal,
    required this.shippingFee,
    this.totalAmount,
    this.productWeight,
    this.productVolume,
    this.notes,
    this.createdAt,
    this.updatedAt,
    this.availableStock,
    this.inStock,
    required this.displayName,
    required this.formattedShippingFee,
    required this.formattedUnitPrice,
    required this.fragileIndicator,
    required this.formattedSubtotal,
    required this.fragileItem,
    required this.inStockItem,
    required this.quantityDisplay,
    required this.stockStatus,
    required this.outOfStock,
    required this.volumeDisplay,
    required this.formattedTotal,
    required this.weightDisplay,
  });

  factory OrderItemDetail.fromJson(Map<String, dynamic> json) {
    return OrderItemDetail(
      volume: json['volume'] != null ? double.parse(json['volume'].toString()) : 0.0,
      weight: json['weight'] != null ? double.parse(json['weight'].toString()) : 0.0,
      id: json['id'] ?? 0,
      orderId: json['orderId'],
      orderNumber: json['orderNumber'],
      productId: json['productId'],
      productName: json['productName'] ?? '',
      productDescription: json['productDescription'],
      productImage: json['productImage'],
      isFragile: json['isFragile'],
      quantity: json['quantity'] ?? 1,
      unitPrice: json['unitPrice'],
      subtotal: json['subtotal'],
      shippingFee: json['shippingFee'] != null ? double.parse(json['shippingFee'].toString()) : 0.0,
      totalAmount: json['totalAmount'],
      productWeight: json['productWeight'],
      productVolume: json['productVolume'],
      notes: json['notes'],
      createdAt: json['createdAt'],
      updatedAt: json['updatedAt'],
      availableStock: json['availableStock'],
      inStock: json['inStock'],
      displayName: json['displayName'] ?? '',
      formattedShippingFee: json['formattedShippingFee'] ?? '',
      formattedUnitPrice: json['formattedUnitPrice'] ?? '',
      fragileIndicator: json['fragileIndicator'] ?? '',
      formattedSubtotal: json['formattedSubtotal'] ?? '',
      fragileItem: json['fragileItem'] ?? false,
      inStockItem: json['inStockItem'] ?? false,
      quantityDisplay: json['quantityDisplay'] ?? '',
      stockStatus: json['stockStatus'] ?? '',
      outOfStock: json['outOfStock'] ?? false,
      volumeDisplay: json['volumeDisplay'] ?? '',
      formattedTotal: json['formattedTotal'] ?? '',
      weightDisplay: json['weightDisplay'] ?? '',
    );
  }
}
