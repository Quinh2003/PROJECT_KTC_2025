
import 'address.dart';
import 'store.dart';
import 'package:ktc_logistics_driver/domain/models/delivery/delivery_response.dart';
import 'package:ktc_logistics_driver/domain/models/delivery/driver_status_model.dart';
import 'order_item.dart';

class OrderDetailsResponse {
  final bool resp;
  final String msg;
  final List<DetailsOrder> detailsOrder;
  
  // Các trường bổ sung từ API response
  final int? id;
  final String? status;
  final String? description;
  final String? notes;
  final Address? address;
  final Store? store;
  final DeliveryResponse? deliveryResponse;
  final DriverStatus? driver;
  final List<OrderItem>? orderItems;

  OrderDetailsResponse({
    required this.resp,
    required this.msg,
    required this.detailsOrder,
    this.id,
    this.status,
    this.description,
    this.notes,
    this.address,
    this.store,
    this.deliveryResponse,
    this.driver,
    this.orderItems,
  });

  factory OrderDetailsResponse.fromJson(Map<String, dynamic> json) {
    // Kiểm tra nếu là JSON từ API cũ
    if (json["detailsOrder"] != null) {
      return OrderDetailsResponse(
        resp: json["resp"] ?? true,
        msg: json["msg"] ?? "",
        detailsOrder: json["detailsOrder"] != null 
          ? List<DetailsOrder>.from(json["detailsOrder"].map((x) => DetailsOrder.fromJson(x))) 
          : [],
      );
    }
    
    // Xử lý JSON từ API mới
    return OrderDetailsResponse(
      resp: true,
      msg: "Success",
      detailsOrder: [], // API mới không sử dụng detailsOrder
      id: json["id"],
      status: json["status"],
      description: json["description"],
      notes: json["notes"],
      address: json["address"] != null ? Address.fromJson(json["address"]) : null,
      store: json["store"] != null ? Store.fromJson(json["store"]) : null,
      deliveryResponse: json["delivery"] != null ? DeliveryResponse.fromJson({"success": true, "message": "OK", "data": json["delivery"]}) : null,
      driver: json["driver"] != null ? DriverStatus.fromJson(json["driver"]) : null,
      orderItems: json["orderItems"] != null 
        ? List<OrderItem>.from(json["orderItems"].map((x) => OrderItem.fromJson(x))) 
        : [],
    );
  }
}

class DetailsOrder {

  final int id;
  final int orderId;
  final int productId;
  final String nameProduct;
  final String picture;
  final int quantity;
  final double total;

  DetailsOrder({
    required this.id,
    required this.orderId,
    required this.productId,
    required this.nameProduct,
    required this.picture,
    required this.quantity,
    required this.total,
  });
    

  factory DetailsOrder.fromJson(Map<String, dynamic> json) => DetailsOrder(
    id: json["id"],
    orderId: json["order_id"],
    productId: json["product_id"],
    nameProduct: json["nameProduct"],
    picture: json["picture"],
    quantity: json["quantity"],
    total: json["total"].toDouble(),
  );

}


