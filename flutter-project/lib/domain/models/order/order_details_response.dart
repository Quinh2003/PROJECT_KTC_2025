
import 'address.dart';
import 'store.dart';
import 'package:ktc_logistics_driver/domain/models/delivery/delivery_response.dart';
import 'package:ktc_logistics_driver/domain/models/delivery/driver_status_model.dart';
import 'package:ktc_logistics_driver/domain/models/delivery_proof/delivery_proof.dart';
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
  final List<DeliveryProof>? deliveryProofs; // Added delivery proofs

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
    this.deliveryProofs, // Added delivery proofs parameter
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
    
    // Xử lý JSON từ API mới - kiểm tra cả root level và orderDetail
    // API có thể trả về status ở root level hoặc trong orderDetail object
    String? statusValue;
    int? idValue;
    String? descriptionValue;
    String? notesValue;
    Address? addressValue;
    Store? storeValue;
    Map<String, dynamic>? deliveryValue;
    List<dynamic>? orderItemsValue;
    
    // Ưu tiên lấy từ orderDetail nếu có
    if (json["orderDetail"] != null) {
      final orderDetail = json["orderDetail"];
      statusValue = orderDetail["status"];
      idValue = orderDetail["id"];
      descriptionValue = orderDetail["description"];
      notesValue = orderDetail["notes"];
      addressValue = orderDetail["address"] != null ? Address.fromJson(orderDetail["address"]) : null;
      storeValue = orderDetail["store"] != null ? Store.fromJson(orderDetail["store"]) : null;
      deliveryValue = orderDetail["delivery"];
      orderItemsValue = orderDetail["orderItems"];
    } else {
      // Fallback lấy từ root level
      statusValue = json["status"];
      idValue = json["id"];
      descriptionValue = json["description"];
      notesValue = json["notes"];
      addressValue = json["address"] != null ? Address.fromJson(json["address"]) : null;
      storeValue = json["store"] != null ? Store.fromJson(json["store"]) : null;
      deliveryValue = json["delivery"];
      orderItemsValue = json["orderItems"];
    }
    
    return OrderDetailsResponse(
      resp: true,
      msg: "Success",
      detailsOrder: [], // API mới không sử dụng detailsOrder
      id: idValue,
      status: statusValue,
      description: descriptionValue,
      notes: notesValue,
      address: addressValue,
      store: storeValue,
      deliveryResponse: deliveryValue != null ? DeliveryResponse.fromJson({"success": true, "message": "OK", "data": deliveryValue}) : null,
      driver: json["driver"] != null ? DriverStatus.fromJson(json["driver"]) : null,
      orderItems: orderItemsValue != null 
        ? List<OrderItem>.from(orderItemsValue.map((x) => OrderItem.fromJson(x))) 
        : [],
      deliveryProofs: json["deliveryProofs"] != null 
        ? List<DeliveryProof>.from(json["deliveryProofs"].map((x) => DeliveryProof.fromJson(x))) 
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


