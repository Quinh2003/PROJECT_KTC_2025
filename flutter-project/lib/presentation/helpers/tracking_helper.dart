import '../../../services/delivery_services.dart';
import '../../../domain/models/delivery/delivery.dart';
import 'package:shared_preferences/shared_preferences.dart';

/// Helper class để lấy thông tin tracking từ deliveries
class TrackingHelper {
  static final DeliveryServices _deliveryServices = DeliveryServices();
  
  // SharedPreferences keys
  static const String _activeTrackingDeliveryKey = 'active_tracking_delivery_id';
  
  /// Xác định delivery đang active theo priority order:
  /// 1. Delivery đang được track (có active tracking session)
  /// 2. Delivery có status "In Transit" (statusId = 3)
  /// 3. Delivery có status "Picked Up" (statusId = 2) 
  /// 4. Delivery có schedule gần nhất với thời gian hiện tại
  /// 5. Delivery đầu tiên chưa completed
  static Future<Delivery?> _determineActiveDelivery(List<Delivery> deliveries) async {
    if (deliveries.isEmpty) return null;
    
    final prefs = await SharedPreferences.getInstance();
    
    // 1. Kiểm tra delivery đang được track
    final activeTrackingId = prefs.getInt(_activeTrackingDeliveryKey);
    if (activeTrackingId != null) {
      final tracking = deliveries.firstWhere(
        (d) => d.id == activeTrackingId && !d.completed,
        orElse: () => deliveries.first,
      );
      if (tracking.id == activeTrackingId) {
        print('📍 Using CURRENTLY TRACKING delivery: #${tracking.id}');
        return tracking;
      }
    }
    
    // 2. Tìm delivery "In Transit" (đang vận chuyển)
    final inTransitDeliveries = deliveries.where(
      (d) => d.statusId == 3 && !d.completed
    ).toList();
    
    if (inTransitDeliveries.isNotEmpty) {
      print('🚛 Found IN-TRANSIT delivery: #${inTransitDeliveries.first.id}');
      return inTransitDeliveries.first;
    }
    
    // 3. Tìm delivery "Picked Up" (đã lấy hàng)
    final pickedUpDeliveries = deliveries.where(
      (d) => d.statusId == 2 && !d.completed
    ).toList();
    
    if (pickedUpDeliveries.isNotEmpty) {
      print('📦 Found PICKED-UP delivery: #${pickedUpDeliveries.first.id}');
      return pickedUpDeliveries.first;
    }
    
    // 4. Tìm delivery có scheduled time gần nhất với hiện tại
    final now = DateTime.now();
    final scheduledDeliveries = deliveries.where(
      (d) => !d.completed && d.scheduleDeliveryTime != null
    ).toList();
    
    if (scheduledDeliveries.isNotEmpty) {
      // Sort theo scheduled time gần nhất
      scheduledDeliveries.sort((a, b) {
        try {
          final timeA = DateTime.parse(a.scheduleDeliveryTime!);
          final timeB = DateTime.parse(b.scheduleDeliveryTime!);
          
          // Tính khoảng cách thời gian với hiện tại
          final diffA = (timeA.difference(now)).abs();
          final diffB = (timeB.difference(now)).abs();
          
          return diffA.compareTo(diffB);
        } catch (e) {
          return 0;
        }
      });
      
      print('⏰ Found SCHEDULED delivery: #${scheduledDeliveries.first.id}');
      return scheduledDeliveries.first;
    }
    
    // 5. Fallback: delivery đầu tiên chưa completed
    final activeDeliveries = deliveries.where(
      (d) => !d.completed && d.statusId != 4 // 4 = completed
    ).toList();
    
    if (activeDeliveries.isNotEmpty) {
      print('📋 Found ACTIVE delivery: #${activeDeliveries.first.id}');
      return activeDeliveries.first;
    }
    
    print('⚠️ No suitable active delivery found');
    return null;
  }
  
  /// Lấy vehicleId từ delivery đang active
  /// 
  /// Returns vehicleId nếu có delivery active, null nếu không có
  static Future<int?> getActiveVehicleId() async {
    try {
      // Lấy danh sách deliveries của driver
      final deliveries = await _deliveryServices.getDriverDeliveries();
      
      if (deliveries.isNotEmpty) {
        // Xác định delivery đang active theo logic priority
        final activeDelivery = await _determineActiveDelivery(deliveries);
        
        if (activeDelivery != null) {
          print('🚛 Active delivery #${activeDelivery.id} has vehicle ID ${activeDelivery.vehicleId}');
          print('📋 Order: ${activeDelivery.orderNumber ?? 'N/A'}');
          print('🚗 Vehicle: ${activeDelivery.vehicleLicensePlate ?? 'N/A'}');
          print('📍 Status: ${activeDelivery.statusDisplay}');
          
          return activeDelivery.vehicleId;
        }
      }
      
      print('⚠️ No active deliveries found');
      return null;
      
    } catch (e) {
      print('❌ Error getting active vehicle ID: $e');
      return null;
    }
  }
  
  /// Lấy thông tin delivery đang active với đầy đủ chi tiết
  /// 
  /// Returns delivery object nếu có, null nếu không có
  static Future<Map<String, dynamic>?> getActiveDeliveryInfo() async {
    try {
      final deliveries = await _deliveryServices.getDriverDeliveries();
      
      if (deliveries.isNotEmpty) {
        final activeDelivery = await _determineActiveDelivery(deliveries);
        
        if (activeDelivery != null) {
          return {
            'deliveryId': activeDelivery.id,
            'vehicleId': activeDelivery.vehicleId,
            'orderNumber': activeDelivery.orderNumber,
            'orderDescription': activeDelivery.orderDescription,
            'vehicleLicensePlate': activeDelivery.vehicleLicensePlate,
            'vehicleType': activeDelivery.vehicleType,
            'statusId': activeDelivery.statusId,
            'statusDisplay': activeDelivery.statusDisplay,
            'deliveryAddress': activeDelivery.deliveryAddress,
            'scheduleDeliveryTime': activeDelivery.scheduleDeliveryTime,
            'estimatedDistance': activeDelivery.estimatedDistance,
            'estimatedDuration': activeDelivery.estimatedDuration,
          };
        }
      }
      
      return null;
    } catch (e) {
      print('❌ Error getting active delivery info: $e');
      return null;
    }
  }
  

  
  /// Set delivery đang được track (được gọi khi start tracking)
  static Future<void> setTrackingDelivery(int deliveryId) async {
    final prefs = await SharedPreferences.getInstance();
    await prefs.setInt(_activeTrackingDeliveryKey, deliveryId);
    print('📍 Set delivery #$deliveryId as currently tracking');
  }
  
  /// Clear tracking delivery (được gọi khi stop tracking)
  static Future<void> clearTrackingDelivery() async {
    final prefs = await SharedPreferences.getInstance();
    await prefs.remove(_activeTrackingDeliveryKey);
    print('⏹️ Cleared tracking delivery');
  }

}