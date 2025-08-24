// notification_model.dart
// Model cho thông báo từ server

class Notification {
  final String id;            // ID thông báo
  final String title;         // Tiêu đề
  final String body;          // Nội dung
  final String type;          // Loại thông báo (ORDER_ASSIGNED, ORDER_CANCELLED...)
  final Map<String, dynamic> data; // Dữ liệu đi kèm
  final DateTime createdAt;   // Thời gian tạo
  final bool isRead;          // Đã đọc chưa

  // Constructor với các tham số bắt buộc
  Notification({
    required this.id,
    required this.title,
    required this.body,
    required this.type,
    required this.data,
    required this.createdAt,
    required this.isRead,
  });

  // Factory constructor để parse JSON thành object
  factory Notification.fromJson(Map<String, dynamic> json) {
    return Notification(
      id: json['id'] ?? '',
      title: json['title'] ?? '',
      body: json['body'] ?? '',
      type: json['type'] ?? '',
      data: json['data'] ?? {},
      createdAt: json['createdAt'] != null 
          ? DateTime.parse(json['createdAt']) 
          : DateTime.now(),
      isRead: json['isRead'] ?? false,
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'title': title,
      'body': body,
      'type': type,
      'data': data,
      'createdAt': createdAt.toIso8601String(),
      'isRead': isRead,
    };
  }

  // Tạo một bản sao của đối tượng với thuộc tính isRead được cập nhật
  Notification markAsRead() {
    return Notification(
      id: this.id,
      title: this.title,
      body: this.body,
      type: this.type,
      data: this.data,
      createdAt: this.createdAt,
      isRead: true,
    );
  }
}

// Model response cho API danh sách thông báo
class NotificationsResponse {
  final bool success;              // Trạng thái thành công
  final String message;            // Thông báo
  final List<Notification> notifications; // Danh sách thông báo
  final int unreadCount;          // Số thông báo chưa đọc

  // Constructor với các tham số bắt buộc
  NotificationsResponse({
    required this.success,
    required this.message,
    required this.notifications,
    required this.unreadCount,
  });

  // Factory constructor để parse JSON thành object
  factory NotificationsResponse.fromJson(Map<String, dynamic> json) {
    // Parse danh sách thông báo
    List<Notification> notificationsList = [];
    if (json['notifications'] != null) {
      notificationsList = List<Notification>.from(
        json['notifications'].map((notification) => Notification.fromJson(notification))
      );
    }

    return NotificationsResponse(
      success: json['success'] ?? false,
      message: json['message'] ?? '',
      notifications: notificationsList,
      unreadCount: json['unreadCount'] ?? 0,
    );
  }

  // Convert object thành JSON
  Map<String, dynamic> toJson() {
    return {
      'success': success,
      'message': message,
      'notifications': notifications.map((notification) => notification.toJson()).toList(),
      'unreadCount': unreadCount,
    };
  }
}


