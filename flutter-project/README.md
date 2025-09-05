# 📱 KTC Logistics Driver Mobile App 2025

**FastRoute** là ứng dụng di động dành cho tài xế giao hàng, phát triển bằng Flutter với giao diện **Spatial UI** hiện đại. Được xây dựng theo nguyên tắc **Clean Architecture**, ứng dụng dễ bảo trì, mở rộng và test, với sự phân tách rõ ràng giữa UI, business logic và data layers.

## 🚀 Tính năng chính

### 📦 Quản lý đơn hàng
- Nhận đơn hàng mới theo thời gian thực
- Xem chi tiết thông tin giao hàng
- Cập nhật trạng thái đơn hàng (đang giao, hoàn thành)
- Lịch sử các chuyến giao hàng

### 🗺️ Navigation & Maps
- Tích hợp MapBox với GPS tracking và tối ưu tuyến đường
- Hiển thị vị trí pickup, delivery và hướng dẫn chi tiết (turn-by-turn)
- Google Maps Integration với đa điểm dừng
- Background Location Tracking

### 📊 Data Visualization & Analytics
- Dashboard Analytics với tổng quan hiệu suất giao hàng
- Đa dạng biểu đồ: Area, Bar, Pie, Scatter, Bubble và Radar Chart
- Chế độ sáng/tối cho tất cả biểu đồ

## Các màn hình chính

### 🏠 Dashboard Screen
- Overview hiệu suất tài xế và key metrics
- Danh sách đơn hàng sắp tới
- Biểu đồ phân tích xu hướng giao hàng

### 🗺️ Route Map Screen
- MapBox Integration với nhiều lớp dữ liệu
- Route Optimization và hướng dẫn chi tiết
- Hỗ trợ đa điểm giao hàng và tích hợp Google Maps

### 📊 Analytics Screen
- Đa dạng biểu đồ phân tích hiệu suất
- Phân tích theo thời gian, khu vực và loại dịch vụ
- Tương thích Dark/Light Mode

### 📦 Delivery Detail Screen
- Chi tiết đơn hàng và quản lý trạng thái
- Tương tác với khách hàng (gọi điện, nhắn tin)
- Proof of Delivery (chụp ảnh, chữ ký xác nhận)

### 👤 Profile & Settings
- Thông tin cá nhân tài xế và phương tiện
- Thống kê hiệu suất làm việc
- Cài đặt ứng dụng

## 🛠️ Tech Stack

### Frontend
- **Flutter**: 3.32.8 (latest stable)
- **Dart**: 3.8.1
- **State Management**: BLoC Pattern
- **UI Components**: Material Design 3
- **Charts**: fl_chart library (3.0.0+)

### Backend Integration
- **HTTP Client**: Dart HTTP package
- **Real-time**: Socket.IO client
- **Caching**: Flutter Secure Storage

### Maps & Location
- **MapBox**: MapBox Flutter SDK
- **GPS**: Geolocator package
- **Geocoding**: Geocoding package

### Android Configuration
- **SDK**: 35 (Android 14)
- **Java**: 21 (LTS)
- **Min SDK**: 24 (Android 7.0)

## 🏛️ Clean Architecture Implementation

Ứng dụng được xây dựng theo **Clean Architecture** với 3 tầng chính:

### 🎯 Domain Layer
- Models, Repository Interfaces, Use Cases và BLoC

### 📊 Data Layer
- Repository Implementations, Data Sources và DTOs

### 🎨 Presentation Layer
- Screens, Widgets, State Management và Chart Components

### ⚙️ Data Flow & Dependency Injection
- UI → BLoC → Domain → Data → External Sources
- GetIt service locator cho dependency inversion

## Cấu hình dự án

### Prerequisites
- **Flutter SDK**: 3.32.8 
- **Dart**: 3.8.1
- **Android Studio**: Bản mới nhất
- **Gradle**: 8.14.3
- **Android SDK**: API 35 (Android 14)

### Setup và chạy project

```bash
# Clone và cài đặt dependencies
cd PROJECT_KTC_2025/driver-app
flutter pub get

# Run app
flutter run

# Build release APK
flutter build apk --release
flutter install
```

### Tài khoản test

#### 🚛 **Tài xế 1 - Xe tải**
- **Email**: `driver@ktc.com`
- **Mật khẩu**: `123456`

#### 🏍️ **Tài xế 2 - Xe máy**
- **Email**: `driver2@ktc.com`
- **Mật khẩu**: `123456`

> **Lưu ý**: App đang sử dụng mock data để demo tính năng.

## 🧪 Testing & Debugging

```bash
# Run tests
flutter test
flutter test integration_test/

# Debug trên device
flutter run --debug

# Hot reload trong development
r    # Hot reload
R    # Hot restart
q    # Quit
```

## 🔧 Troubleshooting

### Common Issues

```bash
# Gradle build fails
cd android && ./gradlew clean
cd .. && flutter clean && flutter pub get

# Maps không hiển thị
flutter run -v  # để xem lỗi chi tiết
```

---

## 📄 License

Copyright © 2025 KTC Logistics. All rights reserved.

---

## Liên hệ & Hỗ trợ

Nếu gặp vấn đề khi cài đặt hoặc chạy, hãy mở issue trong repository để được hỗ trợ nhanh chóng.