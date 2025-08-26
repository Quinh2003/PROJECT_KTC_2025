# 📱 KTC Logistics Driver Mobile App 2025

**FastRoute** là ứng dụng di động dành cho tài xế giao hàng, phát triển bằng Flutter với giao diện **Spatial UI** hiện đại: bố cục đa tầng, hiệu ứng chuyển động, hiển thị trực quan bản đồ và thông tin theo ngữ cảnh. Được xây dựng theo nguyên tắc **Clean Architecture**, ứng dụng dễ bảo trì, mở rộng và test, với sự phân tách rõ ràng giữa UI, business logic và data layers. Ứng dụng tối ưu cho thao tác ngoài trời, giúp tài xế tập trung và xử lý đơn hàng nhanh chóng, an toàn.

## 🚀 Tính năng chính

### 📦 Quản lý đơn hàng

- Nhận đơn hàng mới theo thời gian thực
- Xem chi tiết thông tin giao hàng
- Cập nhật trạng thái đơn hàng (đang giao, hoàn thành)
- Lịch sử các chuyến giao hàng

### 🗺️ Navigation & Maps

- Tích hợp MapBox với GPS tracking
- Tối ưu tuyến đường thời gian thực
- Hiển thị vị trí pickup và delivery
- Tuỳ chỉnh style bản đồ và đường đi
- Định vị chính xác vị trí tài xế với CustomPosition
- Tính toán quãng đường và thời gian di chuyển
- Hiển thị hướng dẫn chỉ đường chi tiết (turn-by-turn)

### 📊 Data Visualization & Analytics

- **Dashboard Analytics**: Hiển thị tổng quan hiệu suất giao hàng
- **Biểu đồ khu vực (Area Chart)**: Theo dõi xu hướng giao hàng theo thời gian
- **Biểu đồ cột (Bar Chart)**: Phân tích số lượng giao hàng theo ngày trong tuần
- **Biểu đồ tròn (Pie Chart)**: Trực quan hóa phân phối loại hình giao hàng
- **Biểu đồ phân tán (Scatter Chart)**: Phân tích mối quan hệ giữa quãng đường và thời gian giao hàng
- **Biểu đồ bong bóng (Bubble Chart)**: So sánh hiệu suất giao hàng giữa các khu vực
- **Biểu đồ radar (Radar Chart)**: Đánh giá hiệu suất đa chiều của các loại hình giao hàng
- **Chế độ sáng/tối**: Tất cả biểu đồ đều tương thích với chế độ giao diện sáng và tối

## Các màn hình chính

### 🏠 Dashboard Screen

- **Overview**: Hiển thị tổng quan hiệu suất tài xế
- **Key Metrics**: Số đơn hoàn thành, tỷ lệ đúng giờ, quãng đường trung bình, đánh giá
- **Upcoming Deliveries**: Danh sách đơn hàng sắp tới
- **Charts**: Biểu đồ khu vực và cột cho phân tích xu hướng giao hàng

### 🗺️ Route Map Screen

- **MapBox Integration**: Bản đồ tương tác với nhiều lớp dữ liệu
- **Route Optimization**: Tính toán và hiển thị tuyến đường tối ưu
- **Turn-by-turn Navigation**: Hướng dẫn chi tiết từng đoạn đường
- **Custom Position Handling**: Xử lý và hiển thị vị trí chính xác với kiểu dữ liệu tùy chỉnh
- **Multiple Stops**: Hỗ trợ đa điểm giao hàng trên cùng tuyến đường

### � Analytics Screen

- **Weekly Performance**: Biểu đồ area chart cho xu hướng theo thời gian
- **Daily Analysis**: Biểu đồ cột phân tích theo ngày trong tuần
- **Delivery Distribution**: Biểu đồ tròn phân loại các chuyến giao hàng
- **Efficiency Analysis**: Biểu đồ scatter chart so sánh quãng đường và thời gian
- **Regional Performance**: Biểu đồ bubble chart phân tích theo khu vực
- **Service Comparison**: Biểu đồ radar đánh giá các loại hình dịch vụ giao hàng
- **Dark/Light Mode**: Tự động điều chỉnh hiển thị theo chế độ của hệ thống

### � Delivery Detail Screen

- **Package Information**: Chi tiết đơn hàng
- **Status Management**: Cập nhật trạng thái giao hàng
- **Customer Interaction**: Gọi điện, nhắn tin với khách hàng
- **Proof of Delivery**: Chụp ảnh, lấy chữ ký xác nhận
- **Special Instructions**: Hiển thị yêu cầu đặc biệt của đơn hàng

### 👤 Profile & Settings

- **Driver Information**: Thông tin cá nhân tài xế
- **Vehicle Details**: Thông tin phương tiện
- **Performance Stats**: Thống kê hiệu suất làm việc
- **App Settings**: Cài đặt ngôn ngữ, thông báo, theme

## 📊 Data Visualization Components

Ứng dụng tích hợp nhiều loại biểu đồ để trực quan hóa dữ liệu giao hàng, giúp tài xế theo dõi hiệu suất và tối ưu hóa công việc:

### 🌊 Area Chart

- **DeliveryAreaChart**: Hiển thị xu hướng giao hàng theo thời gian
- **Tính năng**: Đường cong mượt mà, vùng gradient, tương thích dark/light mode
- **Thông số hiển thị**: Số lượng giao hàng theo ngày trong tuần
- **Use case**: Phân tích xu hướng, phát hiện mẫu hình theo thời gian

### 📊 Bar Chart

- **DeliveryBarChart**: So sánh số lượng giao hàng theo ngày trong tuần
- **Tính năng**: Thanh gradient, tương tác tooltip, hiển thị giá trị
- **Thông số hiển thị**: Số lượng giao hàng mỗi ngày
- **Use case**: So sánh hiệu suất giữa các ngày, xác định ngày cao điểm/thấp điểm

### 🍩 Pie Chart

- **DeliveryTypePieChart**: Phân phối các loại giao hàng
- **Tính năng**: Sections động, tương tác tooltip, hiển thị phần trăm
- **Thông số hiển thị**: Tỷ lệ phần trăm của từng loại giao hàng
- **Use case**: Phân tích cơ cấu giao hàng, so sánh loại hình dịch vụ

### 🔵 Scatter Chart

- **DeliveryScatterChart**: Phân tích mối quan hệ giữa khoảng cách và thời gian
- **Tính năng**: Kích thước điểm động (thể hiện kích thước gói hàng), tooltip chi tiết
- **Thông số hiển thị**: Khoảng cách (km), thời gian (phút), kích thước gói hàng
- **Use case**: Phân tích hiệu quả giao hàng, xác định outliers

### 🫧 Bubble Chart (Custom Scatter)

- **DeliveryBubbleChart**: So sánh hiệu suất giao hàng giữa các khu vực
- **Tính năng**: Bong bóng kích thước động, tooltip thông tin, tương tác hover
- **Thông số hiển thị**: Khoảng cách trung bình, tỷ lệ đúng giờ, số lượng giao hàng
- **Use case**: So sánh hiệu suất giữa các khu vực địa lý

### 🔮 Radar Chart

- **DeliveryRadarChart**: Đánh giá đa chiều các loại hình dịch vụ giao hàng
- **Tính năng**: Hiển thị đa metric, tương tác chọn loại dịch vụ, legend
- **Thông số hiển thị**: Tốc độ, độ chính xác, chi phí, phạm vi, sự hài lòng của khách hàng
- **Use case**: So sánh toàn diện hiệu suất của các loại dịch vụ giao hàng

### 🎨 Thiết kế và tích hợp

- **Responsive**: Tự động điều chỉnh kích thước theo thiết bị
- **Theme Support**: Tương thích đầy đủ với chế độ sáng/tối
- **Interaction**: Tooltips, hover effects, và animations
- **Performance**: Tối ưu hóa hiệu suất với Flutter
- **Integration**: Tích hợp liền mạch với hệ thống analytics

---

## 🛠️ Tech Stack

### Frontend

- **Flutter**: 3.32.8 (latest stable)
- **Dart**: 3.8.1
- **State Management**: BLoC Pattern
- **UI Components**: Material Design 3
- **Charts**: fl_chart library (3.0.0+)
- **Spatial UI**: Custom implementation của Material 3 với hiệu ứng depth và layering

### Backend Integration

- **HTTP Client**: Dart HTTP package
- **Real-time**: Socket.IO client
- **Caching**: Flutter Secure Storage
- **Image Handling**: Image Picker
- **Notifications**: Firebase Cloud Messaging (tạm thời)

### Maps & Location

- **MapBox**: MapBox Flutter SDK
- **GPS**: Geolocator package
- **Geocoding**: Geocoding package
- **Permissions**: Permission Handler

### Android Configuration

- **SDK**: 35 (Android 14)
- **Java**: 21 (LTS)
- **Gradle**: 8.14.3
- **Min SDK**: 24 (Android 7.0)

---

## 🏛️ Clean Architecture Implementation

Ứng dụng được xây dựng theo **Clean Architecture**, giúp tạo ra một codebase dễ maintain, test và mở rộng. Kiến trúc này chia ứng dụng thành 3 tầng chính:

### 🎯 Domain Layer

Tầng core chứa business logic, hoàn toàn độc lập với framework:
- **Models**: Entities và business objects
- **Repository Interfaces**: Contracts cho data operations
- **Use Cases**: Business rules cụ thể
- **BLoC**: Business Logic Components

### 📊 Data Layer

Tầng xử lý dữ liệu từ external sources:
- **Repository Implementations**: Concrete implementations của domain interfaces
- **Data Sources**: Remote (API) và local (cache, database)
- **DTOs**: Data Transfer Objects và mappers

### 🎨 Presentation Layer

Tầng giao diện người dùng:
- **Screens & Pages**: UI của ứng dụng
- **Widgets**: Reusable UI components
- **State Management**: BLoC providers và consumers
- **Chart Components**: Các biểu đồ dữ liệu được phát triển với fl_chart
- **Spatial UI**: Design system theo nguyên tắc đa lớp, tương thích dark/light mode

### � Data Flow & Dependency Injection

**Flow**: UI → BLoC → Domain (Use Cases, Repository Interfaces) → Data (Repository Implementations) → External Sources

**Dependency Inversion**: Sử dụng GetIt service locator để đảm bảo Domain layer không phụ thuộc vào implementation details

**Benefits**:
- **Testability**: Mỗi layer có thể test independently
- **Flexibility**: Dễ thay đổi UI/data sources mà không ảnh hưởng business logic
- **Maintainability**: Code organization rõ ràng
- **Independence**: Business logic không phụ thuộc framework

---

## � Data Visualization Components

Ứng dụng tích hợp nhiều loại biểu đồ để trực quan hóa dữ liệu giao hàng, giúp tài xế theo dõi hiệu suất và tối ưu hóa công việc:

### � Area Chart

- **DeliveryAreaChart**: Hiển thị xu hướng giao hàng theo thời gian
- **Tính năng**: Đường cong mượt mà, vùng gradient, tương thích dark/light mode
- **Thông số hiển thị**: Số lượng giao hàng theo ngày trong tuần
- **Use case**: Phân tích xu hướng, phát hiện mẫu hình theo thời gian

### � Bar Chart

- **DeliveryBarChart**: So sánh số lượng giao hàng theo ngày trong tuần
- **Tính năng**: Thanh gradient, tương tác tooltip, hiển thị giá trị
- **Thông số hiển thị**: Số lượng giao hàng mỗi ngày
- **Use case**: So sánh hiệu suất giữa các ngày, xác định ngày cao điểm/thấp điểm

### 🍩 Pie Chart

- **DeliveryTypePieChart**: Phân phối các loại giao hàng
- **Tính năng**: Sections động, tương tác tooltip, hiển thị phần trăm
- **Thông số hiển thị**: Tỷ lệ phần trăm của từng loại giao hàng
- **Use case**: Phân tích cơ cấu giao hàng, so sánh loại hình dịch vụ

### � Scatter Chart

- **DeliveryScatterChart**: Phân tích mối quan hệ giữa khoảng cách và thời gian
- **Tính năng**: Kích thước điểm động (thể hiện kích thước gói hàng), tooltip chi tiết
- **Thông số hiển thị**: Khoảng cách (km), thời gian (phút), kích thước gói hàng
- **Use case**: Phân tích hiệu quả giao hàng, xác định outliers

### 🫧 Bubble Chart (Custom Scatter)

- **DeliveryBubbleChart**: So sánh hiệu suất giao hàng giữa các khu vực
- **Tính năng**: Bong bóng kích thước động, tooltip thông tin, tương tác hover
- **Thông số hiển thị**: Khoảng cách trung bình, tỷ lệ đúng giờ, số lượng giao hàng
- **Use case**: So sánh hiệu suất giữa các khu vực địa lý

### � Radar Chart

- **DeliveryRadarChart**: Đánh giá đa chiều các loại hình dịch vụ giao hàng
- **Tính năng**: Hiển thị đa metric, tương tác chọn loại dịch vụ, legend
- **Thông số hiển thị**: Tốc độ, độ chính xác, chi phí, phạm vi, sự hài lòng của khách hàng
- **Use case**: So sánh toàn diện hiệu suất của các loại dịch vụ giao hàng

### 🎨 Thiết kế và tích hợp

- **Responsive**: Tự động điều chỉnh kích thước theo thiết bị
- **Theme Support**: Tương thích đầy đủ với chế độ sáng/tối
- **Interaction**: Tooltips, hover effects, và animations
- **Performance**: Tối ưu hóa hiệu suất với Flutter
- **Integration**: Tích hợp liền mạch với hệ thống analytics

---

## 🗺️ Cải tiến Map và Custom Position

Để giải quyết vấn đề xung đột giữa các kiểu dữ liệu vị trí, ứng dụng đã triển khai `CustomPosition` class:

### CustomPosition Class

```dart
class CustomPosition {
  final double latitude;
  final double longitude;
  final double altitude;
  final double accuracy;
  final double heading;
  final double speed;
  final double speedAccuracy;
  final DateTime timestamp;

  CustomPosition({
    required this.latitude,
    required this.longitude,
    this.altitude = 0.0,
    this.accuracy = 0.0,
    this.heading = 0.0,
    this.speed = 0.0,
    this.speedAccuracy = 0.0,
    DateTime? timestamp,
  }) : timestamp = timestamp ?? DateTime.now();

  // Conversion from geolocator Position
  factory CustomPosition.fromPosition(position) {
    return CustomPosition(
      latitude: position.latitude,
      longitude: position.longitude,
      altitude: position.altitude,
      accuracy: position.accuracy,
      heading: position.heading,
      speed: position.speed,
      speedAccuracy: position.speedAccuracy,
      timestamp: position.timestamp,
    );
  }

  // Conversion to Mapbox's Position
  dynamic toMapboxPosition() {
    // Implementation depends on the exact Mapbox Position format
    return [longitude, latitude]; // Typically [lng, lat] for Mapbox
  }
}
```

### Tính năng của CustomPosition

- **Loại bỏ xung đột**: Giải quyết vấn đề xung đột giữa geolocator Position và Mapbox Position
- **Khả năng chuyển đổi**: Hỗ trợ chuyển đổi qua lại giữa các định dạng vị trí khác nhau
- **Chuẩn hóa dữ liệu**: Đảm bảo nhất quán trong cách sử dụng dữ liệu vị trí trong ứng dụng
- **Mở rộng**: Dễ dàng mở rộng với các thuộc tính bổ sung khi cần

### Tối ưu hóa Route Map

- **Xử lý marker động**: Tự động cập nhật vị trí marker theo GPS
- **Custom marker icons**: Sử dụng icon tùy chỉnh cho các loại điểm trên bản đồ
- **Polyline animation**: Hiệu ứng đường đi với gradient và animation
- **Turn-by-turn instructions**: Hướng dẫn chi tiết cho từng đoạn đường
- **Multiple route options**: Hiển thị nhiều tùy chọn tuyến đường và cho phép người dùng chọn
- **Route optimization**: Tính toán tuyến đường tối ưu với nhiều điểm dừng

### Các cải tiến khác

- **Low battery optimization**: Giảm tần suất cập nhật GPS khi pin yếu
- **Offline map data**: Hỗ trợ tải xuống dữ liệu bản đồ để sử dụng offline
- **Background tracking**: Theo dõi vị trí ngay cả khi ứng dụng ở chế độ nền
- **Geofencing**: Thông báo khi tài xế đến gần điểm giao hàng

## Cấu hình dự án

### Prerequisites

- **Flutter SDK**: 3.32.8 
- **Dart**: 3.8.1
- **Android Studio**: Bản mới nhất
- **Gradle**: 8.14.3
- **Android SDK**: API 35 (Android 14)

### Bước 1: Cài đặt môi trường

#### Cài đặt Gradle
```bash
# Tải Gradle bản 8.14.3 từ https://gradle.org/releases/
# Giải nén file zip vào thư mục như C:\gradle\gradle-8.14.3
# Thêm vào Environment Variables > Path: C:\gradle\gradle-8.14.3\bin
# Kiểm tra cài đặt
gradle --version
```

#### Cài đặt Flutter SDK
```bash
# Tải Flutter 3.32.8 từ https://docs.flutter.dev/release/archive
# Giải nén và thêm vào Environment Variables > Path: 
# D:\flutter\bin
# C:\Users\<username>\AppData\Local\Pub\Cache\bin
# Kiểm tra cài đặt
flutter --version
```

#### Cài đặt Android Studio và SDK
- Cài Android Studio, cài extension cho Flutter
- Đổi thư mục máy ảo nếu hết dung lượng ổ cứng: 
  `File > Settings > Appearance & Behavior > System Settings > Android SDK > SDK Tools`
- Cài đặt cmdline-tools qua SDK Manager nếu bị thiếu
- Cấu hình Android SDK:
```bash
flutter config --android-sdk "D:\Android\Sdk"
flutter doctor --android-licenses
```

### Cấu hình Gradle Wrapper

Sau khi tải file `gradle-8.14.3-all.zip` về, cập nhật file `android/gradle/wrapper/gradle-wrapper.properties`:

```properties
distributionBase=GRADLE_USER_HOME
distributionPath=wrapper/dists
zipStoreBase=GRADLE_USER_HOME
zipStorePath=wrapper/dists
distributionUrl=file\:///D:/gradle-8.14.3-all.zip
```

### Bước 2: Setup và chạy project

#### Clone và cài đặt dependencies
```bash
cd PROJECT_KTC_2025/driver-app
flutter pub get
```

#### Cấu hình Firebase và API Keys
- Thêm `google-services.json` vào `android/app/` (có thể loại bỏ trong tương lai khi backend tích hợp đầy đủ)
- Cấu hình Firebase project cho FCM (tạm thời sử dụng cho notification, sẽ chuyển sang backend service)
- Đổi IP/backend: sửa file `lib/data/env/env.dart`
- Firebase Cloud Messaging: Đặt Server Key vào `lib/data/services/notification_service.dart` (tạm thời)
- MapBox: 
  - Đăng ký và lấy token từ https://account.mapbox.com/
  - Đặt token vào `lib/data/env/map_keys.dart`

#### Chạy ứng dụng

```bash
# Khởi động thiết bị ảo trong AVD hoặc kết nối thiết bị thật (USB debugging)
flutter run

# Hoặc build release APK
flutter build apk --release

# Cài đặt trực tiếp vào thiết bị
flutter install
```

### Bước 3: Build APK để cài đặt trên thiết bị

#### Build Debug APK (cho development)

```bash
# Build debug APK
flutter build apk --debug

# Hoặc sử dụng script tự động (nếu có lỗi về đường dẫn APK)
.\build_apk.ps1
```

#### Build Release APK (cho production)

```bash
# Clean project trước khi build
flutter clean
flutter pub get

# Build release APK
flutter build apk --release

# APK sẽ được tạo tại: build/app/outputs/flutter-apk/app-release.apk
```

#### Cài đặt APK trên thiết bị

```bash
# Via ADB (thiết bị kết nối USB)
adb install build/app/outputs/flutter-apk/app-debug.apk

# Hoặc copy file APK vào thiết bị và cài đặt thủ công
# Đảm bảo enable "Install from Unknown Sources" trên Android
```

### Bước 4: Tài khoản test để đăng nhập

Ứng dụng có sẵn các tài khoản test sau:

#### 🚛 **Tài xế 1 - Xe tải**

- **Email**: `driver@ktc.com`
- **Mật khẩu**: `123456`
- **Tên**: Nguyễn Văn An
- **Số điện thoại**: +84 901 234 567
- **Phương tiện**: Xe tải nhỏ Hyundai Porter (29A-12345)
- **Tải trọng**: 1.5 tấn

#### 🏍️ **Tài xế 2 - Xe máy**

- **Email**: `driver2@ktc.com`
- **Mật khẩu**: `123456`
- **Tên**: Trần Thị Lan
- **Số điện thoại**: +84 902 345 678
- **Phương tiện**: Xe máy Honda Lead (29B1-67890)
- **Tải trọng**: 30 kg

#### 📱 **Cách sử dụng tài khoản test**

1. Mở ứng dụng sau khi cài đặt
2. Chọn "Đăng nhập"
3. Nhập một trong hai tài khoản trên
4. Khám phá các tính năng: nhận đơn hàng, xem bản đồ, cập nhật trạng thái giao hàng

> **Lưu ý**: Hiện tại app đang sử dụng mock data, không cần kết nối backend server. Tất cả dữ liệu đơn hàng và thông tin tài xế đều được mô phỏng để demo tính năng.

#### Debug và Hot Reload

```bash
# Connect device qua USB
adb devices

# Run debug với hot reload
flutter run --debug

# Các lệnh hot reload
r    # Hot reload
R    # Hot restart
q    # Quit
```

---

## 🧪 Testing

### Run tests

```bash
# Unit tests
flutter test

# Integration tests  
flutter test integration_test/
```

### Debug trên device

```bash
# Connect device qua USB
adb devices

# Run debug
flutter run --debug

# Hot reload trong development
r    # Hot reload
R    # Hot restart
q    # Quit
```

---

## 🔧 Troubleshooting

### Common Issues

#### 1. Gradle build fails

```bash
cd android
./gradlew clean
cd ..
flutter clean
flutter pub get
```

#### 2. Permission issues

- Enable USB Debugging
- Allow "Install from Unknown Sources"
- Grant location permissions

#### 3. Firebase không hoạt động (tạm thời sử dụng)

- Kiểm tra `google-services.json`
- Verify Firebase project configuration
- Check FCM token generation

#### 4. Maps không hiển thị

```bash
# Kiểm tra MapBox token
flutter run -v # để xem lỗi chi tiết
```

- Kiểm tra token MapBox có hợp lệ không
- Verify quyền truy cập vị trí
- Đảm bảo thiết bị có kết nối internet

---

## 📄 License

Copyright © 2025 KTC Logistics. All rights reserved.

---

## Liên hệ & Hỗ trợ

Nếu gặp vấn đề khi cài đặt hoặc chạy, hãy mở issue trong repository để được hỗ trợ nhanh chóng.
