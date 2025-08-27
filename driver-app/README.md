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

### 📸 Xác nhận giao hàng

- Chụp ảnh xác nhận giao hàng thành công
- Lấy signature khách hàng
- Upload ảnh lên server

### 🔔 Push Notifications

- Firebase Cloud Messaging (FCM)
- Thông báo đơn hàng mới
- Cập nhật trạng thái từ dispatcher

### 🔐 Authentication & Security

- Đăng nhập secure cho tài xế
- Flutter Secure Storage
- JWT token authentication
- Role-based access control

---

## 🛠️ Tech Stack

### Frontend

- **Flutter**: 3.32.8 (latest stable)
- **Dart**: 3.8.1
- **State Management**: BLoC Pattern
- **UI Components**: Material Design 3

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

### � Data Flow & Dependency Injection

**Flow**: UI → BLoC → Domain (Use Cases, Repository Interfaces) → Data (Repository Implementations) → External Sources

**Dependency Inversion**: Sử dụng GetIt service locator để đảm bảo Domain layer không phụ thuộc vào implementation details

**Benefits**:
- **Testability**: Mỗi layer có thể test independently
- **Flexibility**: Dễ thay đổi UI/data sources mà không ảnh hưởng business logic
- **Maintainability**: Code organization rõ ràng
- **Independence**: Business logic không phụ thuộc framework

---

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
