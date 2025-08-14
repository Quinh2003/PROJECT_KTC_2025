# 📱 KTC Logistics Driver Mobile App 2025

**FastRoute** là ứng dụng di động dành cho tài xế giao hàng, phát triển bằng Flutter với giao diện Spatial UI hiện đại: bố cục đa tầng, hiệu ứng chuyển động, hiển thị trực quan bản đồ và thông tin theo ngữ cảnh. Ứng dụng tối ưu cho thao tác ngoài trời, giúp tài xế tập trung và xử lý đơn hàng nhanh chóng, an toàn.

## 🚀 Tính năng chính

### 📦 **Quản lý đơn hàng**
- Nhận đơn hàng mới theo thời gian thực
- Xem chi tiết thông tin giao hàng
- Cập nhật trạng thái đơn hàng (đang giao, hoàn thành)
- Lịch sử các chuyến giao hàng

### 🗺️ **Navigation & Maps**
- Tích hợp Google Maps với GPS tracking
- Tối ưu tuyến đường thời gian thực
- Hiển thị vị trí pickup và delivery
- Polyline routing với flutter_polyline_points

### 📸 **Xác nhận giao hàng**
- Chụp ảnh xác nhận giao hàng thành công
- Lấy signature khách hàng
- Upload ảnh lên server

### � **Push Notifications**
- Firebase Cloud Messaging (FCM)
- Thông báo đơn hàng mới
- Cập nhật trạng thái từ dispatcher

### � **Authentication & Security**
- Đăng nhập secure cho tài xế
- Flutter Secure Storage
- JWT token authentication
- Role-based access control

---

## 🛠️ Tech Stack

### **Frontend**
- **Flutter**: 3.32.8 (latest stable)
- **Dart**: 3.8.1
- **State Management**: BLoC Pattern
- **UI Components**: Material Design 3

### **Backend Integration**
- **HTTP Client**: Dart HTTP package
- **Real-time**: Socket.IO client
- **Caching**: Flutter Secure Storage
- **Image Handling**: Image Picker

### **Maps & Location**
- **Google Maps**: Google Maps Flutter
- **GPS**: Geolocator package
- **Geocoding**: Geocoding package
- **Permissions**: Permission Handler

### **Android Configuration**
- **SDK**: 35 (Android 14)
- **Java**: 21 (LTS)
- **Gradle**: 8.9
- **Min SDK**: 24 (Android 7.0)

---

## 🏗️ Cấu trúc project

```
lib/
├── data/                    # Data layer
│   ├── env/                # Environment config
│   └── services/           # API services
├── domain/                 # Business logic
│   ├── bloc/              # BLoC state management
│   ├── models/            # Data models
│   └── services/          # Domain services
├── presentation/          # UI layer
│   ├── components/        # Reusable widgets
│   ├── screens/          # App screens
│   ├── themes/           # App themes
│   └── helpers/          # UI helpers
└── main.dart             # App entry point

Assets/                   # App assets
├── Logo/                # App logos
└── svg/                 # SVG icons
```

---

## ⚙️ Cài đặt và chạy project

### **Prerequisites**
```bash
# Kiểm tra Flutter
flutter --version
# Flutter 3.32.8 • Dart 3.8.1

# Kiểm tra Android SDK
flutter doctor
```

### **1. Clone và setup**
```bash
cd PROJECT_KTC_2025/driver-app
flutter pub get
```

### **2. Cấu hình Firebase**
- Thêm `google-services.json` vào `android/app/`
- Cấu hình Firebase project cho FCM

### **3. Build và chạy**
```bash
# Debug build
flutter run

# Release APK
flutter build apk --release

# Install trên device
flutter install
```

---

## 🧪 Testing

### **Run tests**
```bash
# Unit tests
flutter test

# Integration tests  
flutter test integration_test/
```

### **Debug trên device**
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

## 📦 Dependencies chính

```yaml
dependencies:
  # State Management
  bloc: ^9.0.0
  flutter_bloc: ^9.1.1
  
  # Firebase
  firebase_core: ^4.0.0
  firebase_messaging: ^16.0.0
  
  # Maps & Location
  google_maps_flutter: ^2.12.3
  geolocator: ^14.0.2
  geocoding: ^4.0.0
  flutter_polyline_points: ^3.0.1
  
  # UI & Utils
  google_fonts: ^6.3.0
  flutter_svg: ^2.2.0
  image_picker: ^1.1.2
  permission_handler: ^12.0.1
  
  # Storage & Network
  flutter_secure_storage: ^9.2.4
  http: ^1.5.0
  socket_io_client: ^3.1.2
```

---

## 🎯 Build Information

- **App Name**: KTC Logistics Driver
- **Package**: `com.ktc.logistics.ktc_logistics_driver`
- **Version**: 1.0.0+1
- **Build Size**: ~209MB (debug)
- **Target Platform**: Android ARM64

---

## 🔧 Troubleshooting

### **Common Issues**

**1. Gradle build fails**
```bash
cd android
./gradlew clean
cd ..
flutter clean
flutter pub get
```

**2. Permission issues**
- Enable USB Debugging
- Allow "Install from Unknown Sources"
- Grant location permissions

**3. Firebase không hoạt động**
- Kiểm tra `google-services.json`
- Verify Firebase project configuration
- Check FCM token generation

**4. Maps không hiển thị**
- Enable Google Maps API
- Add API key vào `AndroidManifest.xml`
- Check location permissions

---

## 👥 Development Team

- **Project**: KTC Logistics 2025
- **Platform**: Android Mobile App
- **Architecture**: Clean Architecture + BLoC
- **CI/CD**: Flutter Build Runner

---

## 📄 License

Copyright © 2025 KTC Logistics. All rights reserved.

---

## 🆕 Changelog

### v1.0.0 (2025-08-14)
- Initial release
- Complete migration from legacy restaurant app
- Updated to latest Flutter 3.32.8
- Implemented all logistics features
- Android SDK 35 support
- Modern dependency management

**Ready for production deployment! 🚀**
- Cài đặt Android Studio, Android SDK, và tạo máy ảo (AVD). Nếu thiếu cmdline-tools, cài qua SDK Manager. Có thể đổi vị trí lưu AVD nếu ổ đĩa đầy.

## Cấu hình dự án

```bash
cd .\mobile-app\
flutter clean
flutter pub get
flutter doctor
flutter config --android-sdk "D:\Android\Sdk"
flutter doctor --android-licenses
```

### Cấu hình Gradle Wrapper

Sau khi tải file `gradle-8.14.3-all.zip` về ổ D, cần cập nhật file `android/gradle/wrapper/gradle-wrapper.properties` để sử dụng file Gradle đã tải:

```properties
distributionBase=GRADLE_USER_HOME
distributionPath=wrapper/dists
zipStoreBase=GRADLE_USER_HOME
zipStorePath=wrapper/dists
distributionUrl=file\:///D:/gradle-8.14.3-all.zip
```

Điều này giúp tránh tải lại Gradle từ internet và sử dụng file đã có sẵn.

## Thiết lập dịch vụ & khóa API

- Đổi IP/backend: sửa file `Lib/Services/url.dart`.
- Google Maps: bật API trên Google Cloud Console, thêm API key vào `lib/Services/GoogleServices` và `Android/app/src/main/AndroidManifest.xml`.
- MapBox: tạo token và đặt vào `lib/Controller/MapBoxController`.
- Firebase Cloud Messaging: đặt Server Key vào `lib/Services/PushNotification.dart` và thêm `google-services.json` vào `Android/App/`.

## Chạy ứng dụng

- Mở máy ảo Android hoặc kết nối thiết bị thật (USB debugging)
- Chạy:
```bash
flutter run
```

## Công nghệ sử dụng

- Flutter + Dart
- Flutter Bloc (state management)
- Socket.io client (realtime)
- Google Maps / MapBox, Geolocator
- Firebase Cloud Messaging (push)

---

## Liên hệ & Hỗ trợ

Nếu gặp vấn đề khi cài đặt hoặc chạy, hãy mở issue trong repository để được hỗ trợ nhanh chóng.
