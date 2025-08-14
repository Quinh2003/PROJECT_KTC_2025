# Driver Mobile App - FastRoute

FastRoute là ứng dụng di động dành cho tài xế giao hàng, phát triển bằng Flutter với giao diện Spatial UI hiện đại: bố cục đa tầng, hiệu ứng chuyển động, hiển thị trực quan bản đồ và thông tin theo ngữ cảnh. Ứng dụng tối ưu cho thao tác ngoài trời, giúp tài xế tập trung và xử lý đơn hàng nhanh chóng, an toàn.

## Tính năng nổi bật

- 🚗 Quản lý đơn hàng: nhận, điều hướng, cập nhật trạng thái
- 📍 Định vị GPS và tối ưu tuyến đường theo thời gian thực
- 🗺️ Bản đồ Google Maps/MapBox, POI và gợi ý lộ trình
- 📱 Thông báo đẩy (FCM) theo sự kiện đơn hàng
- 👤 Tài khoản, phân quyền, bảo mật khóa an toàn
- 💳 Tích hợp thanh toán, chứng từ giao nhận
- 📊 Báo cáo hiệu suất, nhật ký chuyến đi

---

## Chuẩn bị môi trường phát triển

- Cài đặt Gradle 8.14.3: tải từ Gradle Releases, giải nén vào `C:\gradle\gradle-8.14.3`, thêm `C:\gradle\gradle-8.14.3\bin` vào PATH. Kiểm tra bằng: `gradle --version`.
- Cài đặt Flutter 3.16.9 (Dart 3.2.6 – tương thích SDK ">=2.19.4 <3.3.0"): tải từ Flutter SDK archive, thêm `D:\flutter\bin` và `C:\Users\<user>\AppData\Local\Pub\Cache\bin` vào PATH.
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
