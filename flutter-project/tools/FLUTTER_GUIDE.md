# Hướng dẫn công cụ phát triển giao diện Flutter

## Các công cụ để xem và chỉnh sửa giao diện Flutter

### 1. Flutter DevTools (Tích hợp)

**DevTools** là công cụ chính thức của Flutter cho phép xem và chỉnh sửa giao diện trực tiếp khi đang chạy ứng dụng:

```bash
# Để bật DevTools từ terminal
flutter run -d chrome --web-renderer html
# Sau đó nhấn 'd' để mở DevTools trong trình duyệt
```

Hoặc từ VS Code:
1. Chạy ứng dụng bằng cách nhấn F5 hoặc chọn "Run and Debug"
2. Mở DevTools từ thanh trạng thái ở dưới cùng hoặc từ menu "View" > "Command Palette" > "Flutter: Open DevTools"
3. Chọn "Widget Inspector" để xem cấu trúc widget

### 2. Flutter Inspector trong VS Code

Cho phép xem cấu trúc cây widget và điều chỉnh các thuộc tính:
1. Cài đặt extension Flutter cho VS Code
2. Chạy ứng dụng
3. Mở Flutter Inspector từ sidebar hoặc Command Palette

### 3. Hot Reload & Hot Restart

- **Hot Reload (r)**: Cập nhật giao diện nhanh chóng mà không mất trạng thái hiện tại
- **Hot Restart (Shift + r)**: Khởi động lại ứng dụng với mã mới, phù hợp khi có thay đổi lớn

### 4. Flutter Layout Explorer

Giúp trực quan hóa các vấn đề về bố cục:
1. Mở DevTools
2. Chọn "Layout Explorer"
3. Xem các thông số như constraints, padding, margin

### 5. Sử dụng Widget Overlay

Công cụ này giúp hiển thị các đường viền để dễ dàng nhìn thấy vấn đề overflow:

```dart
import 'package:flutter/rendering.dart';

void main() {
  // Bật chế độ hiển thị đường viền cho các widget
  debugPaintSizeEnabled = true;
  runApp(MyApp());
}
```

### 6. Công cụ ngoài

#### a. Widgetbook (https://pub.dev/packages/widgetbook)

Tạo một "storybook" để xem các thành phần giao diện riêng biệt:

```bash
flutter pub add widgetbook
```

#### b. Flutter UI Challenges

Có thể sử dụng để xem các mẫu giao diện:

```bash
flutter pub add flutter_ui_challenges
```

#### c. Zeplin/Figma/Adobe XD integration

Cho phép so sánh trực quan giữa thiết kế và giao diện thực tế:
- Cài đặt Zeplin extension cho Flutter
- Hoặc sử dụng Figma to Flutter plugin

### 7. UI Debugging trong code

```dart
// Dễ dàng nhìn thấy các phần tử trong widget tree
Container(
  decoration: BoxDecoration(
    border: kDebugMode ? Border.all(color: Colors.red, width: 2) : null,
  ),
  child: yourWidget,
)
```

## Xử lý lỗi Overflow

### 1. Sử dụng Flexible và Expanded

```dart
Row(
  children: [
    // Widget cố định
    Icon(Icons.star),
    // Widget có thể co giãn
    Expanded(
      child: Text(
        'Văn bản dài có thể bị tràn',
        overflow: TextOverflow.ellipsis, // Hiển thị dấu ... khi bị tràn
      ),
    ),
  ],
)
```

### 2. LayoutBuilder

```dart
LayoutBuilder(
  builder: (context, constraints) {
    return Container(
      width: constraints.maxWidth, // Sử dụng chiều rộng có sẵn
      child: Text('Nội dung'),
    );
  },
)
```

### 3. Mediaquery

```dart
final screenWidth = MediaQuery.of(context).size.width;
Container(
  width: screenWidth * 0.8, // 80% chiều rộng màn hình
  child: Text('Nội dung'),
)
```

### 4. Thêm Padding và Margin hợp lý

```dart
Padding(
  padding: EdgeInsets.symmetric(horizontal: 16.0),
  child: yourWidget,
)
```

## Workflow đề xuất

1. Thiết kế UI trong Figma hoặc Adobe XD
2. Chuyển thành code Flutter với các thành phần tái sử dụng
3. Sử dụng DevTools để kiểm tra các vấn đề về bố cục
4. Áp dụng các kỹ thuật xử lý overflow khi cần
5. Kiểm tra trên nhiều kích thước màn hình khác nhau
6. Tối ưu hiệu suất render

## Tài nguyên

- [Flutter DevTools Guide](https://docs.flutter.dev/tools/devtools)
- [Flutter Layout Cheat Sheet](https://medium.com/flutter-community/flutter-layout-cheat-sheet-5363348d037e)
- [Flutter Cookbook: Design](https://docs.flutter.dev/cookbook/design)
