# Authentication Flow

Dự án sử dụng Clean Architecture với BLoC pattern để quản lý trạng thái đăng nhập/đăng xuất. File này mô tả chi tiết luồng đăng nhập trong ứng dụng KTC Logistics Driver.

## Luồng Đăng Nhập

1. **Người dùng nhập email/password trong `spatial_login_screen.dart`**
   - Giao diện đăng nhập được thiết kế theo Spatial UI
   - Có xác thực đầu vào đơn giản (email format, độ dài password)
   - Có chức năng "Remember Me" và "Forgot Password"

2. **Màn hình gọi `LoginEvent` từ `auth_event.dart`**
   - `LoginEvent` được tạo với thông tin đăng nhập
   - Event được dispatch đến `AuthBloc` thông qua `context.read<AuthBloc>().add()`

3. **`AuthBloc` xử lý sự kiện và gọi `LoginUseCase`**
   - `AuthBloc` chuyển trạng thái sang `AuthLoadingState`
   - `LoginUseCase` được gọi với các tham số đăng nhập

4. **`LoginUseCase` gọi phương thức `login` từ `AuthRepository`**
   - Use case đóng vai trò trung gian giữa UI và repository
   - Đảm bảo tính đóng gói và tuân thủ nguyên tắc phụ thuộc ngược

5. **`AuthRepositoryImpl` gọi `login` từ `MockAuthService` hoặc `auth_services.dart`**
   - Trong môi trường phát triển: Sử dụng `MockAuthService` với dữ liệu giả
   - Trong môi trường sản phẩm: Gọi API thực từ `auth_services.dart` 
   - API endpoint mặc định: `${env.endpointApi}/auth/login`

6. **Token được lưu trong `secure_storage` và `AuthBloc` cập nhật trạng thái**
   - Token JWT được lưu trữ an toàn trong `FlutterSecureStorage`
   - Thông tin người dùng được lưu trong bộ nhớ ứng dụng
   - `AuthBloc` cập nhật trạng thái thành `AuthenticatedState`

7. **UI phản ứng với trạng thái mới và chuyển đến màn hình Dashboard**
   - `BlocListener` trong `spatial_login_screen.dart` lắng nghe trạng thái
   - Khi nhận được `AuthenticatedState`, điều hướng đến `DashboardScreenSpatial`

## Tài khoản Test

Ứng dụng có sẵn 2 tài khoản test:

- **Backend thật:**
  - Email: `driver_01@fr.com` 
  - Password: `123456`

- **Offline mode:**
  - Email: `driver_offline@ktc.com` 
  - Password: `123456`

## Các file liên quan

### Presentation Layer
- `lib/presentation/screens/auth/*.dart`: Các màn hình đăng nhập, đăng ký, quên mật khẩu
- `lib/presentation/blocs/auth/*.dart`: BLoC quản lý trạng thái xác thực

### Domain Layer
- `lib/domain/usecases/usecases.dart`: Chứa LoginUseCase, CheckLoginStatusUseCase
- `lib/domain/models/auth/*.dart`: Models cho phản hồi xác thực
- `lib/domain/repositories/repository_interfaces.dart`: Định nghĩa AuthRepository

### Data Layer
- `lib/data/repositories/repository_implementations.dart`: Cài đặt AuthRepositoryImpl
- `lib/services/auth_services.dart`: Gọi API xác thực
- `lib/services/mock_auth_service.dart`: Mock service cho testing
- `lib/data/local_secure/secure_storage.dart`: Lưu trữ token JWT

## Diagram

```
┌─────────────────┐     ┌──────────────┐     ┌──────────────┐
│  Login Screen   │────>│  Auth Event  │────>│   Auth Bloc  │
└─────────────────┘     └──────────────┘     └───────┬──────┘
                                                     │
┌─────────────────┐                                  │
│     Storage     │<─────┐                           ▼
└─────────────────┘      │                ┌──────────────────────┐
                         │                │     Login UseCase    │
┌─────────────────┐      │                └──────────┬───────────┘
│    Dashboard    │      │                           │
└─────────────────┘      │                           ▼
        ▲                │                ┌──────────────────────┐
        │                └───--───────────┤   Auth Repository    │
        │                                 └──────────┬───────────┘
        │                                            │
        └────────────────────────────--──────────────┘
```
