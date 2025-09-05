# KTC 2025 - Authentication & Security Overview

## 1. JWT + Stateless Sessions
- Sử dụng JSON Web Token (JWT) để xác thực người dùng.
- Stateless: Không lưu session trên server, mọi thông tin xác thực nằm trong token.
- Token chứa thông tin user, quyền hạn, thời gian hết hạn.
- Token được gửi qua header Authorization cho mỗi request.
- Giúp scale hệ thống dễ dàng, không phụ thuộc vào lưu trữ session.

## 2. Social OAuth (Google, Apple) với Firebase
- Đăng nhập bằng tài khoản Google, Apple qua OAuth 2.0.
- Firebase Authentication quản lý xác thực, tự động tạo user nếu chưa có.
- Đảm bảo bảo mật, giảm rủi ro lộ mật khẩu.

## 3. Tích hợp 2FA/MFA
- Hỗ trợ xác thực 2 lớp (2FA/MFA) để tăng bảo mật.
- TOTP (Google Authenticator): Sinh mã xác thực động, người dùng nhập mã từ app.
- Email OTP: Gửi mã xác thực qua email, xác nhận khi đăng nhập hoặc thực hiện hành động nhạy cảm.
- Đang lên kế hoạch bổ sung chức năng bật/tắt 2FA cho từng user.


## 4. Data Encryption
- Mã hóa dữ liệu nhạy cảm (mật khẩu, thông tin cá nhân) trước khi lưu vào database.
- Sử dụng thuật toán mạnh như bcrypt cho mật khẩu, AES cho dữ liệu khác.
- Đảm bảo dữ liệu không bị lộ ngay cả khi database bị truy cập trái phép.

## 5. Optimize SEO
- Sử dụng Next.js App Router để tối ưu SEO cho từng route.
- Tích hợp Open Graph, Twitter Card để hiển thị ảnh, mô tả khi chia sẻ link.
