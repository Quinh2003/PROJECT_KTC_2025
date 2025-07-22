# Common UI Components - User Stories

## 🌟 Overview
Các components UI được sử dụng chung trên toàn bộ hệ thống logistics management.

---

## 🧭 Header Navigation Component

### US-HEADER-01: Main Navigation Header

**As a** logged-in user  
**I want** a consistent and responsive header navigation  
**So that** I can easily navigate between features based on my role

**🎯 Priority:** High | **🧮 Story Points:** 3

#### ✅ Tiêu chí chấp nhận:

- Logo công ty hiển thị ở góc trái header
- Avatar và tên người dùng hiển thị ở góc phải
- Menu điều hướng thay đổi theo vai trò:
  - **Admin:** Dashboard, Đơn hàng, Xe tải, Kho bãi, Người dùng, Báo cáo
  - **Tài xế:** Dashboard, Đơn giao hàng, Lộ trình, Hồ sơ
  - **Khách hàng:** Dashboard, Đơn hàng của tôi, Theo dõi, Hồ sơ
- Dropdown menu từ avatar: Hồ sơ, Cài đặt, Đăng xuất
- Đăng xuất có hộp thoại xác nhận
- Menu hamburger responsive trên mobile/tablet

### US-HEADER-02: Real-time Notifications

**As a** logged-in user  
**I want** real-time notifications in the header  
**So that** I can get immediate updates on orders and activities

**🎯 Priority:** Medium | **🧮 Story Points:** 5

#### ✅ Tiêu chí chấp nhận:

- Icon chuông với badge số thông báo chưa đọc
- Click chuông hiển thị dropdown 5 thông báo gần nhất
- Đánh dấu đã đọc khi click vào thông báo
- Link "Xem tất cả" đến trang thông báo
- Cập nhật real-time qua WebSocket
- Các loại thông báo khác nhau với icons riêng

---

## 🦶 Footer Component

### US-FOOTER-01: Site Footer Information

**As a** user accessing the system  
**I want** a footer with helpful information  
**So that** I can easily find help and contact information

**🎯 Priority:** Low | **🧮 Story Points:** 2

#### ✅ Tiêu chí chấp nhận:

- Thông tin công ty: logo, mô tả ngắn, liên kết mạng xã hội
- Liên kết nhanh: Về chúng tôi, Chính sách bảo mật, Điều khoản
- Hỗ trợ: Trung tâm trợ giúp, Liên hệ, FAQ
- Thông báo bản quyền với năm hiện tại
- Thông tin phiên bản hệ thống
- Giao diện responsive trên tất cả thiết bị
