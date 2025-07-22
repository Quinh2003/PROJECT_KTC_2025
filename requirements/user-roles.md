<!-- filepath: D:\PROJECT_KTC_2025\requirements\user-roles.md -->
# 📘 user-roles.md - Vai trò và Phân quyền Người dùng

## 🎯 Mục đích

Định nghĩa chi tiết vai trò và quyền hạn của từng loại người dùng trong hệ thống quản lý logistics.

---

## 🔖 Nội dung chính

- **Định nghĩa vai trò**: Mô tả rõ ràng từng vai trò trong hệ thống
- **Ma trận phân quyền**: Bảng chi tiết quyền hạn theo chức năng
- **Cấu trúc phân cấp**: Mối quan hệ giữa các vai trò
- **Quy tắc kiểm soát**: Nguyên tắc kiểm soát quyền truy cập
- **Quy trình đăng ký**: Các bước tạo tài khoản theo vai trò
- **Quy trình phân quyền**: Cách thức gán và thay đổi quyền hạn

---

## 🧩 Ma trận Phân quyền

| 📌 Chức năng                         | 👑 Quản trị | 🚦 Điều phối | 🚚 Tài xế | 👤 Khách hàng |
|-------------------------------------|:----------:|:------------:|:--------:|:-------------:|
| Quản lý người dùng                  |     ✓      |      ✗       |    ✗     |       ✗       |
| Tạo đơn hàng mới                    |     ✗      |      ✗       |    ✗     |       ✓       |
| Xem đơn hàng của mình               |     ✓      |      ✓       |    ✓     |       ✓       |
| Xem tất cả đơn hàng                 |     ✓      |      ✓       |    ✗     |       ✗       |
| Cập nhật trạng thái đơn hàng        |     ✓      |      ✓       |    ✓     |       ✗       |
| Xem bản đồ 3D                       |     ✓      |      ✓       |    ✓     |       ✗       |
| Sử dụng AI đề xuất lộ trình         |     ✓      |      ✓       |    ✗     |       ✗       |
| Xem báo cáo tổng hợp                |     ✓      |      ✗       |    ✗     |       ✗       |
| Theo dõi GPS tài xế                 |     ✓      |      ✓       |    ✗     |       ✗       |
| Quản lý phương tiện                 |     ✓      |      ✓       |    ✗     |       ✗       |

---

## 👥 Mô tả Vai trò

### 1. 👑 Quản trị viên (Administrator)

**Mô tả**: Người có toàn quyền quản lý hệ thống.

**Trách nhiệm**:

- Quản lý tài khoản người dùng và phân quyền
- Cấu hình các thông số hệ thống
- Xem tất cả báo cáo và thống kê hoạt động
- Quản lý dữ liệu gốc (kho bãi, xe cộ, tuyến đường)
- **Giám sát và quản lý toàn bộ đơn hàng trong hệ thống**

---

### 2. 🚦 Điều phối viên (Dispatcher)

**Mô tả**: Nhân viên điều phối vận chuyển hàng hóa.

**Trách nhiệm**:

- **Xử lý và quản lý đơn hàng từ khách hàng**
- Phân công tài xế và phương tiện vận chuyển
- Theo dõi tiến độ và trạng thái giao hàng
- Sử dụng AI để tối ưu hóa lộ trình vận chuyển

---

### 3. 🚚 Tài xế (Driver)

**Mô tả**: Người lái xe thực hiện việc giao hàng.

**Trách nhiệm**:

- Xem lộ trình và đơn hàng được phân công
- Cập nhật trạng thái tiến độ giao hàng
- Báo cáo sự cố hoặc tình huống chậm trễ
- Xác nhận hoàn thành việc giao hàng

---

### 4. 👤 Khách hàng (Customer)

**Mô tả**: Người sử dụng dịch vụ, nhận hàng hoặc đặt vận chuyển.

**Trách nhiệm**:

- **Tạo đơn hàng mới** (quyền độc quyền)
- Theo dõi trạng thái đơn hàng của mình
- Xem thời gian dự kiến nhận hàng
- Đánh giá chất lượng dịch vụ giao hàng
- Gửi phản hồi về trải nghiệm sử dụng

---

## 🔐 Cấu trúc Phân cấp & Quy tắc Kiểm soát

- **Phân cấp quyền lực**: `Quản trị viên > Điều phối viên > Tài xế > Khách hàng`
- **Nguyên tắc tối thiểu (Least Privilege)**: Người dùng chỉ được cấp quyền đủ để hoàn thành nhiệm vụ.
- **RBAC - Role-Based Access Control**: Phân quyền dựa trên vai trò, dễ kiểm soát và mở rộng.
- **Quy tắc đặc biệt**: Chỉ khách hàng được phép tạo đơn hàng mới để đảm bảo tính toàn vẹn quy trình nghiệp vụ.

---

## 📝 Quy trình Đăng ký Người dùng

1. Người dùng truy cập hệ thống và chọn loại tài khoản cần đăng ký.
2. Nhập thông tin cá nhân (Họ tên, Email, Mật khẩu,...).
3. Xác thực email (nếu có).
4. Tài khoản được gán vai trò mặc định (ví dụ: Khách hàng).
5. Quản trị viên có thể điều chỉnh vai trò nếu cần.

---

## 🔄 Quy trình Phân quyền & Cập nhật

- **Gán vai trò khi tạo tài khoản**: Tự động hoặc do quản trị viên xác định.
- **Cập nhật phân quyền**: Quản trị viên có thể nâng cấp, hạ cấp hoặc thu hồi quyền.
- **Theo dõi lịch sử phân quyền**: Ghi nhận các thay đổi để đảm bảo minh bạch và bảo mật.
