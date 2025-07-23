# 🧑‍💼 Vai Trò Người Dùng – KTC Logistics 2025

Hệ thống được thiết kế với **5 vai trò chính**, mỗi vai trò có các chức năng riêng biệt nhằm tối ưu quy trình vận hành logistics nội bộ (1PL).

| 👤 **Vai trò**         | ⚙️ **Chức năng chính**                                                                                       |
|-----------------------|---------------------------------------------------------------------------------------------------------------|
| 🚦 **Dispatcher**      | - 📝 Tạo lệnh vận chuyển mới <br> - 👨‍✈️ Phân công tài xế và phương tiện phù hợp <br> - 🔄 Theo dõi trạng thái chuyến hàng <br> - ⚠️ Cập nhật thay đổi phát sinh nhanh chóng |
| 🚛 **Fleet Manager**   | - 🚗 Quản lý đội xe <br> - 🛠️ Cập nhật & theo dõi lịch bảo trì xe định kỳ <br> - 🔔 Cảnh báo phương tiện gần tới hạn kỹ thuật |
| 📱 **Driver (Tài xế)** | - 📥 Nhận lệnh mới từ mobile app <br> - 🗺️ Xem tuyến đường và trạng thái đơn <br> - 🔄 Cập nhật trạng thái vận chuyển <br> - 📸 Gửi ảnh hoá đơn, ✍️ ký nhận từ khách |
| 📊 **Operations Manager** | - 👁️ Giám sát toàn hệ thống <br> - 📉 Phân tích hiệu suất: thời gian, trễ, tài xế tốt, tuyến hiệu quả <br> - 📤 Xuất báo cáo & đề xuất cải tiến |
| 👑 **Admin**           | - 👤 Quản lý người dùng & phân quyền <br> - ⚙️ Cấu hình hệ thống, định kỳ, tích hợp API <br> - 📜 Theo dõi nhật ký hệ thống & quyền truy cập |

## 👑 Quản trị viên (Admin)

**Mô tả**: Người có toàn quyền cấu hình và giám sát hệ thống.

**Trách nhiệm**:

- Quản lý tài khoản người dùng và phân quyền
- Cấu hình các thông số hệ thống
- Tích hợp hệ thống bên ngoài qua API
- Xem tất cả báo cáo và thống kê hoạt động
- Theo dõi nhật ký hệ thống (logs) và lịch sử truy cập
- Quản lý dữ liệu gốc (kho, xe, tuyến đường)

---

## 🧭 Điều phối viên (Dispatcher)

**Mô tả**: Người lên kế hoạch và điều hành các chuyến giao hàng.

**Trách nhiệm**:

- Tạo và chỉnh sửa lệnh vận chuyển mới
- Phân công tài xế và phương tiện phù hợp
- Theo dõi trạng thái các chuyến hàng theo thời gian thực
- Cập nhật thay đổi nhanh chóng khi có phát sinh

---

## 🚚 Tài xế (Driver)

**Mô tả**: Người thực hiện vận chuyển và cập nhật trạng thái đơn hàng.

**Trách nhiệm**:

- Nhận thông báo và chi tiết lệnh mới từ ứng dụng mobile
- Xem tuyến đường, đơn hàng và khách cần giao
- Cập nhật trạng thái vận chuyển theo từng chặng
- Gửi ảnh hóa đơn, bằng chứng giao hàng và chữ ký nhận hàng

---

## 🛠️ Quản lý đội xe (Fleet Manager)

**Mô tả**: Người chịu trách nhiệm quản lý và bảo trì phương tiện.

**Trách nhiệm**:

- Quản lý thông tin và lịch sử bảo trì phương tiện
- Theo dõi và lên lịch kiểm tra định kỳ
- Nhận cảnh báo phương tiện gần tới hạn kiểm tra kỹ thuật
- Quản lý tài liệu đăng kiểm, bảo hiểm và hồ sơ xe

---

## 📊 Quản lý vận hành (Operations Manager)

**Mô tả**: Người giám sát toàn hệ thống, tối ưu hiệu suất vận hành.

**Trách nhiệm**:

- Giám sát tiến độ vận chuyển và chất lượng dịch vụ
- Phân tích hiệu suất vận hành (thời gian, trễ, tỷ lệ hoàn thành)
- Đánh giá hiệu quả tài xế và tuyến đường
- Xuất báo cáo theo định kỳ hoặc tùy chọn
- Đề xuất giải pháp cải tiến quy trình

---
