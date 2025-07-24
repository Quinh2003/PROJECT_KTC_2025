# 🗺️ 3D Map Visualization – User Stories

Tính năng bản đồ 3D giúp người dùng (như điều phối viên hoặc quản lý vận hành) dễ dàng theo dõi xe trên bản đồ theo thời gian thực. Dưới đây là các chức năng chính cần xây dựng.

---

## ✅ US-MAP-REALTIME-01 – Xem xe theo thời gian thực

**Người dùng**: Điều phối viên / Quản lý vận hành  
**Mục tiêu**: Thấy được vị trí và trạng thái xe trực tiếp trên bản đồ 3D  
**Ưu tiên**: ⭐⭐⭐⭐  
**Story Points**: 5

### 🎯 Yêu cầu chức năng:

- Hiển thị bản đồ 3D bằng **CesiumJS**
- Cập nhật vị trí xe tự động mỗi **30 giây**
- Màu xe thể hiện trạng thái:
  - 🟢 Xanh: Xe rảnh
  - 🟡 Vàng: Đang giao hàng
  - 🔴 Đỏ: Trễ hoặc gặp sự cố
  - ⚫ Xám: Offline
- Bấm vào xe để xem **thông tin chi tiết** (popup)
- Có thể **phóng to/thu nhỏ, xoay bản đồ** mượt mà
- Tối ưu để hiển thị được **ít nhất 50 xe cùng lúc** mà không bị lag

---

## ✅ US-MAP-DETAIL-02 – Xem chi tiết hành trình giao hàng

**Người dùng**: Tất cả người dùng liên quan đến vận hành  
**Mục tiêu**: Thấy rõ tuyến đường giao hàng và trạng thái chuyến  
**Ưu tiên**: ⭐⭐⭐  
**Story Points**: 4

### 🎯 Yêu cầu chức năng:

- Vẽ tuyến đường xe chạy với **đường cong tự nhiên**
- Hiển thị điểm **nhận/giao hàng** rõ ràng
- **Theo dõi chuyển động** của xe trên bản đồ
- Hiển thị **thời gian giao còn lại (ETA)**
- Tích hợp **thông tin giao thông** nếu có API
- Gợi ý tuyến đường khác nếu bị **kẹt xe**
- Cho phép **xem lại hành trình đã đi (lịch sử)**

---

## ✅ US-MAP-FILTER-03 – Lọc và tìm kiếm xe trên bản đồ

**Người dùng**: Điều phối viên / Quản lý vận hành  
**Mục tiêu**: Dễ dàng tìm kiếm và lọc xe theo các tiêu chí khác nhau  
**Ưu tiên**: ⭐⭐⭐  
**Story Points**: 3

### 🎯 Yêu cầu chức năng:

- **Filter theo trạng thái xe:**
  - Checkbox để ẩn/hiện xe theo trạng thái (Rảnh, Đang giao, Trễ, Offline)
  - Có thể chọn nhiều trạng thái cùng lúc
  - Cập nhật bản đồ ngay lập tức khi thay đổi filter

- **Tìm kiếm xe:**
  - Ô tìm kiếm theo biển số xe
  - Tìm kiếm theo tên tài xế
  - Tìm kiếm theo mã đơn hàng đang giao
  - Kết quả tìm kiếm highlight xe trên bản đồ
  - Tự động zoom đến xe được tìm thấy

- **Bộ lọc nâng cao:**
  - Lọc theo loại xe (nhỏ, vừa, lớn)
  - Lọc theo khu vực hoạt động
  - Lọc theo thời gian hoạt động (trong 1h, 2h, 1 ngày)
  - Reset filter về mặc định

### 📋 Acceptance Criteria:

- Hiển thị panel filter ở góc trái/phải bản đồ
- Filter hoạt động real-time, không reload trang
- Hiển thị số lượng xe được filter
- Lưu trạng thái filter khi chuyển trang
- Responsive trên mobile và desktop

---
