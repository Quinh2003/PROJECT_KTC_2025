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
