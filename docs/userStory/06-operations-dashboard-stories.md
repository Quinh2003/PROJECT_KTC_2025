# 📊 Operations Dashboard - User Stories

## ✅ US-OPS-KPI-01

**As an Operations Manager**,  
I want to see real-time KPIs,  
So that tôi có thể đánh giá hiệu suất vận hành.  
**Priority**: ⭐⭐⭐⭐⭐  
**Story Points**: 4

### 📋 Acceptance Criteria

- Tỷ lệ giao đúng, trễ, thời gian trung bình
- Thống kê theo thời gian thực
- Cảnh báo khi chỉ số vượt ngưỡng

---

## ✅ US-OPS-ANALYTICS-01

**As an Operations Manager**,  
I want to analyze route and driver performance,  
So that tôi có thể tối ưu hoạt động vận chuyển.  
**Priority**: ⭐⭐⭐⭐  
**Story Points**: 4

### 📋 Acceptance Criteria

- Báo cáo theo tuyến, tài xế, thời gian, biểu đồ
- So sánh hiệu suất giữa các tài xế
- Phân tích hiệu quả từng tuyến đường

---

## ✅ US-OPS-RESOURCE-01

**As an Operations Manager**,  
I want to manage resource allocation,  
So that tôi có thể cân đối nguồn lực và tối ưu chi phí.  
**Priority**: ⭐⭐⭐  
**Story Points**: 5

### 📋 Acceptance Criteria

- Phân bổ lại tài xế dựa trên nhu cầu
- Điều chỉnh kế hoạch theo mức độ ưu tiên
- Cân đối tải cho các tài xế và phương tiện

---

## ✅ US-OPS-IMPROVEMENT-01

**As an Operations Manager**,  
I want to identify improvement opportunities,  
So that tôi có thể nâng cao hiệu suất toàn hệ thống.  
**Priority**: ⭐⭐  
**Story Points**: 3

### 📋 Acceptance Criteria

- Phân tích điểm nghẽn trong quy trình
- Đề xuất cải tiến dựa trên dữ liệu
- So sánh trước và sau khi thực hiện thay đổi

## ✅ US-OPS-EXPORT-01

**As an operations manager**,  
I want to export reports and data to files,  
So that tôi có thể chia sẻ báo cáo với leadership và lưu trữ dữ liệu.  
**Priority**: ⭐⭐⭐  
**Story Points**: 3

### 📋 Acceptance Criteria

- **Xuất báo cáo KPI:**
  - Export dữ liệu dashboard thành file Excel (.xlsx)
  - Export biểu đồ thành hình ảnh (PNG, PDF)
  - Chọn khoảng thời gian xuất báo cáo (hôm nay, tuần, tháng, tùy chỉnh)

- **Xuất dữ liệu phân tích:**
  - Export danh sách đơn hàng thành CSV/Excel
  - Export hiệu suất tài xế thành file báo cáo
  - Export dữ liệu tuyến đường và thời gian giao hàng

- **Tùy chọn xuất file:**
  - Chọn định dạng file (Excel, CSV, PDF)
  - Chọn các cột dữ liệu cần xuất
  - Thêm logo công ty và thông tin header
  - Tự động đặt tên file theo thời gian xuất

- **Quản lý file đã xuất:**
  - Lịch sử các file đã xuất trong 30 ngày
  - Download lại file đã xuất trước đó
  - Xóa file cũ tự động sau 30 ngày

### 🎯 Yêu cầu kỹ thuật:

- Xử lý xuất file không làm block UI (background processing)
- Hiển thị progress bar khi xuất file lớn
- Validate dữ liệu trước khi xuất
- Giới hạn kích thước file tối đa (50MB)
- Hỗ trợ xuất file có nhiều sheet (Excel)
---
