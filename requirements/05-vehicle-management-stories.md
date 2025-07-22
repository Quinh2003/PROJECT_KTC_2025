# 🚚 **Quản lý phương tiện**

---

## 📄 Page 1: Vehicle Fleet Overview - Tổng quan đội xe

### 🧩 US-VEHICLE-01: Vehicle Fleet Dashboard

**As a** logistics manager  
**I want** to view all vehicles in a dashboard  
**So that** I can monitor fleet status at a glance

**🎯 Priority:** High | **🧮 Story Points:** 5

#### ✅ Acceptance Criteria:

- Hiển thị danh sách tất cả xe với trạng thái
- Filter theo loại xe, trạng thái, khu vực
- Search xe theo biển số, tên tài xế
- Export danh sách xe ra Excel

### 🧩 US-VEHICLE-02: Vehicle Details View

**As a** logistics manager  
**I want** to view detailed information of each vehicle  
**So that** I can make informed decisions

**🎯 Priority:** Medium | **🧮 Story Points:** 3

#### ✅ Acceptance Criteria:

- Thông tin chi tiết xe (biển số, loại, tải trọng)
- Lịch sử hoạt động và bảo trì
- Tài xế được phân công
- Trạng thái hiện tại (sẵn sàng/bận/bảo trì)

---

## 📄 Page 2: Vehicle Assignment - Phân công phương tiện

### 🧩 US-VEHICLE-03: Assign Vehicle to Route

**As a** logistics coordinator  
**I want** to assign vehicles to delivery routes  
**So that** shipments can be delivered efficiently

**🎯 Priority:** High | **🧮 Story Points:** 8

#### ✅ Acceptance Criteria:

- Chọn xe phù hợp theo tải trọng và khu vực
- Assign tài xế cho xe
- Xem conflict scheduling
- Confirm assignment với notification

### 🧩 US-VEHICLE-04: Vehicle Availability Check

**As a** logistics coordinator  
**I want** to check vehicle availability  
**So that** I can plan routes effectively

**🎯 Priority:** Medium | **🧮 Story Points:** 3

#### ✅ Acceptance Criteria:

- Calendar view xe available
- Filter theo thời gian và khu vực
- Hiển thị xe đang bảo trì
- Quick booking cho xe available

---

## 📄 Page 3: Vehicle Maintenance - Bảo trì phương tiện

### 🧩 US-VEHICLE-05: Maintenance Schedule

**As a** fleet manager  
**I want** to schedule vehicle maintenance  
**So that** vehicles are always in good condition

**🎯 Priority:** High | **🧮 Story Points:** 5

#### ✅ Acceptance Criteria:

- Tạo lịch bảo trì định kỳ
- Alert khi đến hạn bảo trì
- Track chi phí bảo trì
- Update trạng thái sau bảo trì

### 🧩 US-VEHICLE-06: Maintenance History

**As a** fleet manager  
**I want** to track maintenance history  
**So that** I can analyze vehicle performance

**🎯 Priority:** Low | **🧮 Story Points:** 3

#### ✅ Acceptance Criteria:

- Xem lịch sử bảo trì của từng xe
- Report chi phí bảo trì theo tháng/quý
- So sánh performance giữa các xe
- Export maintenance reports
