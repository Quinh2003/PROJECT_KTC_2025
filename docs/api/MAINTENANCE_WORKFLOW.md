# Vehicle Maintenance Management Workflow

## 🚗 Complete Flow Overview

### Step 1: Driver Creates Maintenance Request
**Who:** Driver  
**Action:** Gửi yêu cầu bảo trì xe  
**API:** `POST /api/drivers/{driverId}/maintenance-requests`

**Request Body:**
```json
{
  "vehicleId": 1,
  "description": "Lốp xe bị thủng, cần thay lốp mới",
  "maintenanceType": "repair",
  "statusId": 17
}
```

**Status:** Vehicle status = 17 (AVAILABLE - waiting for fleet approval)

---

### Step 2: Fleet Manager Views Requests
**Who:** Fleet Manager  
**Action:** Xem danh sách xe cần bảo trì + chi tiết từng xe

**APIs:**
- `GET /api/fleet/maintenance-requests` - Danh sách tất cả yêu cầu
- `GET /api/fleet/maintenance-requests/{maintenanceId}` - Chi tiết 1 yêu cầu

---

### Step 3: Fleet Manager Accepts & Schedules
**Who:** Fleet Manager  
**Action:** Chấp nhận yêu cầu, lên lịch bảo trì, chỉ định garage  
**API:** `PUT /api/fleet/maintenance-requests/{maintenanceId}/accept`

**Query Parameters:**
- `maintenanceDate`: "2025-09-15"
- `garageInfo`: "Garage ABC - 123 Nguyễn Văn Linh - SĐT: 0123456789"

**Updates:**
- `status_id` → 18 (IN_USE - approved for maintenance)
- `maintenance_date` → Ngày lên lịch bảo trì
- `notes` → Thông tin garage (do fleet manager thêm)

---

### Step 4: Driver Views Updated Status
**Who:** Driver  
**Action:** Kiểm tra trạng thái yêu cầu, thấy đã được chấp nhận + lịch bảo trì

**APIs:**
- `GET /api/drivers/{driverId}/maintenance-requests` - Danh sách yêu cầu của driver
- `GET /api/drivers/{driverId}/maintenance-requests/{maintenanceId}` - Chi tiết cụ thể

**Driver sees:**
- Status: IN_USE (đã chấp nhận)
- Maintenance Date: Ngày bảo trì
- Notes: Thông tin garage

---

### Step 5: Fleet Manager Updates Maintenance Status
**Who:** Fleet Manager  
**Action:** Cập nhật trạng thái bảo trì (đang bảo trì, hoàn thành, etc.)  
**API:** `PUT /api/fleet/maintenance-requests/{maintenanceId}/status`

**Query Parameters:**
- `statusId`: 19 (MAINTENANCE - đang bảo trì)
- `notes`: "Đang thay lốp, dự kiến hoàn thành 2 giờ"
- `cost`: 500000

**Status Changes:**
- 18 (IN_USE) → 19 (MAINTENANCE) → 17 (AVAILABLE)

---

### Step 6: Driver Picks Up Vehicle
**Who:** Driver  
**Action:** Nhận xe sau khi bảo trì xong  
**Status:** Vehicle status = 17 (AVAILABLE - sẵn sàng sử dụng)

---

## 📡 API Endpoints Summary

### Driver APIs
```
POST   /api/drivers/{driverId}/maintenance-requests              # Create request
GET    /api/drivers/{driverId}/maintenance-requests              # List driver requests  
GET    /api/drivers/{driverId}/maintenance-requests/{id}         # Request detail
```

### Fleet Management APIs
```
GET    /api/fleet/maintenance-requests                           # List all requests
GET    /api/fleet/maintenance-requests/{id}                      # Request detail
PUT    /api/fleet/maintenance-requests/{id}/accept               # Accept & schedule
PUT    /api/fleet/maintenance-requests/{id}/status               # Update status
```

### Utility APIs
```
GET    /api/maintenance-requests/summary                         # Dashboard summary
GET    /api/maintenance-requests/search?keyword={}               # Search requests
DELETE /api/maintenance-requests/{id}                            # Delete (admin only)
```

---

## 💾 Database Schema Usage

### Table: `vehicle_maintenance`
- `id` - Primary key
- `vehicle_id` - FK to vehicles table
- `created_by` - FK to users table (driver who created request)
- `status_id` - FK to status table (17=AVAILABLE, 18=IN_USE, 19=MAINTENANCE)
- `description` - Mô tả vấn đề (do driver nhập)
- `notes` - Thông tin garage (do fleet manager nhập)
- `maintenance_type` - Loại bảo trì (repair, routine, inspection, emergency)
- `maintenance_date` - Ngày lên lịch bảo trì
- `next_due_date` - Ngày bảo trì tiếp theo (optional)
- `cost` - Chi phí bảo trì
- `created_at`, `updated_at` - Timestamps

### Status Values (from status table)
- **17 - AVAILABLE**: Xe sẵn sàng hoặc yêu cầu chờ duyệt
- **18 - IN_USE**: Yêu cầu đã được chấp nhận, chuẩn bị bảo trì
- **19 - MAINTENANCE**: Xe đang được bảo trì

---

## 🎯 Benefits of This Design

1. **Minimal Backend Changes**: Sử dụng database hiện có, không cần tạo bảng mới
2. **Clear Separation**: Driver APIs vs Fleet APIs
3. **Flexible Status Tracking**: Sử dụng status table có sẵn
4. **Rich Information**: description (driver) + notes (fleet manager)
5. **Complete Workflow**: From request creation to completion
6. **Easy Integration**: Flutter app có thể dễ dàng integrate

---

## 🔄 Status Flow
```
[Driver Creates] → Status 17 (AVAILABLE)
[Fleet Accepts] → Status 18 (IN_USE) 
[Maintenance Starts] → Status 19 (MAINTENANCE)
[Maintenance Completes] → Status 17 (AVAILABLE)
```

This workflow ensures complete traceability and proper communication between drivers and fleet managers! 🚀