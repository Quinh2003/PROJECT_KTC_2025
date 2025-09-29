# 🚚 Kịch Bản Demo: Hệ Thống Quản Lý Vận Chuyển Logistics

## 📋 Tổng Quan

Kịch bản demo này trình bày một luồng hoàn chỉnh từ việc customer tạo đơn hàng, dispatcher phân công xe và tài xế, theo dõi hành trình thực tế, cho đến việc hoàn thành giao hàng và xuất hoá đơn thanh toán.

---

## 🎭 Các Vai Trò Trong Demo

### 👤 **Customer (Khách hàng)**
- Đăng ký/đăng nhập tài khoản
- Tạo đơn hàng mới
- Theo dõi trạng thái đơn hàng
- Xem lịch sử đơn hàng
- Tải hóa đơn thanh toán

### 📦 **Dispatcher (Điều phối viên)**
- Xử lý và quản lý đơn hàng
- Phân công xe và tài xế
- Theo dõi hành trình thực tế

### 🚛 **Driver (Tài xế)**
- Nhận và xác nhận đơn hàng
- Cập nhật trạng thái giao hàng
- Chụp ảnh bằng chứng giao hàng

### 🚘 **Fleet Manager (Quản lý đội xe)**
- Quản lý trạng thái và lịch bảo trì xe
- Xử lý sự cố xe đột xuất
- Điều phối xe thay thế
- Theo dõi chi phí vận hành

### 📊 **Operations Manager (Quản lý vận hành)**
- Giám sát hiệu suất hoạt động
- Quản lý KPIs và chất lượng dịch vụ
- Phân tích dữ liệu và đề xuất cải tiến

### 👨‍💼 **Admin (Quản trị viên)**
- Quản lý tài khoản người dùng
- Phân quyền và cấu hình hệ thống
- Xem báo cáo tổng hợp

---

## 🚀 **Phần 1: Customer Tạo Đơn Hàng**

### 📝 **Bước 1.1: Đăng ký/Đăng nhập**
- Đăng ký tài khoản mới (nếu chưa có)
- Đăng nhập vào hệ thống
- Truy cập trang tạo đơn hàng

### 🎯 **Bước 1.2: Tạo đơn hàng mới**
- Click nút "Tạo đơn hàng mới"
- Nhập thông tin đơn hàng:
  - Chọn loại dịch vụ vận chuyển
  - Địa chỉ lấy hàng
  - Địa chỉ giao hàng
  - Thông tin người nhận
  - Mô tả hàng hóa
  - Khối lượng và kích thước
  - Ghi chú đặc biệt (nếu có)

### 💰 **Bước 1.3: Xác nhận và thanh toán**
- Xem ước tính phí vận chuyển
- Xác nhận đơn hàng

---

## 📦 **Phần 2: Dispatcher Xử Lý Đơn Hàng**

### 📋 **Bước 2.1: Kiểm tra đơn hàng mới**
- Xem danh sách đơn hàng chờ xử lý
- Kiểm tra thông tin đơn hàng
- Xác nhận khả năng thực hiện

### 🚛 **Bước 2.2: Phân công xe và tài xế**
- Xem danh sách xe và tài xế có sẵn
- Phân công tài xế cho xe cụ thể
- Phân công đơn hàng cho tài xế
- Gửi thông báo cho tài xế

---

## 🚗 **Phần 3: Driver Thực Hiện Giao Hàng**

### 📱 **Bước 3.1: Driver nhận thông báo và xác nhận**
- Driver nhận notification trên app
- Xem chi tiết đơn hàng
- Xác nhận nhận đơn

### 📍 **Bước 3.2: Bắt đầu hành trình**
- Cập nhật trạng thái "Đang đến điểm lấy hàng"
- Tracking GPS realtime
- Hiển thị ước tính thời gian đến

### 📦 **Bước 3.3: Quy trình giao hàng**
- Xác nhận đã lấy hàng
- Di chuyển đến điểm giao hàng
- Cập nhật trạng thái realtime
- Chụp ảnh bằng chứng giao hàng
- Xác nhận hoàn thành

---

## 📊 **Phần 4: Quản Lý Vận Hành**

### 🚘 **Bước 4.1: Fleet Manager Quản Lý Đội Xe**

#### 4.1.1: Lên lịch bảo trì định kỳ
- Xem danh sách xe và lịch bảo trì
- Tạo lịch bảo trì mới:
  - Chọn xe cần bảo trì
  - Đặt lịch với garage
  - Thời gian bảo trì dự kiến
  - Các hạng mục bảo trì

#### 4.1.2: Xử lý xe hỏng khi đang giao
- Nhận thông báo sự cố từ tài xế
- Đánh giá mức độ sự cố
- Các phương án xử lý:
  1. **Sửa chữa tại chỗ**
     - Gửi đội kỹ thuật
     - Ước tính thời gian sửa
     - Thông báo khách hàng
  
  2. **Không thể sửa chữa tức thì**
     - Đưa xe đến garage để sửa chữa
     - Lập biên bản tình trạng xe
     - Theo dõi tiến độ sửa chữa

#### 4.1.3: Theo dõi và báo cáo
- Cập nhật trạng thái xe
- Ghi nhận chi phí sửa chữa
- Phân tích nguyên nhân
- Lên kế hoạch phòng ngừa

### 📊 **Bước 4.2: Operations Manager Giám Sát Hoạt Động**
- Theo dõi dashboard vận hành:
  - KPIs về thời gian giao hàng
  - Tỷ lệ giao hàng đúng hẹn
  - Hiệu suất sử dụng xe
  - Chi phí vận hành
- Phân tích và đề xuất cải tiến:
  - Báo cáo hiệu suất định kỳ
  - Đề xuất điều chỉnh quy trình
  - Kế hoạch nâng cao chất lượng

#### 4.2.1: Xuất hoá đơn thanh toán
- Tạo hóa đơn từ thông tin đơn hàng
- Gửi email hóa đơn cho khách hàng

#### 4.2.2: Báo cáo tổng hợp
- Thống kê số đơn hàng
- Phân tích hiệu suất giao hàng
- Báo cáo chi phí vận chuyển
- Đánh giá KPIs toàn hệ thống

---

## 🎬 **Demo Timeline & Duration**

```
⏱️ Estimated Demo Duration: 15-20 minutes

📅 Demo Breakdown:
- Part 1 (Customer tạo đơn): 3-4 mins
- Part 2 (Dispatcher xử lý): 3-4 mins
- Part 3 (Driver giao hàng): 4-5 mins
- Part 4 (Quản lý vận hành): 3-4 mins
- Part 5 (Báo cáo): 2-3 mins
- Q&A: 5 mins
```

---

### 🎯 **Demo Environment**

- **Backend URL**: http://localhost:8080
- **Frontend URL**: http://localhost:5173
- **Test Accounts**:
  - Customer: nguyentngoclien0704@gmail.com / 123456
  - Dispatcher: dispatcher@fr.com / dispatcher@1234
  - Driver: driver_01@fr.com / 123456
  - Fleet Manager: fleet@fr.com / fleet@1234
  - Operations Manager: operations@fr.com / operations@1234
  - Admin: admin@fr.com / admin@1234
- **Sample Orders**: ✅
- **Test Locations**: ✅ (HCMC area)
- **Payment Integration**: Test Mode ✅
- **Mapbox Integration**: ✅
- **Email Service**: Test Mode ✅

---

### 📝 **Các Điểm Nhấn Demo**

1. **Real-time Tracking**
   - Hiển thị vị trí xe realtime
   - Cập nhật trạng thái tức thì

2. **Quản Lý Vận Hành Hiệu Quả**
   - Dashboard trực quan
   - Phân tích dữ liệu realtime
   - Tối ưu hóa quy trình

3. **Tích Hợp Đa Nền Tảng**
   - Mapbox cho bản đồ
   - Email service
   - hoá đơn thanh toán

---
