# Kế hoạch kiểm thử ChecklistService (Refactor - Chuẩn hóa luồng 5 bước)


## ✅ Các thay đổi đã hoàn thành

### 1. Chuẩn hóa entity, repository, service checklist
- ChecklistStep có stepCode unique, bổ sung trường/annotation chuẩn
- ChecklistProgress có FK tới orders, users, checklist_step (step_id)
- Repository/service dùng quan hệ mới, truy vấn đúng chuẩn FK

### 2. Xóa controller/DTO checklist thừa
- Xóa DispatcherChecklistController, DriverChecklistController
- Xóa ChecklistDto, ChecklistItemDto, AssignDriverRequest, DispatcherOrderDTO

### 3. Chuẩn hóa luồng API và logic
- ChecklistController, DispatcherController, DriverController chỉ giữ logic cần thiết cho 5 bước nghiệp vụ
- API timeline checklist trả về đúng format cho FE

### 4. Logic checklist thống nhất
- Các bước checklist lấy từ bảng checklist_step (không hardcode)
- Tiến trình từng bước lưu trong checklist_progress, liên kết bằng step_id
- Trạng thái từng bước lấy từ business data (orders, payments, deliveries, activity logs)

## 🧪 Checklist kiểm thử


### Các API cần kiểm thử
1. `GET /api/checklist/orders/{orderId}/timeline` → getOrderTimeline(orderId)
2. `POST /api/checklist/mark-step-completed` → markStepCompleted()
3. `GET /api/checklist/steps/{role}` → getChecklistStepsByRole(role)


### Kết quả mong đợi
- **Trước đây**: Các bước checklist hardcode, logic lẫn lộn, khó mở rộng
- **Sau khi refactor**: Các bước checklist lấy từ database, logic thống nhất, dễ mở rộng, dễ kiểm thử


### Các trường hợp kiểm thử
1. **Khách hàng chưa có đơn hàng**: Tất cả các bước "Chưa hoàn thành", trạng thái đúng
2. **Khách hàng có đơn hàng**: Các bước phản ánh đúng dữ liệu thực tế
3. **Dispatcher**: Các bước lấy từ checklist_step, mapping đúng dữ liệu, thao tác xác nhận/chuyển bước
4. **Driver**: Các bước phản ánh trạng thái giao hàng, mapping đúng activity log
5. **Ghi nhận tiến trình**: markStepCompleted() cập nhật đúng bảng checklist_progress


## 🔍 Điểm cải tiến chính
- ✅ ChecklistStep là nguồn duy nhất cho định nghĩa bước
- ✅ ChecklistProgress lưu tiến trình từng bước, chuẩn FK
- ✅ Logic checklist nhất quán cho mọi vai trò
- ✅ API trả về đúng format cho FE, dễ render UI
- ✅ Loại bỏ hardcode, dễ mở rộng cho nghiệp vụ mới


## 🎯 Các bước kiểm thử tiếp theo
1. Khởi động ứng dụng Spring Boot
2. Gọi thử từng API checklist/timeline
3. Kiểm tra tích hợp với database, dữ liệu thực tế
4. Đảm bảo logic trạng thái, tiến trình checklist phản ánh đúng nghiệp vụ

# Quy trình ChecklistService - Chuẩn hóa logic & luồng 5 bước

## Danh sách file liên quan đến Checklist


### 1. Entity
- `entities/ChecklistStep.java`: Định nghĩa bước checklist
- `entities/ChecklistProgress.java`: Tiến trình hoàn thành checklist
- `entities/Order.java`, `entities/User.java`: Liên kết khóa ngoại


### 2. Repository
- `repositories/ChecklistStepRepository.java`: CRUD cho bước checklist
- `repositories/ChecklistProgressRepository.java`: Truy vấn tiến trình checklist


### 3. Service
- `services/ChecklistService.java`: Xử lý logic checklist, tiến trình, timeline


### 4. Controller
- `controllers/ChecklistController.java`: Các API checklist, timeline, đánh dấu hoàn thành
- `controllers/DispatcherController.java`, `controllers/DriverController.java`: Chỉ giữ logic cần thiết cho luồng 5 bước


### 5. DTO (Data Transfer Object)
- `dtos/ChecklistStepResponse.java`: DTO trả về thông tin bước checklist
- `dtos/ChecklistProgressResponse.java`: DTO tổng hợp tiến trình checklist
- `dtos/timeline/OrderTimelineResponse.java`: DTO timeline checklist đơn hàng
- `dtos/timeline/ActorDto.java`: Thông tin người thực hiện bước
- `dtos/timeline/OrderStatusDto.java`, `dtos/timeline/TimelineStepDto.java`: Trạng thái đơn hàng, chi tiết từng bước timeline


### 6. Cấu hình & test
- `resources/database-8-4.sql`: Cấu trúc bảng checklist_step, checklist_progress
- `test_checklist_service.md`: Kế hoạch kiểm thử checklist

---


## 7. Quy trình tổng quát
- Các bước checklist được định nghĩa động trong bảng checklist_step (không hardcode)
- Tiến trình từng bước lưu trong checklist_progress, liên kết bằng step_id (chuẩn khóa ngoại)
- Trạng thái từng bước lấy từ business data (orders, payments, deliveries, activity logs)
- DTO trả về cho FE đúng format, dễ dùng cho UI/UX


## 8. Luồng chạy Checklist (Flow - Chuẩn hóa)


### 1. FE gửi request API
- Ví dụ: Gọi `GET /api/checklist/orders/{orderId}/timeline` từ frontend


### 2. Controller nhận request
- File: `controllers/ChecklistController.java`
- Nhận request, xác thực, gọi hàm tương ứng trong service


### 3. Service xử lý logic
- File: `services/ChecklistService.java`
- Xử lý nghiệp vụ: lấy dữ liệu, kiểm tra trạng thái, mapping DTO
- Gọi các repository để truy vấn dữ liệu


### 4. Repository truy vấn DB
- File: `repositories/ChecklistProgressRepository.java`, `repositories/ChecklistStepRepository.java`
- Truy vấn bảng checklist_progress, checklist_step, order, user
- Trả về entity cho service


### 5. Entity mapping dữ liệu
- File: `entities/ChecklistProgress.java`, `entities/ChecklistStep.java`, ...
- Mapping dữ liệu từ DB sang object Java


### 6. Service mapping sang DTO
- File: `dtos/OrderTimelineResponse.java`, `dtos/timeline/TimelineStepDto.java`, ...
- Service chuyển entity sang DTO, format dữ liệu cho FE


### 7. Controller trả response cho FE
- Trả về JSON response đúng format, đầy đủ thông tin timeline, trạng thái, actor, ...


### 8. FE nhận dữ liệu, render UI
- FE dùng dữ liệu để hiển thị tiến trình checklist, trạng thái từng bước, thông tin người thực hiện, ...

---

#### Ví dụ luồng chạy cụ thể:
1. FE gọi API `/api/checklist/orders/65810/timeline`
2. ChecklistController nhận request, gọi `getOrderTimeline(orderId)` trong ChecklistService
3. ChecklistService truy vấn ChecklistProgressRepository, ChecklistStepRepository, OrderRepository
4. Repository trả về entity ChecklistProgress, ChecklistStep, Order
5. ChecklistService mapping sang OrderTimelineResponse, TimelineStepDto, ActorDto
6. Controller trả về response cho FE
7. FE nhận JSON, render UI timeline checklist