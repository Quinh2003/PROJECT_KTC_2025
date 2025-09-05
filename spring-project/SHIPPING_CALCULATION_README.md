


# 🚚 HỆ THỐNG TÍNH PHÍ LOGISTICS - API DOCUMENTATION

## 1. Tổng Quan Kiến Trúc

Hệ thống phân biệt rõ ràng:

- **Shipping Fee (OrderItem):**
  - Tính cho từng OrderItem (sản phẩm trong đơn hàng).
  - KHÔNG sử dụng khoảng cách. Chỉ dựa vào trọng lượng, thể tích, loại dịch vụ, tính chất hàng hóa.
  - API: `/api/shipping/*`.

- **Delivery Fee (Order):**
  - Tính cho toàn bộ Order (đơn hàng), có thể sử dụng khoảng cách giữa điểm lấy và điểm giao.
  - API: `/api/delivery/*`.

## 2. Công Thức Tính Phí

## 2. Công Thức Tính Phí

### 2.1. Shipping Fee (OrderItem)

**TỔNG PHÍ = PHÍ CƠ BẢN × HỆ SỐ RỦI RO × HỆ SỐ SERVICE_TYPE × SỐ LƯỢNG**

- **Phí cơ bản:**
  - `PHÍ CƠ BẢN = MAX(Phí theo trọng lượng, Phí quy đổi thể tích)`
  - Trọng lượng tính phí = MAX(trọng lượng thực tế, trọng lượng quy đổi)
  - Trọng lượng quy đổi = Thể tích (cm³) ÷ 5000
  - Phí = Trọng lượng tính phí × 5,000 VNĐ/kg

- **Hệ số rủi ro:**
  - Hàng thường: 1.0
  - Hàng dễ vỡ: 1.3

- **Hệ số Service Type:**

  | Service Type   | Hệ số |
  |---------------|-------|
  | SECOND_CLASS  | 0.8   |
  | STANDARD      | 1.0   |
  | FIRST_CLASS   | 1.3   |
  | EXPRESS       | 1.8   |
  | PRIORITY      | 2.0   |

### 2.2. Delivery Fee (Delivery)

**TỔNG DELIVERY FEE = (PHÍ KHOẢNG CÁCH + PHÍ CƠ BẢN DELIVERY) × HỆ SỐ SERVICE_TYPE**

- **Phí khoảng cách:**
  - `Phí khoảng cách = Khoảng cách × Đơn giá/km + Phí cơ bản vùng`
  - Bảng giá:

    | Vùng         | Khoảng cách | Phí cơ bản | Đơn giá/km |
    |--------------|-------------|------------|------------|
    | Nội thành    | 0-15km      | 15,000     | 1,800      |
    | Ngoại thành  | 15-50km     | 25,000     | 1,500      |
    | Liên tỉnh    | >50km       | 40,000     | 500        |

- **Phí cơ bản delivery:**
  - Có thể là tổng shipping fee các OrderItem hoặc một mức phí tối thiểu theo chính sách.

- **Hệ số Service Type:**
  - Lấy từ trường `serviceType` của entity `Delivery` (giống bảng trên).

- **Công thức tổng quát:**
  - `Tổng Delivery Fee = (Phí khoảng cách + Phí cơ bản delivery) × Hệ số Service Type`

- **Kết quả trả về:**
  - Chi tiết breakdown (DTO: `DeliveryFeeBreakdown`) và tổng phí (`totalDeliveryFee`).

- **Lưu ý:**
  - Nếu chưa có record Delivery cho Order, cần tạo trước khi tính phí.
  - ServiceType mặc định là STANDARD nếu chưa có dữ liệu.
  - Khi lưu delivery fee, hệ thống sẽ cập nhật trường `deliveryFee` trong bảng `deliveries`.

## 3. API Chính

### Shipping Fee (OrderItem)

- `PUT /api/shipping/order-item/{orderItemId}/calculate`
- `GET /api/shipping/order-item/{orderItemId}/preview`
- `POST /api/shipping/calculate-fee`
- `POST /api/shipping/simple-calculate`

### Delivery Fee (Order)

- `GET /api/delivery/order/{orderId}/calculate`
- `PUT /api/delivery/order/{orderId}/calculate-and-save`

## 4. Ví Dụ API

### Tính phí shipping cho OrderItem

```http
POST /api/shipping/calculate-fee
Content-Type: application/json

{
  "actualWeight": 3.0,
  "length": 50,
  "width": 40,
  "height": 30,
  "isFragile": false,
  "serviceType": "STANDARD",
  "quantity": 1
}
```

### Tính phí delivery cho Order (có thể dùng khoảng cách)

```http
GET /api/delivery/order/{orderId}/calculate
```

## 5. Lưu Ý

- Shipping fee và delivery fee là 2 trường dữ liệu, 2 logic tính toán và 2 API khác nhau.
- Khi cần tổng hợp chi phí cuối cùng cho khách hàng, cần cộng cả shipping fee của từng item và delivery fee của order (nếu có).
- Các entity liên quan:
  - `OrderItem` (trường `shippingFee`)
  - `Delivery` (trường `deliveryFee`, `serviceType`, ...)

## 6. Test APIs

- `GET /api/test/shipping-example`: Test 3 trường hợp mẫu (không có khoảng cách)
- `GET /api/test/distance-example`: Test tính khoảng cách Hà Nội - TP.HCM (chỉ dùng cho delivery fee)
- `GET /api/test/custom-distance?fromLat=...&fromLon=...&toLat=...&toLon=...`: Test với tọa độ tùy chỉnh
