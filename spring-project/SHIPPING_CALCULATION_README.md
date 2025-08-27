# 🚚 Hệ Thống Tính Phí Logistics - API Documentation

## 📋 Tổng Quan

Hệ thống tính phí logistics được xây dựng để tự động tính toán phí vận chuyển dựa trên:
- **Khoảng cách**: Tính từ tọa độ latitude/longitude của điểm đi và điểm đến
- **Trọng lượng & Thể tích**: Của sản phẩm trong OrderItem
- **Loại dịch vụ**: STANDARD, EXPRESS, SAME_DAY, etc.
- **Tính chất hàng hóa**: Hàng thường hay hàng dễ vỡ

## 🎯 Công Thức Tính Phí

**TỔNG PHÍ = PHÍ CƠ BẢN × HỆ SỐ RỦI RO × HỆ SỐ SERVICE_TYPE × SỐ LƯỢNG**

### 1. Phí Cơ Bản
```
PHÍ CƠ BẢN = MAX(Phí theo trọng lượng, Phí theo khoảng cách)
```

#### Phí theo trọng lượng:
- Trọng lượng tính phí = MAX(Trọng lượng thực tế, Trọng lượng quy đổi)
- Trọng lượng quy đổi = Volume ÷ 5000
- Phí = Trọng lượng tính phí × 5,000 VNĐ/kg

#### Phí theo khoảng cách (Công Thức Đơn Giản):

**Công thức:** Phí khoảng cách = Khoảng cách × Đơn giá/km + Phí cơ bản vùng

| Vùng | Khoảng cách | Phí cơ bản | Đơn giá/km |
|------|-------------|------------|------------|
| Nội thành | 0-15km | 15,000 VNĐ | 1,800 VNĐ/km |
| Ngoại thành | 15-50km | 25,000 VNĐ | 1,500 VNĐ/km |
| Liên tỉnh | >50km | 40,000 VNĐ | 500 VNĐ/km |

### 2. Hệ Số Rủi Ro
- Hàng thường: 1.0
- Hàng dễ vỡ: 1.3

### 3. Hệ Số Service Type
| Service Type | Hệ số | Mô tả |
|--------------|-------|-------|
| SECOND_CLASS | 0.8 | Tiết kiệm |
| STANDARD | 1.0 | Tiêu chuẩn |
| FIRST_CLASS | 1.3 | Cao cấp |
| EXPRESS | 1.8 | Nhanh |
| PRIORITY | 2.0 | Ưu tiên |

*Lưu ý: Đã loại bỏ SAME_DAY service type*

## 🔧 API Endpoints

### 1. Tính Khoảng Cách
```http
POST /api/shipping/calculate-distance
Content-Type: application/json

{
  "fromLatitude": 21.0285,
  "fromLongitude": 105.8542,
  "toLatitude": 10.8231,
  "toLongitude": 106.6297
}
```

### 2. Tính Phí Shipping
```http
POST /api/shipping/calculate-fee
Content-Type: application/json

{
  "actualWeight": 3.0,
  "length": 50,
  "width": 40,
  "height": 30,
  "distance": 20,
  "isFragile": false,
  "serviceType": "STANDARD",
  "quantity": 1
}
```

### 3. Tính Phí cho OrderItem
```http
PUT /api/shipping/order-item/{orderItemId}/calculate?serviceType=EXPRESS
```

### 4. Preview Phí cho OrderItem
```http
GET /api/shipping/order-item/{orderItemId}/preview?serviceType=STANDARD
```

### 5. Tính Phí Đơn Giản
```http
POST /api/shipping/simple-calculate
Content-Type: application/json

{
  "weight": 2.5,
  "volume": 6000,
  "fromLatitude": 21.0285,
  "fromLongitude": 105.8542,
  "toLatitude": 10.8231,
  "toLongitude": 106.6297,
  "isFragile": true,
  "serviceType": "EXPRESS",
  "quantity": 2
}
```

### 6. Lấy Danh Sách Service Types
```http
GET /api/shipping/service-types
```

## 🧪 Test APIs

### Test Ví Dụ Tính Phí
```http
GET /api/test/shipping-example
```

### Test Tính Khoảng Cách
```http
GET /api/test/distance-example
```

### Test Khoảng Cách Tùy Chỉnh
```http
GET /api/test/custom-distance?fromLat=21.0285&fromLon=105.8542&toLat=10.8231&toLon=106.6297
```

## 📊 Ví Dụ Tính Toán

### Trường hợp 1: Hàng thường, STANDARD (Bảng Giá Cuối Cùng)
- **Input**: 3kg, 60,000 cm³, 20km, không dễ vỡ
- **Tính toán**:
  - Trọng lượng quy đổi = 60,000 ÷ 5000 = 12kg
  - Phí trọng lượng = 12 × 5,000 = 60,000 VNĐ
  - Phí khoảng cách (Ngoại thành):
    - Phí cơ bản = 25,000 VNĐ
    - Phí theo km = 20 × 1,500 = 30,000 VNĐ
    - Tổng = 25,000 + 30,000 = 55,000 VNĐ
  - Phí cơ bản = MAX(60,000, 55,000) = 60,000 VNĐ
  - **Tổng phí = 60,000 × 1.0 × 1.0 = 60,000 VNĐ**

### Trường hợp 2: Hàng dễ vỡ, EXPRESS (Bảng Giá Cuối Cùng)
- **Input**: 1.5kg, 11,250 cm³, 12km, dễ vỡ
- **Tính toán**:
  - Trọng lượng quy đổi = 11,250 ÷ 5000 = 2.25kg
  - Phí trọng lượng = 2.25 × 5,000 = 11,250 VNĐ
  - Phí khoảng cách (Nội thành):
    - Phí cơ bản = 15,000 VNĐ
    - Phí theo km = 12 × 1,800 = 21,600 VNĐ
    - Tổng = 15,000 + 21,600 = 36,600 VNĐ
  - Phí cơ bản = MAX(11,250, 36,600) = 36,600 VNĐ
  - **Tổng phí = 36,600 × 1.3 × 1.8 = 85,644 VNĐ**

### Trường hợp 3: Hàng liên tỉnh, STANDARD (TP.HCM → Hà Nội)
- **Input**: 3.2kg, 20,000 cm³, 1,144km, không dễ vỡ
- **Tính toán**:
  - Trọng lượng quy đổi = 20,000 ÷ 5000 = 4kg
  - Phí trọng lượng = 4 × 5,000 = 20,000 VNĐ
  - Phí khoảng cách (Liên tỉnh):
    - Phí cơ bản = 40,000 VNĐ
    - Phí theo km = 1,144 × 500 = 572,000 VNĐ
    - Tổng = 40,000 + 572,000 = 612,000 VNĐ
  - Phí cơ bản = MAX(40,000, 612,000) = 612,000 VNĐ
  - **Tổng phí = 612,000 × 1.0 × 1.0 = 612,000 VNĐ**

## 🏗️ Kiến Trúc Hệ Thống

### Services
- **DistanceCalculationService**: Tính khoảng cách từ tọa độ
- **ShippingCalculationService**: Tính phí shipping theo công thức
- **OrderItemShippingService**: Tích hợp tính phí cho OrderItem
- **OrderItemService**: Lưu OrderItem với tự động tính phí

### DTOs
- **DistanceCalculationRequest**: Request tính khoảng cách
- **ShippingCalculationRequest**: Request tính phí shipping
- **ShippingFeeBreakdown**: Response chi tiết tính phí

### Controllers
- **ShippingCalculationController**: APIs chính cho tính phí
- **ShippingTestController**: APIs test và demo

## 🚀 Cách Sử Dụng

### 1. Tự động tính phí khi tạo OrderItem
```java
@Autowired
private OrderItemService orderItemService;

// Lưu với tính phí tự động
OrderItem orderItem = new OrderItem();
// ... set properties
OrderItem saved = orderItemService.saveWithShippingCalculation(orderItem, ServiceType.EXPRESS);
```

### 2. Tính phí thủ công cho OrderItem có sẵn
```java
@Autowired
private OrderItemShippingService shippingService;

ShippingFeeBreakdown breakdown = shippingService.calculateAndUpdateShippingFee(
    orderItem, ServiceType.SAME_DAY
);
```

### 3. Tính phí mà không lưu database
```java
ShippingFeeBreakdown breakdown = shippingService.calculateShippingFeeOnly(
    orderItem, ServiceType.STANDARD
);
```

## 🔧 Cấu Hình

### Database Requirements
- Store phải có `latitude` và `longitude`
- Address phải có `latitude` và `longitude` 
- Product phải có `weight`, `volume`, `is_fragile`
- OrderItem phải có `quantity`

### Dependencies
- Spring Boot
- JPA/Hibernate
- MySQL

## 📝 Lưu Ý

1. **Tọa độ**: Cần đảm bảo Store và Address có đầy đủ latitude/longitude
2. **Validation**: Hệ thống sẽ validate tất cả dữ liệu cần thiết
3. **Error Handling**: Lỗi sẽ được log nhưng không làm fail toàn bộ process
4. **Performance**: Sử dụng BigDecimal để đảm bảo độ chính xác tính toán
5. **Extensibility**: Dễ dàng thêm loại dịch vụ mới hoặc thay đổi công thức

## 🎯 Test Data

Có thể sử dụng các API test để kiểm tra:
- `GET /api/test/shipping-example`: Test 3 trường hợp mẫu
- `GET /api/test/distance-example`: Test tính khoảng cách Hà Nội - TP.HCM
- `GET /api/test/custom-distance`: Test với tọa độ tùy chỉnh
