# 🚚 Hệ Thống Tính Phí Shipping - KTC Project

## 📋 Tổng Quan

Hệ thống tính phí shipping dựa trên công thức được cung cấp trong file `🎯 Công Thức Tính Phí Logistics Hoàn Chỉnh.md`. 

### 🧮 Công Thức Tổng
```
TỔNG PHÍ LOGISTICS = PHÍ CƠ BẢN × HỆ SỐ RỦI RO × HỆ SỐ SERVICE_TYPE
```

## 🏗️ Cấu Trúc Hệ Thống

### 📁 Services
- **`DistanceCalculationService`**: Tính khoảng cách giữa 2 điểm dựa trên latitude/longitude
- **`ShippingCalculationService`**: Tính phí shipping dựa trên trọng lượng, thể tích, khoảng cách
- **`OrderItemShippingService`**: Tích hợp tính phí cho OrderItem

### 📄 DTOs
- **`DistanceCalculationRequest`**: Request tính khoảng cách
- **`ShippingCalculationRequest`**: Request tính phí shipping (sử dụng volume thay vì length/width/height)
- **`ShippingFeeBreakdown`**: Response chi tiết phí shipping

### 🔢 Service Types & Multipliers
| Service Type  | Hệ Số | Mô Tả        |
|--------------|-------|--------------|
| SECOND_CLASS | 0.8   | Tiết kiệm    |
| STANDARD     | 1.0   | Tiêu chuẩn   |
| FIRST_CLASS  | 1.3   | Cao cấp      |
| EXPRESS      | 1.8   | Nhanh        |
| PRIORITY     | 2.0   | Ưu tiên      |

*Lưu ý: Đã loại bỏ SAME_DAY service type*

## 🚀 API Endpoints

### 1. Tính Khoảng Cách
```
POST /api/shipping/calculate-distance
```

**Request Body:**
```json
{
  "fromLatitude": 21.0285,
  "fromLongitude": 105.8542,
  "toLatitude": 10.8231,
  "toLongitude": 106.6297
}
```

### 2. Tính Phí Shipping
```
POST /api/shipping/calculate-fee
```

**Request Body:**
```json
{
  "actualWeight": 3.0,
  "volume": 60000,
  "distance": 20.0,
  "isFragile": false,
  "serviceType": "STANDARD",
  "quantity": 1
}
```

### 3. Tính Phí Cho OrderItem
```
PUT /api/shipping/order-item/{orderItemId}/calculate?serviceType=STANDARD
```

### 4. Xem Trước Phí Shipping
```
GET /api/shipping/order-item/{orderItemId}/preview?serviceType=EXPRESS
```

### 5. Tính Phí Đơn Giản
```
POST /api/shipping/simple-calculate
```

**Request Body:**
```json
{
  "weight": 2.5,
  "volume": 30000,
  "fromLatitude": 21.0285,
  "fromLongitude": 105.8542,
  "toLatitude": 10.8231,
  "toLongitude": 106.6297,
  "isFragile": true,
  "serviceType": "EXPRESS",
  "quantity": 2
}
```

## 🧪 Test APIs

### Test Các Trường Hợp Mẫu
```
GET /api/test/shipping-example
```

Trả về 3 test cases:
1. Hàng thường, STANDARD - 3kg, 60,000 cm³, 20km
2. Hàng dễ vỡ, EXPRESS - 1.5kg, 11,250 cm³, 12km  
3. Hàng thường, PRIORITY - 0.5kg, 3,000 cm³, 8km

### Test Tính Khoảng Cách
```
GET /api/test/distance-example
GET /api/test/custom-distance?fromLat=21.0285&fromLon=105.8542&toLat=10.8231&toLon=106.6297
```

## 📊 Cách Tính Phí Chi Tiết

### 1. Phí Cơ Bản
**PHÍ CƠ BẢN = MAX(Phí theo trọng lượng, Phí theo khoảng cách)**

#### A. Phí Theo Trọng Lượng
- **Trọng lượng quy đổi = Volume ÷ 5000**
- **Trọng lượng tính phí = MAX(Trọng lượng thực tế, Trọng lượng quy đổi)**
- **Phí trọng lượng = Trọng lượng tính phí × 10,000 VNĐ/kg**

#### B. Phí Theo Khoảng Cách (Công Thức Đơn Giản)

**Công thức:** Phí khoảng cách = Khoảng cách × Đơn giá/km + Phí cơ bản vùng

| Vùng               | Phí Cơ Bản | Đơn Giá/km |
|-------------------|------------|------------|
| Nội thành (≤15km) | 15,000 VNĐ | 1,800 VNĐ/km |
| Ngoại thành (≤50km)| 25,000 VNĐ | 1,500 VNĐ/km |
| Liên tỉnh (>50km) | 40,000 VNĐ | 500 VNĐ/km |

### 2. Hệ Số Rủi Ro
- **Hàng thường**: 1.0
- **Hàng dễ vỡ**: 1.3

### 3. Hệ Số Service Type
Xem bảng Service Types ở trên.

## 💡 Ví Dụ Tính Toán

### Trường Hợp: Hàng dễ vỡ, EXPRESS (Bảng Giá Cuối Cùng)
- **Input**: 1.5kg, 11,250 cm³, 12km, fragile=true, EXPRESS
- **Tính toán**:
  - Trọng lượng quy đổi = 11,250 ÷ 5000 = 2.25kg
  - Trọng lượng tính phí = MAX(1.5, 2.25) = 2.25kg
  - Phí trọng lượng = 2.25 × 10,000 = 22,500 VNĐ
  - Phí khoảng cách (Nội thành):
    - Phí cơ bản = 15,000 VNĐ
    - Phí theo km = 12 × 1,800 = 21,600 VNĐ
    - Tổng phí khoảng cách = 15,000 + 21,600 = 36,600 VNĐ
  - Phí cơ bản = MAX(22,500, 36,600) = 36,600 VNĐ
  - Hệ số rủi ro = 1.3 (hàng dễ vỡ)
  - Hệ số service = 1.8 (EXPRESS)
- **Kết quả**: 36,600 × 1.3 × 1.8 = **85,644 VNĐ**

## 🔧 Cách Sử Dụng Trong Code

### Tính Phí Cho OrderItem
```java
@Autowired
private OrderItemShippingService orderItemShippingService;

// Tính và cập nhật shipping fee
ShippingFeeBreakdown breakdown = orderItemShippingService
    .calculateAndUpdateShippingFee(orderItem, ServiceType.EXPRESS);

// Chỉ tính toán mà không cập nhật
ShippingFeeBreakdown preview = orderItemShippingService
    .calculateShippingFeeOnly(orderItem, ServiceType.STANDARD);
```

### Tính Phí Trực Tiếp
```java
@Autowired
private ShippingCalculationService shippingService;

ShippingCalculationRequest request = new ShippingCalculationRequest();
request.setActualWeight(new BigDecimal("2.5"));
request.setVolume(new BigDecimal("30000"));
request.setDistance(new BigDecimal("25"));
request.setFragile(true);
request.setServiceType(ServiceType.EXPRESS);
request.setQuantity(1);

ShippingFeeBreakdown result = shippingService.calculateShippingFee(request);
```

## 📍 Lưu Ý Quan Trọng

1. **Volume**: Sử dụng thể tích (cm³) thay vì length/width/height riêng lẻ
2. **Coordinates**: Store entity có latitude/longitude trực tiếp, không thông qua Address
3. **Service Types**: Đã loại bỏ SAME_DAY, chỉ còn 5 loại dịch vụ
4. **Error Handling**: Tất cả API đều có xử lý lỗi và trả về response thống nhất
5. **Database**: OrderItem.shippingFee sẽ được tự động cập nhật khi tính toán

## 🏃‍♂️ Chạy Test

1. Start Spring Boot application
2. Truy cập: `http://localhost:8080/api/test/shipping-example`
3. Xem kết quả tính toán cho các trường hợp mẫu
4. Test các API khác theo nhu cầu

---

**📞 Liên hệ**: Phan Quỳnh - KTC Project Team
