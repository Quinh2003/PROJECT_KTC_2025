# 🚚 KTC LOGISTICS - CÔNG THỨC TÍNH PHÍ

## 📋 Tổng Quan Hệ Thống Tính Phí

Hệ thống KTC Logistics phân biệt hai loại phí vận chuyển:

1. **Shipping Fee**: Tính cho từng sản phẩm (OrderItem) dựa trên trọng lượng, thể tích và tính chất sản phẩm
2. **Delivery Fee**: Tính cho toàn bộ đơn hàng (Order) dựa trên khoảng cách và phí cơ bản

## 🧮 Công Thức Tính Phí Shipping (OrderItem)

### Công Thức Tổng Quát
```
SHIPPING FEE = PHÍ CƠ BẢN × HỆ SỐ RỦI RO × HỆ SỐ SERVICE_TYPE × SỐ LƯỢNG
```

### Các Thành Phần Của Công Thức

#### 1. Phí Cơ Bản
```
PHÍ CƠ BẢN = MAX(Phí theo trọng lượng, Phí theo thể tích quy đổi)
```

- **Trọng lượng quy đổi** = Thể tích (cm³) ÷ 5000
- **Trọng lượng tính phí** = MAX(Trọng lượng thực tế, Trọng lượng quy đổi)
- **Phí theo trọng lượng** = Trọng lượng tính phí × 10,000 VNĐ/kg

#### 2. Hệ Số Rủi Ro
- **Hàng thường**: 1.0
- **Hàng dễ vỡ (isFragile=true)**: 1.3

#### 3. Hệ Số Service Type

| Service Type  | Hệ Số | Mô Tả        |
|--------------|-------|--------------|
| SECOND_CLASS | 0.8   | Tiết kiệm    |
| STANDARD     | 1.0   | Tiêu chuẩn   |
| FIRST_CLASS  | 1.3   | Cao cấp      |
| EXPRESS      | 1.8   | Nhanh        |
| PRIORITY     | 2.0   | Ưu tiên      |

*Lưu ý: Đã loại bỏ SAME_DAY service type*

## 🚚 Công Thức Tính Phí Delivery (Order)

### Công Thức Tổng Quát
```
DELIVERY FEE = (PHÍ KHOẢNG CÁCH + PHÍ CƠ BẢN DELIVERY) × HỆ SỐ SERVICE_TYPE
```

### Các Thành Phần Của Công Thức

#### 1. Phí Khoảng Cách
```
PHÍ KHOẢNG CÁCH = Khoảng cách × Đơn giá/km + Phí cơ bản vùng
```

| Vùng               | Khoảng Cách | Phí Cơ Bản | Đơn Giá/km |
|-------------------|------------|------------|------------|
| Nội thành          | 0-15km     | 15,000 VNĐ | 1,800 VNĐ/km |
| Ngoại thành        | 15-50km    | 25,000 VNĐ | 1,500 VNĐ/km |
| Liên tỉnh          | >50km      | 40,000 VNĐ | 500 VNĐ/km   |

#### 2. Phí Cơ Bản Delivery
- Có thể là tổng shipping fee các OrderItem
- Hoặc một mức phí tối thiểu theo chính sách

#### 3. Hệ Số Service Type
- Sử dụng cùng bảng hệ số như Shipping Fee (bảng ở trên)

## 💡 Ví Dụ Tính Toán

### Ví Dụ 1: Shipping Fee (OrderItem)
- **Input**: 1.5kg, 11,250 cm³, fragile=true, EXPRESS, quantity=1
- **Tính toán**:
  - Trọng lượng quy đổi = 11,250 ÷ 5000 = 2.25kg
  - Trọng lượng tính phí = MAX(1.5, 2.25) = 2.25kg
  - Phí trọng lượng = 2.25 × 10,000 = 22,500 VNĐ
  - Phí cơ bản = 22,500 VNĐ
  - Hệ số rủi ro = 1.3 (hàng dễ vỡ)
  - Hệ số service = 1.8 (EXPRESS)
  - Số lượng = 1
- **Kết quả**: 22,500 × 1.3 × 1.8 × 1 = **52,650 VNĐ**

### Ví Dụ 2: Delivery Fee (Order)
- **Input**: Khoảng cách 12km, Tổng shipping fee = 100,000 VNĐ, Service Type = STANDARD
- **Tính toán**:
  - Phí cơ bản vùng (Nội thành) = 15,000 VNĐ
  - Phí theo km = 12 × 1,800 = 21,600 VNĐ
  - Tổng phí khoảng cách = 15,000 + 21,600 = 36,600 VNĐ
  - Phí cơ bản delivery = 100,000 VNĐ (tổng shipping fee)
  - Hệ số service = 1.0 (STANDARD)
- **Kết quả**: (36,600 + 100,000) × 1.0 = **136,600 VNĐ**

### Ví Dụ 3: Hàng thường, PRIORITY (Bảng Giá Cuối Cùng)
- **Input**: 0.5kg, 3,000 cm³, fragile=false, PRIORITY, quantity=1
- **Tính toán**:
  - Trọng lượng quy đổi = 3,000 ÷ 5000 = 0.6kg
  - Trọng lượng tính phí = MAX(0.5, 0.6) = 0.6kg
  - Phí trọng lượng = 0.6 × 10,000 = 6,000 VNĐ
  - Phí cơ bản = 6,000 VNĐ
  - Hệ số rủi ro = 1.0 (hàng thường)
  - Hệ số service = 2.0 (PRIORITY)
  - Số lượng = 1
- **Kết quả**: 6,000 × 1.0 × 2.0 × 1 = **12,000 VNĐ**

## 📊 Cách Tính Phí Cuối Cùng Cho Khách Hàng

Tổng phí vận chuyển mà khách hàng phải trả:
```
TỔNG PHÍ = TỔNG SHIPPING FEE (các OrderItem) + DELIVERY FEE (Order)
```

## 📍 Lưu Ý Quan Trọng

1. **Phí shipping** áp dụng riêng cho **từng sản phẩm** trong đơn hàng, không phụ thuộc khoảng cách.
2. **Phí delivery** áp dụng cho **toàn bộ đơn hàng**, có tính đến khoảng cách và phí cơ bản.
3. **Volume**: Sử dụng thể tích (cm³) thay vì length/width/height riêng lẻ khi tính phí shipping.
4. **Đơn vị**: Trọng lượng tính bằng kg, thể tích tính bằng cm³, khoảng cách tính bằng km.
5. **Service Type**: Quyết định thời gian giao hàng và ảnh hưởng đến cả shipping fee và delivery fee.

---

**📦 Các Entity Liên Quan**:
- `OrderItem` (trường `shippingFee`): Lưu phí shipping của từng sản phẩm
- `Delivery` (trường `deliveryFee`, `serviceType`): Lưu phí vận chuyển của đơn hàng
