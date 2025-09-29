# 💾 Data - DataCo ETL Pipeline

## 📁 Cấu Trúc Dữ Liệu

### 📥 `raw/` - Dữ Liệu Thô
- **`DataCo_UTF8.csv`** - Dataset chính (91MB, 180,519 records)
- **`DataCoSupplyChainDataset.csv`** - Dataset backup (91MB)  
- **`DescriptionDataCoSupplyChain.csv`** - Mô tả fields và columns

### 📤 `processed/` - Dữ Liệu Đã Xử Lý
*Thư mục này sẽ chứa output sau khi chạy pipeline*

## 📊 Thống Kê Dataset

### DataCo Supply Chain Dataset
- **Tổng records**: 180,519
- **Kích thước**: ~91MB
- **Format**: CSV (UTF-8)
- **Bảng đích**: 9 bảng database

### Mapping Summary
- **orders**: 65,752 records
- **order_items**: 180,519 records  
- **products**: 118 unique products
- **users**: 20,653 customers
- **addresses**: 65,752 addresses
- **payments**: 65,752 transactions
- **deliveries**: 65,752 shipments
- **categories**: 48 categories
- **stores**: 20 stores

## 🔧 Lưu Ý

1. **Encoding**: Sử dụng UTF-8 cho tiếng Việt
2. **Backup**: Luôn giữ raw data trong `raw/`
3. **Processing**: Output sẽ tạo trong `processed/`
4. **Size**: Files lớn, cần RAM đủ để xử lý

