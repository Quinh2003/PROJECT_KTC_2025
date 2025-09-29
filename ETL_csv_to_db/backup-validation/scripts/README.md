# 🧰 Scripts - DataCo ETL Pipeline

## 📁 Cấu Trúc Scripts

### 📊 `pipelines/` - Pipeline Xử Lý Dữ Liệu
- **`enterprise_pipeline.py`** - Pipeline nâng cao với full features (khuyến khích)
- **`basic_pipeline.py`** - Pipeline cơ bản, đơn giản

### ✅ `validation/` - Scripts Validation  
- **`import_validator.py`** - Validation SQL import trước khi chạy

### 🚀 `deployment/` - Scripts Deployment
- **`production_deployer.py`** - Deploy production với safety checks

### 🛠️ `utilities/` - Tiện Ích Hỗ Trợ
- **`address_processor.py`** - Xử lý địa chỉ từ Nominatim API
- **`address_generator.py`** - Generate SQL từ CSV địa chỉ

### ⚙️ Cấu Hình
- **`config.py`** - Cấu hình chung cho tất cả scripts

## 🚀 Thứ Tự Thực Hiện

1. **Validation** (khuyến khích)
2. **Pipeline** (enterprise hoặc basic)  
3. **Deployment** (nếu cần deploy production)

## 📝 Lưu Ý

- Tất cả scripts đều hỗ trợ CLI arguments
- Logs được ghi vào `data_pipeline.log`
- Cấu hình database trong `.env` file

