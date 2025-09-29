import subprocess
import sys
import os

# Đường dẫn các file script
CLEAN_SCRIPT = "clean_line1.py"
DELIVERY_SCRIPT = "deliverytracking_checkpoint.py"

# Chạy file clean_line1.py
print("--- Chạy clean_line1.py ---")
result = subprocess.run([sys.executable, CLEAN_SCRIPT], capture_output=True, text=True)
print(result.stdout)
if result.returncode != 0:
    print("clean_line1.py lỗi, không chạy tiếp!")
    print(result.stderr)
    sys.exit(1)

# Kiểm tra lại file log đã được cắt đúng chưa
# (Kiểm tra file log lớn nhất trong process_log có dòng cuối cùng bắt đầu bằng '✅ Đã tạo sql_output3/')
LOG_DIR = "process_log"
logs = [f for f in os.listdir(LOG_DIR) if f.startswith("process_log_") and f.endswith(".txt")]
max_num = -1
log_file = None
for fname in logs:
    try:
        num = int(fname.replace("process_log_", "").replace(".txt", ""))
        if num > max_num:
            max_num = num
            log_file = fname
    except Exception:
        continue

if log_file is None:
    print("Không tìm thấy file log để kiểm tra!")
    sys.exit(1)

log_path = os.path.join(LOG_DIR, log_file)
with open(log_path, "r", encoding="utf-8") as f:
    lines = f.readlines()
    if not lines or not lines[-1].startswith("✅ Đã tạo sql_output3/"):
        print(f"File log {log_path} không kết thúc bằng dòng '✅ Đã tạo sql_output3/'. Không chạy tiếp!")
        sys.exit(1)

# Nếu kiểm tra OK, chạy tiếp deliverytracking_checkpoint.py
print("--- Chạy deliverytracking_checkpoint.py ---")
subprocess.run([sys.executable, DELIVERY_SCRIPT])
